React = require 'react'
createReactClass = require 'create-react-class'
_l = require 'lodash'
{ inspect } = require 'util'
ordered_map = _l.map

{js_to_lispy} = require './js_to_lisp'
babylon = require 'babylon'

E = require './lispy'
GV = require './graph-vis'

sample_js = """

var x = 4;
var y = 6;

function myfunc(x, y) {
    return x * y + y * x;
}

var z = x + y;
var a = x * y;
var b = z + a;
var c = myfunc(a, z);
var d = "Hello " + c + "!";

"""

WithWindowSizeComponent = ({children}) ->
    ws = GV.useWindowSize()
    return children(ws)

withWindowSize = (fn) ->
    <WithWindowSizeComponent children={fn} />

useMemoBeforeError = (deps, creator) ->
    [saved_deps, set_saved_deps] = React.useState(null)
    [memoed_results, set_memoed_results] = React.useState(null)
    [active_error, set_active_error] = React.useState(null)

    if not _l.isEqual(deps, saved_deps)
        set_saved_deps(deps)

        try
            result = creator()
        catch error
            # pass

        if error?
            set_active_error(error)
            error = error
        else
            set_active_error(null)
            active_error = null
            set_memoed_results(result)
            memoed_results = result

    return [active_error, memoed_results]

export JSDFG = ->
    [source_code, set_source_code] = React.useState(sample_js)

    [error, {lispy_ast, rr, history, evaled, root_scope}] = useMemoBeforeError [source_code], ->
        # only run the program once, so we have one rr we can keep poking around with equal pointers
        # across time
        lispy_ast = js_to_lispy(source_code)
        rr = E.lispy_eval(E.fresh_root_scope(), lispy_ast)
        history = E.recursive_records_in_eval_order(rr)

        evaled = rr.value
        root_scope = rr.scope.vars

        x = {lispy_ast, rr, history, evaled, root_scope}
        _l.extend(window, x)
        return x


    record_is_method_call = (record) -> record.args?[0].value == root_scope['js/.()']
    color = (choice) -> (children) -> <span style={color: choice} children={children} />

    precise_scope_for_var = (relevant_scope, varname) ->
        return relevant_scope if varname of relevant_scope.vars
        return precise_scope_for_var(relevant_scope.parent, varname) unless relevant_scope.parent == null
        # sad; probably going to crash
        return null

    who_set_var = (record) =>
        scope = record.scope
        varname = record.expr[1]
        record_t = _l.findIndex(history, record)

        # find the nearest prior (set)
        for t in [0..record_t - 1]
            r = history[t]
            return r if r.expr?[0] == 'set' and r.scope == scope and r.expr[1] == varname

        # never been set— should be a function argument of a closing scope
        # find the relevant function call for the relevant scope.  This is soooo gross.
        precise_scope = precise_scope_for_var(scope, varname)
        creating_call = _l.find history, (r) -> r.expr?[0] == 'call' and r.body?.scope == precise_scope

        if creating_call?
            argnames = creating_call.args[0].value[cl]?[1][1]
            arg_position = _l.indexOf(argnames, varname)
            return creating_call.args[arg_position + 1]

        # we failed
        return null

    # skip_sets :: Record -> {record: Record}
    skip_sets = (record) =>
        if record.expr?[0] == 'set' then skip_sets(record.body)
        else if record.expr?[0] == 'var' then skip_sets(who_set_var(record))
        else if record.expr?[0] == 'call' and record.body? then skip_sets(record.body)
        # else if (record.callees?.length ? 0) > 0 then record.callees.map (cle) -> cle.body
        else {record}

    deps_for = (record) ->
        if record.args? then record.args?.slice(1)
        else []

    withWindowSize (window_size) =>
        code_editor_width = 600
        code_editor_padding = 20

        <React.Fragment>
            <GV.GraphVisual
                style={
                    backgroundColor: 'rgb(230, 230, 230)'
                }
                favored_viewport_area={[[550, 0], [window_size.width, window_size.height]]}
                width={window_size.width}
                height={window_size.height}
                root_nodes={rr.args.slice(1).map(skip_sets).filter ({record}) -> not E.is_lambda(record.value)}
                outedges={({record}) -> deps_for(record).map(skip_sets)}
                onClickNode={({record}) ->
                    # just stash this on the console, so you can debug your way out
                    window.r = record
                }
                keyByObject={({record}) -> record}
                renderNode={({record}, is_hovered) ->
                    <div style={
                        backgroundColor: unless is_hovered then 'rgb(255, 248, 221)' else 'rgb(169, 226, 255)'
                        border: unless is_hovered then '2px solid rgb(160, 159, 94)' else '2px solid rgb(129, 146, 185)'
                        width: 2 * GV.node_radius, height: 2 * GV.node_radius, borderRadius: '100%'
                        pointerEvents: 'none'

                        display: 'flex', flexDirection: 'column', position: 'relative'
                        textAlign: 'center', alignItems: 'center', justifyContent: 'center'

                        color: 'black', fontFamily: 'sans-serif', fontSize: 16, fontWeight: 'light'
                    }>
                        {
                            if record.expr?[0] == 'call'
                                <React.Fragment>
                                    <div style={
                                        position: 'absolute', top: -20
                                    }>
                                        <span style={color: 'black'}>
                                            { E.ppexpr record.args[0].expr }
                                        </span>
                                    </div>
                                    <div>
                                        <span style={color: 'blue'}
                                            children={ E.inspect_value(record.value) } />
                                    </div>
                                </React.Fragment>

                            else if record.expr?[0] == 'lit'
                                <span style={color: 'black'}>
                                    { E.inspect_value(record.value) }
                                </span>

                            else if record.expr?[0] == 'var'
                                <span style={color: 'brown'}>
                                    { E.inspect_value(record.value) }
                                </span>

                            else if record.expr?[0] == 'lambda'
                                <span style={color: 'brown'}>
                                    { "λ" }
                                </span>
                        }
                    </div>
                }
            />
            <div style={
                position: 'fixed', left: 20, top: 20, height: 600, width: code_editor_width
                perspective: '1000px'
                pointerEvents: 'none'
            }>
                <textarea
                    style={{
                        height: 600 - 2*20 - 2*20, width: code_editor_width
                        transform: 'rotateY(28deg)', transformOrigin: 'left'
                        padding: code_editor_padding, backgroundColor: '#333', borderRadius: 10
                        color: 'white', fontFamily: 'monaco', fontSize: 14
                        pointerEvents: 'all'
                    }}
                    value={source_code}
                    onChange={(evt) -> set_source_code(evt.target.value)}
                />
            </div>
            { if error?
                <div style={position: 'fixed', bottom: 30, right: 30}>
                    { "#{error}" }
                </div>
            }
        </React.Fragment>
