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

export JSDFG = ->
    [source_code, set_source_code] = React.useState(sample_js)
    [selected_labeled_node, set_selected_labeled_node] = React.useState(null)

    [error, {lispy_ast, rr, history, evaled, root_scope}] = GV.useMemoBeforeError [source_code], ->
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

    # labeled_record :: Record -> LabeledRecord
    labeled_record = (record) -> {labels: [], record: record}
    # add_label :: LabeledRecord -> String -> LabeledRecord
    add_label = (label, lr) -> {labels: [label].concat(lr.labels), record: lr.record}

    # skip_sets :: Record -> LabeledRecord
    skip_sets = (record) =>
        if record.expr?[0] == 'set' then add_label record.expr[1], skip_sets(record.body)
        else if record.expr?[0] == 'var' then add_label record.expr[1], skip_sets(who_set_var(record))
        else if record.expr?[0] == 'call'
            add_label E.ppexpr(record.args[0].expr), (
                if record.body? then skip_sets(record.body)
                else labeled_record(record)
            )
        # else if (record.callees?.length ? 0) > 0 then record.callees.map (cle) -> cle.body
        else labeled_record(record)

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
                inedges={({record}) -> deps_for(record).map(skip_sets)}
                onClickNode={(lr) ->
                    set_selected_labeled_node(lr)
                    # just stash this on the console, so you can debug your way out
                    window.lr = lr
                }
                keyByObject={({record}) -> record}
                renderNode={({record, labels}, is_hovered) ->
                    color = switch record.expr?[0]
                        when 'call' then 'blue'
                        when 'lit' then 'black'
                        when 'lambda' then 'brown'
                        # following should never happen, but are included for safety
                        when 'var' then 'brown'
                        when 'set' then 'brown'
                        else 'brown'

                    ppvalue =
                        if record.expr?[0] == 'lambda' then "λ"
                        else E.inspect_value(record.value)

                    <div style={
                        backgroundColor: unless is_hovered then 'rgb(255, 248, 221)' else 'rgb(169, 226, 255)'
                        border: unless is_hovered then '2px solid rgb(160, 159, 94)' else '2px solid rgb(129, 146, 185)'
                        width: 2 * GV.node_radius, height: 2 * GV.node_radius, borderRadius: '100%'
                        pointerEvents: 'none'

                        display: 'flex', flexDirection: 'column', position: 'relative'
                        textAlign: 'center', alignItems: 'center', justifyContent: 'center'

                        color: 'black', fontFamily: 'sans-serif', fontSize: 16, fontWeight: 'light'
                    }>
                        <div style={bottom: '100%', position: 'absolute'}>
                            { labels.map (label, i) ->
                                <div style={color: 'black'} key={i}>
                                    { label }
                                </div>
                            }
                        </div>
                        <span style={{color}}>
                            { ppvalue }
                        </span>
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
            <div style={position: 'fixed', bottom: 30, right: 30}>
                { if error?
                    "#{error}"
                }
                { if (selected_source_range = selected_labeled_node?.record.expr?.source_range)?
                    source_code.slice(selected_source_range[0], selected_source_range[1])
                }
            </div>
        </React.Fragment>
