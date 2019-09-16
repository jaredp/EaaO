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
var z = x + y;
var a = x * y;
var b = z + a;

"""

WithWindowSizeComponent = ({children}) ->
    ws = GV.useWindowSize()
    return children(ws)

withWindowSize = (fn) ->
    <WithWindowSizeComponent children={fn} />

export class JSDFG
    init: (@react_root) ->
        # set up console shortcuts
        Object.defineProperties window, _l.mapValues(@console_shortcuts, (v) -> {get: v})

        # only run the program once, so we have one rr we can keep poking around with equal pointers
        # across time
        @lispy_ast = js_to_lispy(sample_js)
        @rr = E.lispy_eval(E.fresh_root_scope(), @lispy_ast)
        @history = E.recursive_records_in_eval_order(@rr)

        @evaled = @rr.value
        @root_scope = @rr.scope.vars


    did_mount: ->
    render: ->
        record_is_method_call = (record) -> record.args?[0].value == @root_scope['js/.()']
        color = (choice) -> (children) -> <span style={color: choice} children={children} />

        who_set_var = (record) =>
            scope = record.scope
            varname = record.expr[1]
            record_t = _l.findIndex(@history, record)
            for t in [0..record_t - 1]
                r = @history[t]
                return r if r.expr?[0] == 'set' and r.scope == scope and r.expr[1] == varname
            return null

        skip_sets = (record) =>
            if record.expr?[0] == 'set' then skip_sets(record.body)
            else if record.expr?[0] == 'var' then skip_sets(who_set_var(record))
            else record

        deps_for = (record) ->
            if record.args? then record.args?.slice(1)
            else []

        withWindowSize (window_size) =>
            <GV.GraphVisual
                style={
                    backgroundColor: 'rgb(230, 230, 230)'
                    marginTop: 20, marginLeft: 20
                    borderRadius: 10
                }
                width={window_size.width - 40} height={window_size.height - 40}
                root_nodes={@rr.args.slice(1).map(skip_sets)}
                outedges={(record) -> deps_for(record).map(skip_sets)}
                renderNode={(record, is_hovered) ->
                    <div style={
                        backgroundColor: unless is_hovered then 'rgb(255, 248, 221)' else 'rgb(169, 226, 255)'
                        border: unless is_hovered then '2px solid rgb(160, 159, 94)' else '2px solid rgb(129, 146, 185)'
                        width: 2 * GV.node_radius, height: 2 * GV.node_radius, borderRadius: '100%'
                        pointerEvents: 'none'

                        display: 'flex', flexDirection: 'column'
                        textAlign: 'center', alignItems: 'center', justifyContent: 'center'

                        color: 'black', fontFamily: 'sans-serif', fontSize: 16, fontWeight: 'light'
                    }>
                        {
                            if record.expr?[0] == 'call'
                                <React.Fragment>
                                    <div><span style={color: 'black'}>
                                        { E.ppexpr record.args[0].expr }
                                    </span></div>
                                    <div>→ <span style={color: 'blue'}>
                                        { E.inspect_value(record.value) }
                                    </span></div>
                                </React.Fragment>

                            else if record.expr?[0] == 'lit'
                                <React.Fragment>
                                    <div><span style={color: 'black'}>
                                        { E.inspect_value(record.value) }
                                    </span></div>
                                </React.Fragment>

                            else if record.expr?[0] == 'var'
                                <React.Fragment>
                                    <div><span style={color: 'brown'}>
                                        { E.inspect_value(record.value) }
                                    </span></div>
                                </React.Fragment>
                        }
                    </div>
                }
            />

    console_shortcuts: {
    }
