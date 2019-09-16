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
        # only run the program once, so we have one rr we can keep poking around with equal pointers
        # across time
        @lispy_ast = js_to_lispy(sample_js)
        @rr = E.lispy_eval(E.fresh_root_scope(), @lispy_ast)

        # UI state
        @active_record = null
        @collapsed = new Set()

        # set up console shortcuts
        Object.defineProperties window, _l.mapValues(@console_shortcuts, (v) -> {get: v})

    console_shortcuts: {
    }

    did_mount: ->
    render: ->
        @evaled = @rr.value
        root_scope = @rr.scope.vars

        record_is_method_call = (record) -> record.args?[0].value == root_scope['js/.()']
        color = (choice) -> (children) -> <span style={color: choice} children={children} />

        fib = (n) ->
            r = [1, 1]
            for i in [0...n]
                r = [r[0] + r[1], r[0]]
            return r[1]

        withWindowSize (window_size) ->
            <GV.GraphVisual
                style={
                    backgroundColor: 'rgb(230, 230, 230)'
                    marginTop: 20, marginLeft: 20
                    borderRadius: 10
                }
                width={window_size.width - 40} height={window_size.height - 40}
                root_nodes={[{n: 7, path: []}]}
                keyForNode={(p) -> p.path.concat(p.n).join('/')}
                pinned_nodes={new Set(["7"])}
                outedges={(p) ->
                    if p.n <= 1
                    then []
                    else [{n: p.n - 1, path: p.path.concat(p.n)}, {n: p.n - 2, path: p.path.concat(p.n)}]
                }
                renderNode={(p, is_hovered) ->
                    <div style={
                        backgroundColor: unless is_hovered then 'rgb(255, 248, 221)' else 'rgb(169, 226, 255)'
                        border: unless is_hovered then '2px solid rgb(160, 159, 94)' else '2px solid rgb(129, 146, 185)'
                        width: 2 * GV.node_radius, height: 2 * GV.node_radius, borderRadius: '100%'
                        pointerEvents: 'none'

                        display: 'flex', flexDirection: 'column'
                        textAlign: 'center', alignItems: 'center', justifyContent: 'center'

                        color: 'black', fontFamily: 'sans-serif', fontSize: 16, fontWeight: 'light'
                    }>
                        <div>fib(<span style={color: 'red'}>{p.n}</span>)</div>
                        <div>= <span style={color: 'blue'}>{fib p.n}</span></div>
                    </div>
                }
            />
