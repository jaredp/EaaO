React = require 'react'
createReactClass = require 'create-react-class'
_l = require 'lodash'
{ inspect } = require 'util'
ordered_map = _l.map

{js_to_lispy} = require './js_to_lisp'
babylon = require 'babylon'

E = require './lispy'
GV = require './graph-vis'

WrapRCRouteRenderInReactFunctionComponent = ({rcroute}) -> rcroute.rc_render()

export class JSCallTreeListView
    render: ->
        <WrapRCRouteRenderInReactFunctionComponent rcroute={this} />

    init: ->
        @sample_js = """
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

        @lispy_ast = js_to_lispy(@sample_js)
        @rr = E.lispy_eval(E.fresh_root_scope(), @lispy_ast)
        # tree of callee_records

        @dummy_tree = {
            v: 1,
            c: [
                {v: 2, c: [{v: 4, c: []}, {v: 6, c: []}]}
                {v: 3, c: [
                    {v: 4, c: []}
                    {v: 6, c: []}
                    {v: 6, c: []}
                ]}
                {v: 5, c: []}
                {v: 3, c: [{v: 4, c: []}]}
            ]
        }

    rc_render: ->
        keyForObject = GV.useUniqueKeyForObject()

        tree_list_view =
            <TreeListView
                root={@dummy_tree}
                keyForNode={keyForObject}
                getChildren={(n) -> n.c}
                renderNode={(n) ->
                    <span>{n.v}</span>
                }
            />

        <div style={margin: '3em', padding: '1em'}>
            { tree_list_view }
        </div>

# flatten_with_depth :: A -> (A -> depth -> B) -> [B]
#   where depth = number
flatten_with_depth = (root, fn) ->
    lst = []
    rec = (node, depth) ->
        fn({
            node, depth,
            rec: (child) -> rec(child, depth + 1)
            emit: (x) -> lst.push(x(lst.length))
        })
    rec(root, 0)
    return lst

useForceUpdate = ->
    [state, setState] = React.useState(0)
    forceUpdateFn = -> setState(state + 1)
    return forceUpdateFn

# useItOrLoseIt :: (A -> B) -> {get: A -> B, set: A -> B -> (), finalize: () -> ()}
useItOrLoseIt = (default_value_fn) ->
    forceUpdate = useForceUpdate()

    ref = React.useRef(new Map())
    next_cache = new Map()

    get = (key) ->
        if next_cache.has(key)
            return next_cache.get(key)

        else if ref.current.has(key)
            cached_value = ref.current.get(key)
            next_cache.set(key, cached_value)
            return cached_value

        else
            cached_value = default_value_fn()
            next_cache.set(key, cached_value)
            return cached_value

    finalize = (key) ->
        ref.current = next_cache

    set = (key, value) ->
        ref.current.set(key, value)
        forceUpdate()

    return {get, set, finalize}

TreeListView = ({root, keyForNode, renderNode, getChildren}) ->
    nkey_is_open_state = useItOrLoseIt(-> true)

    is_selected = false

    lines = flatten_with_depth root, ({node, depth, rec, emit}) ->
        children = getChildren(node)
        has_children = not _l.isEmpty(children)

        key = keyForNode(node)

        is_open = has_children and nkey_is_open_state.get(key)
        toggle_open = -> nkey_is_open_state.set(key, not is_open)

        triangle = if has_children
            <div style={
                display: 'inline-block'
                width: 0
                height: 0
                borderStyle: 'solid'
                borderWidth: '3.5px 0 3.5px 5.5px'
                transition: 'transform 0.1s ease-in-out'

                borderColor: "transparent transparent transparent #{if is_selected then '#fff' else 'rgba(0, 0, 0, 0.45)'}"
                transform: if is_open then 'rotate(90deg)' else ''

                # put it somewhere closer to the middle of the line
                position: 'relative', top: -3
            } />

        emit (line_idx) ->
            is_even_line_idx = line_idx % 2 == 0
            <div key={key} onDoubleClick={toggle_open} style={
                display: 'flex', flexDirection: 'row', padding: '4px 0'
                background: if is_even_line_idx then 'rgb(184, 216, 249)' else 'rgb(208, 231, 255)'
            }>
                <div onClick={toggle_open} children={triangle} style={
                    width: depth * 16 + 36

                    # if there is a triangle
                    textAlign: 'right'
                    paddingRight: 9
                    boxSizing: 'border-box'
                } />
                <div style={flex: 1}>
                    { renderNode(node) }
                </div>
            </div>

        if is_open
            rec(child) for child in children

    nkey_is_open_state.finalize()

    <React.Fragment>
        { lines }
    </React.Fragment>


