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

            function mul(x, y) {
                return x * y;
            }

            function myfunc(x, y, c) {
                return mul(mul(x, y) + mul(y, x), c.multiplier);
            }

            var z = x + y;
            var a = x * y;
            var b = z + a;
            var c = myfunc(a, z, {multiplier: 6});
            var d = "Hello " + c + "!";
        """

        @lispy_ast = js_to_lispy(@sample_js)
        @rr = E.lispy_eval(E.fresh_root_scope(), @lispy_ast)

    rc_render: ->
        keyForObject = GV.useUniqueKeyForObject()

        # selected :: Maybe CallRecord
        [selected, setSelected] = React.useState(null)

        tree_list_view =
            <TreeListView
                roots={_l.flatMap @rr.args, E.immediate_call_records}
                keyForNode={keyForObject}
                getChildren={(cr) -> E.callee_records(cr)}
                renderNode={(cr) -> E.full_label_for_call_record(cr)}
                selected={selected} setSelected={setSelected}
            />

        <div style={paddingTop: '1em'}>
            <code style={{...E.pane_style, margin: '2em'}}>{@sample_js}</code>
            { tree_list_view }
        </div>

# flatten_with_depth :: [A] -> (A -> depth -> {emit: (idx -> B), rec: [A]}) -> [B]
# except that emit and rec are implemented as lambdas passed to the fn, rather than a tuple
# the fn returns.
# Whether rec is invoked before or after emit controls whether the tree is flattened
# preorder or postorder.
# Despite the type given above, this is deeply imperative.
flatten_with_depth = (roots, fn) ->
    lst = []
    rec = (node, depth) ->
        fn({
            node, depth,
            rec: (child) -> rec(child, depth + 1)
            emit: (x) -> lst.push(x(lst.length))
        })
    rec(root, 0) for root in roots
    return lst

useForceUpdate = ->
    [state, setState] = React.useState(0)
    forceUpdateFn = -> setState(state + 1)
    return forceUpdateFn

# useItOrLoseIt :: Hook {get: A -> B, set: A -> B -> (), cycle: ((A -> B) -> ()) -> ()}
useItOrLoseIt = ->
    forceUpdate = useForceUpdate()

    ref = React.useRef(new Map())

    # usage_fn :: (A -> B) -> ()
    cycle = (usage_fn) ->
        next_cache = new Map()

        getter = (key, default_value_fn) ->
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

        retval = usage_fn(getter)

        ref.current = next_cache

        return retval

    set = (key, value) ->
        ref.current.set(key, value)
        forceUpdate()

    get = (key) ->
        return ref.current.get(key)

    return {get, set, cycle}

TreeListView = ({
    roots, keyForNode, renderNode, getChildren,
    selected, setSelected
}) ->
    nkey_is_open_state = useItOrLoseIt(-> true)
    nkey_is_open_state.cycle (get_nkey_is_open) ->
        # visible_nodes :: [Node]
        # line_uis :: [React.Element]
        [visible_nodes, line_uis] = _l.unzip flatten_with_depth roots, ({node, depth, rec, emit}) ->
            children = getChildren(node)
            has_children = not _l.isEmpty(children)

            is_selected = (node == selected)
            key = keyForNode(node)

            toggle_open = -> nkey_is_open_state.set(key, not is_open)
            is_open = has_children and get_nkey_is_open(key, () -> depth < 1)

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
                    position: 'relative', top: 1
                } />

            emit (line_idx) ->
                is_even_line_idx = line_idx % 2 == 0
                line_ui =
                    <div
                        key={key}
                        onClick={-> setSelected(node)} onDoubleClick={toggle_open}
                        style={
                            display: 'flex', flexDirection: 'row', padding: '4px 0'
                            background:
                                if is_selected then 'blue'
                                else if is_even_line_idx
                                then 'rgb(255, 255, 255)'
                                else 'rgb(241, 242, 242)'
                            color: 'white' if is_selected
                        }
                    >
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
                [node, line_ui]

            if is_open
                rec(child) for child in children

        set_selected_open = (should_open) ->
            selected_key = keyForNode(selected)
            nkey_is_open_state.set(selected_key, should_open)

        find_node_at_offset_from_selected = (delta) ->
            return null if selected == null
            selected_index = visible_nodes.indexOf(selected)
            return null if selected_index == -1
            target_index = selected_index + delta
            return null unless 0 <= target_index < visible_nodes.length
            return visible_nodes[target_index]

        <div tabIndex={0} onKeyDown={(evt) -> switch evt.key
            when 'ArrowUp'
                if (target = find_node_at_offset_from_selected(-1))? then setSelected(target)
                else if selected == null and visible_nodes.length > 0 then setSelected _l.last(visible_nodes)
            when 'ArrowDown'
                if (target = find_node_at_offset_from_selected(+1))? then setSelected(target)
                else if selected == null and visible_nodes.length > 0 then setSelected(visible_nodes[0])
            when 'ArrowLeft'           then set_selected_open(no)
            when 'ArrowRight', 'Enter' then set_selected_open(yes)
        }>
            { line_uis }
        </div>


