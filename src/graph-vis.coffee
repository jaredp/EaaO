React = require 'react'
createReactClass = require 'create-react-class'
{config} = require './config'

_l = require 'lodash'
{ DraggingCanvas, windowAnimationFrameScheduler } = require './DraggingCanvas'


find_connected = (start_points, get_key, get_neighbors) ->
    seen = new Set()
    nodes = new Set()
    explore = (node) ->
        nkey = get_key(node)
        return if seen.has(nkey)
        seen.add(nkey)
        nodes.add(node)
        explore(neighbor) for neighbor in get_neighbors(node)
    explore(start_point) for start_point in start_points
    return Array.from(nodes)

# type Vec2 = [x :: Number, y :: Number]
# type Pt2 = Vec2
vsub = ([x1, y1], [x2, y2]) -> [x1 - x2, y1 - y2]
vdelta = (from, to) -> vsub(to, from)
vadd = ([x1, y1], [x2, y2]) -> [x1 + x2, y1 + y2]
vmag_sq = ([x, y]) -> x**2 + y**2
vmag = (vec) -> Math.sqrt(vmag_sq(vec))
vscale = ([x, y], k) -> [x * k, y * k]
vunit = (v) ->
    mag = vmag(v)
    if mag != 0 then vscale(v, 1/mag) else [0, 0]
v_of_len = (v, len) ->
    mag = vmag(v)
    if mag != 0 then vscale(v, len/mag) else [0, 0]

vsum = (vecs) ->
    sum = [0, 0]
    vadd_iplace(sum, vec) for vec in vecs
    return sum

vmean = (vecs) -> vscale vsum(vecs), (1/vecs.length)
pts_near = (p1, p2, dist) -> vmag_sq(vsub(p1, p2)) <= dist ** 2

vset = (dst, src) -> [dst[0], dst[1]] = src
vadd_iplace = (v1, v2) ->
    v1[0] += v2[0]
    v1[1] += v2[1]
vadded_iplace = (dst, src) ->
    vadd_iplace(dst, src)
    return dst

rect_from_bl_tr = (bl, tr) -> [bl, tr]

# take the avg of the two points
rect_center = ([bl, tr]) -> vscale(vadd(bl, tr), 0.5)
bounding_rect = (vecs) ->
    [xs, ys] = _l.unzip(vecs)
    bl = [Math.min(xs...), Math.min(ys...)]
    tr = [Math.max(xs...), Math.max(ys...)]
    return rect_from_bl_tr(bl, tr)



# map_maybe :: A? -> (A -> B) -> B?
map_maybe = (arg, fn) -> if arg? then fn(arg) else null

# DraggingCanvas uses {top, left}, but we use [x, y]
tl2pt = ({top, left}) -> [left, top]

spring_force = (p1, p2, resting_length, k) ->
    delta = vdelta(p1, p2)
    direction = vunit(delta)
    displacement = vmag(delta) - resting_length
    force = vscale direction, (k * displacement)
    return force

attraction_force = (p1, p2, k) ->
    delta = vdelta(p1, p2)
    # want to return direction * (1 / r**2) * k,
    #              = delta * (1 / r**3) * k
    #              = delta * (k / r**3)
    return vscale(delta, k / Math.max(vmag(delta)**3, 100))

export node_radius = 30
box_space = 5
box_hover_rad = (node_radius + box_space / 2)
frames_between_simulation_repaint = 2 # 60fps / 3 = 20fps simulation


export GraphVisualImpl = createReactClass
    needsUpdate: -> windowAnimationFrameScheduler.schedule_render(@boundForceUpdate)
    boundForceUpdate: -> @forceUpdate()

    componentWillMount: ->
        @center_for_nkey = @initial_centers_for_nkeys()
        @pinned_nodes = new Set()
        @selected_nkey = null

        @expects_physics = yes
        @last_simulation_ts_ms = null
        @reset_simulation()
        @stabilize_physics_before_first_render()

        @set_best_viewport()

    componentDidMount: ->
        @frames_until_simulation_repaint = 1
        @reschedule_physics()

    componentWillUnmount: ->
        windowAnimationFrameScheduler.cancel_render(@boundForceUpdate)

    reschedule_physics: ->
        windowAnimationFrameScheduler.schedule_event(@runPhysics)

    runPhysics: ->
        return unless @expects_physics

        now_ts_ms = performance.now()
        dt_ms =
            if @last_simulation_ts_ms?
            then now_ts_ms - @last_simulation_ts_ms
            else 16
        dt_ms = Math.min(dt_ms, 50)
        @last_simulation_ts_ms = now_ts_ms

        while dt_ms > 1
            @simulation_tick(1)
            dt_ms -= 1
        @simulation_tick(dt_ms)

        @frames_until_simulation_repaint -= 1
        if @frames_until_simulation_repaint < 1
            @needsUpdate()
            @frames_until_simulation_repaint = frames_between_simulation_repaint

        windowAnimationFrameScheduler.schedule_next_tick_setup(@reschedule_physics) unless @serious_invariant_violation()

    serious_invariant_violation: ->
        is_broken = no
        for nkey, pt of @center_for_nkey when not _l.isArray(pt) and _l.isFinite(pt[0]) and _l.isFinite(pt[1])
            console.log "broken:", nkey, "is", pt
            is_broken = yes
        return is_broken

    stabilize_physics_before_first_render: ->
        increment_ms = 3
        initial_ticks_per_second = 1000 / increment_ms
        seconds_to_run_physics_before_render = 20
        initial_ticks = seconds_to_run_physics_before_render * initial_ticks_per_second
        @simulation_tick(increment_ms) for i in [1...Math.round(initial_ticks)]

    initial_centers_for_nkeys: ->
        cols = 3
        return _l.fromPairs @props.nodes.map (node, idx) =>
            [@props.keyForNode(node), [
                (idx % cols) * (node_radius * 2 + box_space),           # x
                Math.floor(idx / cols) * (node_radius * 2 + box_space) # y
            ]]

    node_centroid: -> vmean _l.values @center_for_nkey

    set_best_viewport: ->
        favored_viewport_center = rect_center(@props.favored_viewport_area)
        absolute_viewport_center = @viewport_center()
        node_area_center = rect_center bounding_rect(_l.values(@center_for_nkey))

        @viewport_scale = 1
        @canvas_center = vadd node_area_center, vdelta(absolute_viewport_center, favored_viewport_center)

    componentWillReceiveProps: (next_props) ->
        # give positions to new nodes
        # for now just put all of them in the center
        center = null # compute once, but only if you have to

        continued_nkeys = new Set()

        for node in next_props.nodes
            nkey = next_props.keyForNode(node)
            continued_nkeys.add(nkey)
            continue unless @center_for_nkey[nkey]? == false
            center ?= vmean _l.values @center_for_nkey
            @center_for_nkey[nkey] = vadded_iplace [Math.random() * 10 - 5, Math.random(10) - 5], center
            @velocities[nkey] = [0, 0]

        for nkey, center of @center_for_nkey when continued_nkeys.has(nkey) == no
            delete @center_for_nkey[nkey]
            delete @velocities[nkey]


    forall_edges_by_keys: (fn) ->
        for node in @props.nodes
            nkey = @props.keyForNode(node)
            for edge_dst in @props.outedges(node)
                edge_dst_key = @props.keyForNode(edge_dst)
                fn(nkey, edge_dst_key)


    # neighbors_by_nkey :: {nkey: Set(nkey)}
    # neighbors_by_nkey[a].has(b) iff there's an edge between a and b, in either direction
    # neighbors_by_nkey[b].has(a) == neighbors_by_nkey[a].has(b)
    get_neighbors_by_nkey: ->
        neighbors_by_nkey = _l.fromPairs @props.nodes.map (n) => [@props.keyForNode(n), new Set()]
        @forall_edges_by_keys (src_nkey, dst_nkey) ->
            neighbors_by_nkey[src_nkey].add(dst_nkey)
            neighbors_by_nkey[dst_nkey].add(src_nkey)
        return neighbors_by_nkey

    reset_simulation: ->
        @velocities = _l.mapValues @center_for_nkey, -> [0, 0]

    simulation_tick: (dt_ms) ->
        map_reduce_set = (init, reducer, set, mapper) ->
            accum = init
            set.forEach (elem) -> reducer(accum, mapper(elem))
            return accum

        neighbors_by_nkey = @get_neighbors_by_nkey()

        resting_length = 2 * node_radius + box_space

        # compute the forces on each node
        fnet_by_nkey = _l.mapValues @center_for_nkey, (center, nkey) =>
            spring_forces = map_reduce_set [0, 0], vadd_iplace, neighbors_by_nkey[nkey], (neighbor_nkey) =>
                spring_force(center, @center_for_nkey[neighbor_nkey], resting_length, 0.00005)

            repulsion_charges = [0, 0]
            for other_key, other_center of @center_for_nkey when nkey != other_key
                vadd_iplace repulsion_charges, attraction_force(center, other_center, -5)

            friction_force = v_of_len @velocities[nkey], -0.0006

            gravity = [0, 0] #[0, +0.001]

            return vsum [spring_forces, repulsion_charges, friction_force, gravity]

        # imperatively move the nodes according to the forces on them
        for nkey, force of fnet_by_nkey when not (@pinned_nodes.has(nkey) or @props.pinned_nodes.has(nkey))
            velocity = @velocities[nkey]

            # what we really care about is acceleration, but just assume all masses = 1
            # velocity += force*dt
            vadd_iplace(velocity, vscale(force, dt_ms))

            # clamp velocity to a max
            max_velocity = 30
            vset(velocity, v_of_len(velocity, max_velocity)) if vmag(velocity) > max_velocity

            # position += velocity*dt
            vadd_iplace(@center_for_nkey[nkey], vscale(velocity, dt_ms))

        # not technically physics' job, but make sure all pinned nodes have no energy
        @pinned_nodes.forEach (nkey) => @velocities[nkey] = [0, 0]
        @props.pinned_nodes.forEach (nkey) => @velocities[nkey] = [0, 0]



    viewport_center: -> [@props.width/2, @props.height/2]
    to_viewport: (pt) -> vadded_iplace @viewport_center(), vscale(vdelta(@canvas_center, pt), @viewport_scale)
    from_viewport: (pt) -> vadd @canvas_center, vscale(vdelta(@viewport_center(), pt), 1/@viewport_scale)

    # DraggingCanvas uses {top, left}, but we use [x, y]
    from_viewport_tl: (tl) -> @from_viewport tl2pt tl
    # maybe_nkey_at_mouse :: Pt -> Maybe NKey where NKey = String
    maybe_nkey_at_mouse: (mouse) -> _l.findKey @center_for_nkey, (np) => pts_near(np, mouse, box_hover_rad / @viewport_scale)

    # maybe_node_for_nkey is O(n)
    maybe_node_for_nkey: (nkey) ->  _l.find(@props.nodes, (n) => @props.keyForNode(n) == nkey)

    render: ->
        <DraggingCanvas
            style={{
                ...@props.style
                ..._l.pick(@props, ['width', 'height']), overflow: 'hidden'
                position: 'relative'
            }}
            onClick={(where) =>
                # FIXME this whole implementation will need to change

                maybe_nkey = @maybe_nkey_at_mouse @from_viewport_tl(where)
                @selected_nkey = maybe_nkey

                # O(n) sucks, but we do that on every render so... this kind of has to be fast anyway
                if (clicked_node = map_maybe(maybe_nkey, @maybe_node_for_nkey))?
                    @props.onClickNode?(clicked_node)
                else
                    @props.onClickOutside?()

                @needsUpdate()
            }
            onDoubleClick={(where) =>
                @selected_nkey = @maybe_nkey_at_mouse @from_viewport_tl(where)
                @needsUpdate()
            }
            onDrag={(vp_start, onMove, onEnd) =>
                start = @from_viewport_tl(vp_start)

                if (nkey = @maybe_nkey_at_mouse(start))?
                    initial_pos = _l.clone @center_for_nkey[nkey]
                    @pinned_nodes.add(nkey)

                    onMove (vp_to) =>
                        @center_for_nkey[nkey] = vadd initial_pos, vdelta(start, @from_viewport_tl(vp_to))
                        @needsUpdate()

                    onEnd (at) =>
                        @pinned_nodes.delete(nkey)
                        @needsUpdate()

            }
            onMouseMove={(evt) =>
                # sort of really gross
                @needsUpdate()
            }
            onPinchZoom={(zoom_factor, at_vp_tl, evt) =>
                holding_vp_point_constant = (vp_point, mutator) =>
                    vp_point_in_canvas_start = @from_viewport(vp_point)
                    mutator()
                    vp_point_in_canvas_end = @from_viewport(vp_point)
                    vadd_iplace @canvas_center, vdelta(vp_point_in_canvas_end, vp_point_in_canvas_start)

                # GOAL at_vp_point should stay the same point in canvas-space as before
                holding_vp_point_constant tl2pt(at_vp_tl), =>
                    @viewport_scale *= zoom_factor

                @needsUpdate()
            }
            onScroll={(evt) =>
                vadd_iplace @canvas_center, vscale([evt.deltaX, evt.deltaY], 1/@viewport_scale)
                @needsUpdate()
            }
            render={(maybe_mouse_position) =>
                active_nkey = @selected_nkey ? map_maybe(maybe_mouse_position, (o) => @maybe_nkey_at_mouse @from_viewport_tl o)

                arrow = (src_ctr, dst_ctr, color, key) =>
                    [vp_src_x, vp_src_y] = @to_viewport(src_ctr)
                    arrow_delta = vdelta(src_ctr, dst_ctr)
                    arrow_head_len = 6
                    [vp_dst_x, vp_dst_y] = vsub @to_viewport(dst_ctr), v_of_len(arrow_delta, node_radius + arrow_head_len)
                    <line
                        key={key}
                        markerEnd="url(#head)"
                        x1={Math.round vp_src_x} y1={Math.round vp_src_y}
                        x2={Math.round vp_dst_x} y2={Math.round vp_dst_y}
                        stroke={color} strokeWidth={3}
                    />

                <React.Fragment>
                    <svg style={position: 'absolute', top: 0, left: 0} width={@props.width} height={@props.height}>
                        <defs>
                          <marker id='head' orient="auto" markerWidth='2' markerHeight='4' refX='0' refY='2'>
                            <path d='M0,0 V4 L2,2 Z' fill="black"/>
                          </marker>
                        </defs>
                        { @props.nodes.map (node) =>
                            definer_key = @props.keyForNode(node)
                            <React.Fragment key={definer_key}>
                                { @props.outedges(node).map (dst) =>
                                    indicated_key = @props.keyForNode(dst)
                                    # swap the direction of the arrow
                                    [src_key, dst_key] = [indicated_key, definer_key]
                                    color = switch active_nkey
                                        when src_key then "#e60404"
                                        when dst_key then "#0814c1"
                                        else "#CCC"
                                    arrow(@center_for_nkey[src_key], @center_for_nkey[dst_key], color, indicated_key)
                                }
                            </React.Fragment>
                        }
                    </svg>
                    { @props.nodes.map (node) =>
                        key = @props.keyForNode(node)
                        [vp_x, vp_y] = vsub @to_viewport(@center_for_nkey[key]), [node_radius, node_radius]
                        <div children={@props.renderNode(node, key == active_nkey)} key={key} style={{
                            position: 'absolute', top: 0, left: 0,
                            willChange: 'transform'
                            transform: "translate(#{Math.round(vp_x)}px, #{Math.round(vp_y)}px)",
                            width: node_radius * 2, height: node_radius * 2
                            display: 'flex', alignItems: 'center', justifyContent: 'center'
                        }} />
                    }
                </React.Fragment>
            }
        />



class ObjIDs
    # ids vended are unique in the namespace of this ObjID vendor,
    # and may conflict with ids vended by other instances of ObjIDs
    constructor: ->
        @last_id_num = 0
        @obj_to_id= new WeakMap()

    # get :: Any -> String
    get: (obj) ->
        if (existing_id = @obj_to_id.get(obj))?
            return existing_id
        else
            next_id_num = @last_id_num + 1
            new_id = "#{next_id_num}"
            @obj_to_id.set(obj, new_id)
            @last_id_num = next_id_num
            return new_id


# Friendly API
export GraphVisual = (props) ->
    props = _l.clone(props)

    state = React.useRef({
        obj_keyer: new ObjIDs()
    })

    unless props.keyForNode?
        # default to referential equality
        props.keyForNode = (node) -> state.current.obj_keyer.get(node)

    if props.root_nodes?
        props.nodes = find_connected(props.root_nodes, props.keyForNode, props.outedges)
        delete props.root_nodes

    props.favored_viewport_area ?= rect_from_bl_tr([0, 0], [props.width, props.height])
    props.pinned_nodes ?= new Set()

    <GraphVisualImpl {props...} />


# Sample usage

export getWindowSize = -> {width: window.innerWidth, height: window.innerHeight}
export useWindowSize = ->
    [windowSize, setWindowSize] = React.useState(getWindowSize())
    React.useEffect((->
        resizeHandler = -> setWindowSize(getWindowSize())
        window.addEventListener('resize', resizeHandler)
        unset = -> window.removeEventListener('resize', resizeHandler)
        return unset
    ), [])
    return windowSize

dummy_graph = do ->
    data = {
        0: [1, 3]
        1: [2, 3, 4]
        2: []
        3: [5]
        4: [5, 7]
        5: [7]
        6: [3]
        7: []
        8: [1, 3]
        9: [2, 3, 4]
        10:[]
        11:[5]
        12:[5, 7]
        13:[7]
        14:[3]
        15:[]
    }

    nodes = [0...15].map (i) -> {id: "#{i}", friends: []}
    for src_idx, dst_idxes of data
        for dst_idx in dst_idxes
            nodes[src_idx].friends.push(nodes[dst_idx])
    return nodes

export DummyGraph = DummyGraph = ->
    window_size = useWindowSize()

    [root_nkeys, set_root_nkeys] = React.useState([6, 9])
    add_node_named = (name) -> set_root_nkeys root_nkeys.concat(name)

    add_node_form = do =>
        [addNodeInput, setAddNodeInput] = React.useState('')
        <form onSubmit={(evt) ->
            evt.preventDefault()
            add_node_named(addNodeInput)
            setAddNodeInput('')
        }>
            <input value={addNodeInput} onChange={(evt) -> setAddNodeInput(evt.target.value)} />
            <input type="submit" value="add" />
        </form>

    <React.Fragment>
        <GraphVisual
            style={
                backgroundColor: 'rgb(230, 230, 230)'
                marginTop: 20, marginLeft: 20
                borderRadius: 10
            }
            width={window_size.width - 40} height={window_size.height - 40}
            root_nodes={_l.compact root_nkeys.map (nk) -> dummy_graph[nk]}
            keyForNode={(p) -> p.id}
            outedges={(p) -> p.friends}
            renderNode={(p, is_hovered) ->
                <div style={
                    backgroundColor: unless is_hovered then 'rgb(255, 248, 221)' else 'rgb(169, 226, 255)'
                    border: unless is_hovered then '2px solid rgb(160, 159, 94)' else '2px solid rgb(129, 146, 185)'
                    color: 'black', fontFamily: 'sans-serif', fontSize: 16, fontWeight: 'light'
                    width: 2 * node_radius, height: 2 * node_radius, borderRadius: '100%'
                    display: 'flex', alignItems: 'center', justifyContent: 'center'
                    pointerEvents: 'none'
                    textAlign: 'center'
                }>Person {p.id}</div>
            }
        />
        <div style={position: 'fixed', bottom: 35, right: 40}>
            { add_node_form }
        </div>
    </React.Fragment>

fib = (n) ->
    r = [1, 1]
    for i in [0...n]
        r = [r[0] + r[1], r[0]]
    return r[1]

export FibGraph = FibGraph = ->
    window_size = useWindowSize()

    <React.Fragment>
        <GraphVisual
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
                    width: 2 * node_radius, height: 2 * node_radius, borderRadius: '100%'
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
    </React.Fragment>
