React = require 'react'
createReactClass = require 'create-react-class'
ReactDOM = require 'react-dom'
{config} = require './config'

# This file's purpose in life is to expose an onDrag interface via DraggingCanvas
# For that purpose, we map the browser's mouse events to our own implementation of a mouse state machine


# events :: {onDrag, onClick, onDoubleClick, onInteracted}
export MouseStateMachine = () ->
    # states: up, down, dragged
    state = 'up'
    initialPosition = null
    dragHandler = null
    events = null
    target = null
    lastClick = null
    currentMousePositionInWindow = {top: 0, left: 0}
    currentModifierKeysPressed = {altKey: false, shiftKey: false, metaKey: false, ctrlKey: false, capsLockKey: false}

    tracked_hover_targets = new Set()

    setCurrentModifierKeysPressed = (evt) ->
        currentModifierKeysPressed = {
            altKey: evt.altKey, shiftKey: evt.shiftKey, metaKey: evt.metaKey, ctrlKey: evt.ctrlKey,
            capsLockKey: evt.getModifierState('CapsLock')
        }


    getMousePositionForDiv = (_target) ->
        mouse = currentMousePositionInWindow

        # NOTE: _target is the thing we're clicking on. The only reason we use it is to calculate how zoomed in we are
        # so we can compensate for zoom
        target_bounds = _target.getBoundingClientRect()
        [logical_height, logical_width] = [_target.clientHeight, _target.clientWidth]
        return {
            top: Math.round((mouse.top - target_bounds.top) / target_bounds.height * logical_height)
            left: Math.round((mouse.left - target_bounds.left) / target_bounds.width * logical_width)
        }

    targetOffseted = (fn) -> (evt) ->
        currentMousePositionInWindow = {left: evt.clientX, top: evt.clientY}
        setCurrentModifierKeysPressed(evt)

        return if not target?
        interactedHandler = events.onInteracted # save this in case it changes in fn

        {top, left} = getMousePositionForDiv(target)
        fn({top, left, evt: evt, ctx: evt.context})

        interactedHandler?()

    distanceSquared = (coordsA, coordsB) -> Math.pow(coordsB[1] - coordsA[1], 2) + Math.pow(coordsB[0] - coordsA[0], 2)

    setCurrentModifierKeysPressed: setCurrentModifierKeysPressed
    getCurrentModifierKeysPressed: -> currentModifierKeysPressed

    getMousePositionForDiv: getMousePositionForDiv
    tracked_hover_targets: tracked_hover_targets

    down: (_target, evt, _events) ->
        console.warn('down mouse went down') if state != 'up'

        target = _target
        events = _events

        setCurrentModifierKeysPressed(evt)
        currentMousePositionInWindow = {left: evt.clientX, top: evt.clientY}
        {top, left} = getMousePositionForDiv(target)
        where = {top, left, evt: evt, ctx: evt.context}

        state = 'down'
        initialPosition = where
        dragHandler = null
        events.onInteracted?()

    move: targetOffseted (where) ->
        if state == 'up'
            # mouse is moving without a drag; do nothing
            return

        else if state == 'down'
            # transition from ambiguous mouse down to drag event

            # NOTE: here we use evt.clientX/Y instead of top/left since we care about absolute mouse position
            # top, left are relative and take zoom into account
            return if config.ignoreDragsWithinTolerance and distanceSquared([where.evt.clientX, where.evt.clientY], [initialPosition.evt.clientX, initialPosition.evt.clientY]) < config.maxSquaredDistanceForIgnoredDrag

            # start the drag handler
            dragHandler = {moved: (->), ended: (->)}
            events.onDrag?(initialPosition,
                ((h) -> dragHandler.moved = h),
                ((h) -> dragHandler.ended = h))

            state = 'dragged'

        if state == 'dragged' # or down, but if it was down we would be dragged now
            where.delta = {top: where.top - initialPosition.top, left: where.left - initialPosition.left}
            dragHandler.moved(where)

    up: targetOffseted (where) ->
        if state == 'up'
            console.warn('up mouse went up')

        else if state == 'down'
            all = (conds) ->
                for cond in conds
                    return false unless cond()
                return true

            if all [
                -> lastClick?
                -> distanceSquared([where.left, where.top], lastClick.location) < config.maxSquaredDistanceBetweenDoubleClick
                -> (Date.now() - lastClick.time) < config.maxMillisBetweenDoubleClick
            ]
                events.onDoubleClick?(initialPosition)

            else
                events.onClick?(initialPosition)

            # FIXME lastClick.location should not be relative to `where`, but to the screen.  `where` is relative to the
            # target.  Currently, clicking in two different targets at approx the same offset from each's (top,left) will
            # register as a double click, which is certainly wrong.
            lastClick = {location: [where.left, where.top], time: Date.now()}

        else if state == 'dragged'
            dragHandler.ended(where)

        # no matter where we were, we're now up
        state = 'up'
        dragHandler = null
        initialPosition = null
        target = null
        events = null


    reset: ->
        # we might want to clear out state after eg. a crash
        state = 'up'
        dragHandler = null
        initialPosition = null
        target = null
        events = null




export windowMouseMachine = MouseStateMachine()
window.__windowMouseMachine = windowMouseMachine if config.registerGlobalsForDebugging


###
- Ticks execute synchronously
- it's safe for event handlers to add new renders during a tick
- it's not safe for events to add other events during a tick
- Scheduling an event/render is idempotent.  It relies on object identity though.
- Canceling an event/render is safe to do even if the event/render wasn't scheduled.
###
# requestNextTick :: () -> request_id
# cancelNextTick :: (request_id) -> ()
export CoalescingScheduler = (requestNextTick, cancelNextTick) ->
    pending_events = new Set() # Set (() -> ())
    pending_renders = new Set() # Set (() -> ())
    pending_next_tick_setups = new Set() # Set (() -> ())

    next_tick_request_id = null
    tick_is_scheduled = -> next_tick_request_id != null

    run_phase = (steps) ->
        while steps.size > 0
            steps_at_start_of_batch = new Set(steps)
            steps.clear()
            steps_at_start_of_batch.forEach (step) -> step()

    execute = ->
        while (pending_events.size + pending_renders.size) > 0
            run_phase(pending_events)
            run_phase(pending_renders)

        # end of this cycle.  pending_next_tick_setups-caused schedules will schedule us for another tick
        next_tick_request_id = null

        run_phase(pending_next_tick_setups)

        # for safety
        reschedule()

    reschedule = ->
        should_be_scheduled = (pending_events.size + pending_renders.size + pending_next_tick_setups.size) > 0
        is_scheduled = tick_is_scheduled()

        if should_be_scheduled and not is_scheduled
           next_tick_request_id = requestNextTick(execute)

        else if not should_be_scheduled and is_scheduled
            cancelNextTick(next_tick_request_id)
            next_tick_request_id = null

    return {
        schedule_event: (evt) ->
            pending_events.add(evt)
            reschedule()

        cancel_event: (evt) ->
            pending_events.delete(evt)
            reschedule()

        schedule_render: (render) ->
            pending_renders.add(render)
            reschedule()

        cancel_render: (render) ->
            pending_renders.delete(render)
            reschedule()

        schedule_next_tick_setup: (setup) ->
            pending_next_tick_setups.add(setup)
            reschedule()

        cancel_next_tick_setup: (setup) ->
            pending_next_tick_setups.delete(setup)
            reschedule()
    }

# Check window? so we're safe to run on a server when we're a recursive dependency
# of mixed client/server code.  Also, who knows, this may be useful for mocking in a test env.

export windowAnimationFrameScheduler =
    if window?
    then CoalescingScheduler(window.requestAnimationFrame, window.cancelAnimationFrame)
    else null

if window?
    debounced_move_event = null

    fire_move_event = ->
        # clear debounced_move_event before doing anything else, just to be clean
        [evt, debounced_move_event] = [debounced_move_event, null]

        # call pass the event to windowMouseMachine
        windowMouseMachine.move(evt)

    window.addEventListener 'mousemove', (evt) ->
        debounced_move_event = evt
        windowAnimationFrameScheduler.schedule_event(fire_move_event)

    window.addEventListener 'mouseup', (evt) ->
        # flush a pending move event, if there is one
        windowAnimationFrameScheduler.cancel_event(fire_move_event)
        fire_move_event() if debounced_move_event

        # pass the event to windowMouseMachine
        windowMouseMachine.up(evt)

useForceUpdate = ->
    [dummyState, setDummyState] = React.useState()
    return -> setDummyState({})

# useRegisterOnscreen :: Set A -> A -> ()
useRegisterOnscreen = (registry, elem) ->
    registry.add(elem)
    return ->
        registry.delete(elem)

useLatest = (prop_value) ->
    ref = React.useRef(null)
    ref.current = prop_value
    return ref

useZoomHandler = (dom_ref, _zoom_handler, _scroll_handler) ->
    # huh, so React has weird stale callback issues now doesn't it
    zoom_handler = useLatest(_zoom_handler)
    scroll_handler = useLatest(_scroll_handler)

    React.useEffect((->
        return if dom_ref.current == null

        wheel_handler = (evt) ->
            do_zoom = (zoomMultiplier) ->
                return unless zoom_handler.current != null and dom_ref.current != null
                evt.preventDefault()
                zoom_factor = 1 / (1 + evt.deltaY/zoomMultiplier)
                # FIXME same as maybe_mouse below
                container_pos = dom_ref.current.getBoundingClientRect()
                at_point = {
                    top: evt.clientY - container_pos.top
                    left: evt.clientX - container_pos.left
                }
                zoom_handler.current(zoom_factor, at_point, evt)

            if evt.ctrlKey
                # for some reason, scroll events with ctrlKey=true are how we get pinch events
                do_zoom(80)

            else if evt.metaKey
                # We also consider metaKey + scroll to be zoom
                do_zoom(2000)

            else
                # all other events are scroll events
                return unless scroll_handler.current != null
                evt.preventDefault()
                scroll_handler.current(evt)

        # passive:false so we can evt.preventDefault() if we need to
        dom_ref.current.addEventListener('wheel', wheel_handler, passive: false)
        unregister = -> dom_ref.current.removeEventListener('wheel', wheel_handler)
        return unregister
    ), [dom_ref.current])

export DraggingCanvas = ({render, onDrag, onClick, onDoubleClick, onInteractionHappened, onPinchZoom, onScroll, others...}) ->
    # FIXME
    # - probably getting the DOM node way too many times
    # - should probably do a first render without any children just to have a dom node to compare with mouse
    maybe_dom = React.useRef(null)

    useZoomHandler(maybe_dom, onPinchZoom, onScroll)

    # FIXME: we're always going to get the mouse relative to the div in the last render
    # for Pagedraw, the canvas was always in the same place in the window unless the window resized,
    # and we explicitly caught that.  The same likely seems true here.
    # If we wanted full correctness, we'd probably want to check the DOM's location in componentDidUpdate
    # is unchanged from the current position.  If it's changed, rerender.  Worry about getting in a rerender
    # loop if the layout changed because of the hover, and re-rendering changes the layout again.
    maybe_mouse =
        if maybe_dom.current?
        then windowMouseMachine.getMousePositionForDiv(maybe_dom.current)
        else null

    <div {others...}
        ref={maybe_dom}
        children={render(maybe_mouse)}
        onMouseDown={(evt) ->
            # ignore non-left clicks
            return if evt.nativeEvent.which != 1

            # We register these handlers here because in case there are multiple DraggingCanvases in a screen,
            # this essentially says "the current dragging canvas owns this interaction until it ends" starting when the mouse goes down
            # mousemove and mouseup events are handled window-wide above because an interaction is allowed to start in a DraggingCanvas and
            # end somewhere else
            windowMouseMachine.down(maybe_dom.current, evt.nativeEvent, {
                onDrag: onDrag
                onClick: onClick
                onDoubleClick: onDoubleClick
                onInteracted: => onInteractionHappened
            })

            evt.preventDefault()
        }
    />
