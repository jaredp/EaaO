import React from 'react'
import ReactDOM from 'react-dom'
createReactClass = require 'create-react-class'
import './index.css'

_l = require 'lodash'

{ Classic, JSTOLisp, JSTimeline, LispySyntaxExplorer, Lispy } = require './lispy'
{ DummyGraph, FibGraph, DummyObjGraph } = require './graph-vis'
{ JSDFG, JSTraceExplorer } = require './js-graph'
{ RandomReactTest } = require './random-react-test'

class RCRoute
    constructor: (@component_type) ->
    init: (@react_root) ->
    did_mount: ->
    render: -> React.createElement(@component_type)

user_defined_routes = {
    '/react/bench/firstload': -> new RCRoute(RandomReactTest)
    '/syntax/js': -> new JSTOLisp()
    '/syntax/lispy': -> new LispySyntaxExplorer()
    '/classic/lispy': -> new Classic()
    '/timeline/js': -> new JSTimeline()
    '/timeline/lispy': -> new Lispy()
    '/dfg/js': -> new RCRoute(JSDFG)
    '/objgraph/dummy': -> new RCRoute(DummyObjGraph)
    '/objgraph/trace/js': -> new RCRoute(JSTraceExplorer)
    '/graph/dummy': -> new RCRoute(DummyGraph)
    '/graph/fib': -> new RCRoute(FibGraph)
}

IndexPage = ->
    <div>
        <ul>
        { _l.keys(user_defined_routes).map (path) -> <li key={path}><a href={path}>{path}</a></li> }
        </ul>
    </div>

routes = {
    '/': -> new RCRoute(IndexPage)
    ...user_defined_routes
}

not_found_404_route = ->
    <div style={textAlign: 'center', padding: '4em'}>
        <div>404 Not found</div>
        <div><a href="/">← Back to index</a></div>
    </div>

App = createReactClass
    componentWillMount: ->
        @app_state =
            routes[window.location.pathname]?() ?
            routes[window.location.hash.slice(1)]?() ?
            new RCRoute(not_found_404_route)
        window.ui = @app_state
        @app_state.init(this)

    componentDidMount: ->
        @app_state.did_mount?()

    render: -> @app_state.render()

ReactDOM.render <App />, document.getElementById('root')
