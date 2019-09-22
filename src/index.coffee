import React from 'react'
import ReactDOM from 'react-dom'
createReactClass = require 'create-react-class'
import './index.css'

_l = require 'lodash'

{ Classic, JSTOLisp, JSTimeline, LispySyntaxExplorer, Lispy } = require './lispy'
{ DummyGraph, FibGraph } = require './graph-vis'
{ JSDFG } = require './js-graph'
{ RandomReactTest } = require './random-react-test'

class RCRoute
    constructor: (@component_type) ->
    init: (@react_root) ->
    did_mount: ->
    render: -> React.createElement(@component_type)

routes = {
    '/react/bench/firstload': -> new RCRoute(RandomReactTest)
    '/syntax/js': -> new JSTOLisp()
    '/syntax/lispy': -> new LispySyntaxExplorer()
    '/classic/lispy': -> new Classic()
    '/timeline/js': -> new JSTimeline()
    '/timeline/lispy': -> new Lispy()
    '/dfg/js': -> new RCRoute(JSDFG)
    '/graph/dummy': -> new RCRoute(DummyGraph)
    '/graph/fib': -> new RCRoute(FibGraph)
}

default_route = ->
    <div>
        <ul>
        { _l.keys(routes).map (path) -> <li key={path}><a href={path}>{path}</a></li> }
        </ul>
    </div>

App = createReactClass
    componentWillMount: ->
        @app_state =
            routes[window.location.pathname]?() ?
            routes[window.location.hash.slice(1)]?() ?
            new RCRoute(default_route)
        window.ui = @app_state
        @app_state.init(this)

    componentDidMount: ->
        @app_state.did_mount?()

    render: -> @app_state.render()

ReactDOM.render <App />, document.getElementById('root')
