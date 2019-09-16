import React from 'react'
import ReactDOM from 'react-dom'
createReactClass = require 'create-react-class'
import './index.css'
import registerServiceWorker from './registerServiceWorker';

{ Classic, JSTOLisp, JSTimeline, LispySyntaxExplorer, Lispy } = require './lispy'
{ DummyGraph, FibGraph } = require './graph-vis'
{ JSDFG } = require './js-graph'

class RCRoute
    constructor: (@component_type) ->
    init: (@react_root) ->
    did_mount: ->
    render: -> React.createElement(@component_type)

routes = {
    '/classic': -> new Classic()
    '/js-to-lispy': -> new JSTOLisp()
    '/js': -> new JSTimeline()
    '/lispy-syntax': -> new LispySyntaxExplorer()
    '/dummy-graph': -> new RCRoute(DummyGraph)
    '/fib-graph': -> new RCRoute(FibGraph)
    '/lispy': -> new Lispy()
    '/js/dfg': -> new JSDFG()
}

default_route = ->
    <div>
        <ul>
        { _l.keys(routes).map (path) -> <li key={path}><a href={path}>{path}</a></li> }
        </ul>
    </div>

App = createReactClass
    componentWillMount: ->
        @app_state = routes[window.location.pathname]?() ? new RCRoute(default_route)
        window.ui = @app_state
        @app_state.init(this)

    componentDidMount: ->
        @app_state.did_mount?()

    render: -> @app_state.render()

ReactDOM.render <App />, document.getElementById('root')
registerServiceWorker()
