import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import registerServiceWorker from './registerServiceWorker';

const createReactClass = require('create-react-class');

const App = createReactClass({
    componentWillMount: function() {
        this.app_state = new (require('./lispy').Lispy)();
        this.app_state.init(this);
    },

    componentDidMount: function() {
        this.app_state.did_mount();
    },

    render: function() {
        return this.app_state.render()
    }
})


ReactDOM.render(<App />, document.getElementById('root'));
registerServiceWorker();
