React = require 'react'
createReactClass = require 'create-react-class'
_l = require 'lodash'
{ inspect } = require 'util'
ordered_map = _l.map

export RandomReactTest = ->
    [random_dom, set_random_dom] = React.useState(null)

    if random_dom?
        <React.Fragment>
            <button children="remove dom" onClick={-> set_random_dom(null)} />
            { random_dom }
        </React.Fragment>

    else
        <button children="add dom" onClick={-> set_random_dom(create_random_dom())} />


rng = (min, max) -> min + Math.floor(Math.random() * (max - min + 1))
coin = (p) -> Math.random() <= p

create_random_dom = ->
    node_count = 0

    recurse = (max_depth) ->
        node_count += 1

        if max_depth <= 0 or coin(0.3)
            return <div>Hello world!</div>

        else
            for i in [0...rng(2, 10)]
                <div key={i} style={margin: 10, padding: 10, border: '1px solid #AAA'}>
                    {recurse(max_depth - 1)}
                </div>

    dom = recurse(4)
    console.log node_count, "nodes"
    return dom
