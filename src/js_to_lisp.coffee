React = require 'react'
_l = require 'lodash'

babylon = require 'babylon'

export js_to_lispy = (js) ->
    $ = js_to_lispy
    $_ = (o, dfault) -> if o? then $(o) else dfault
    unknown = -> {unknown: js}
    switch js.type
        when 'File' then $(js.program)

        # assumes straightline control flow, with a final return
        when 'Program' then ['call', [['var', ';'], js.body.map($)...]]

        when 'BlockStatement' then ['call', [['var', ';'], js.body.map($)...]]
        when 'VariableDeclaration' then ['call', [['var', ';'], js.declarations.map($)...]]
        when 'ExpressionStatement' then $(js.expression)
        when 'VariableDeclarator'
            return unknown() unless js.id.type == "Identifier"
            ['set', js.id.name, $_(js.init, ['lit', null])]
        when 'ReturnStatement' then $(js.argument)

        when 'Identifier' then ['var', js.name]
        when 'BinaryExpression'
            ['call', [['var', "js/#{js.operator}"], $(js.left), $(js.right)]]
        when 'StringLiteral' then ['lit', js.value]
        when 'ArrayExpression' then ['call', [['var', '[]'], js.elements.map($)...]]

        when 'FunctionDeclaration', 'FunctionExpression', 'ArrowFunctionExpression'
        # `=>` have not quite the right semantics, but we expect these to be removed in -> ES5 pass

            # assumes params are simple
            fn = ['lambda', _l.map(js.params, 'name'), $(js.body)]
            return fn unless js.id
            return ['set', js.id.name, fn] if js.id

        when 'CallExpression'
            if js.callee.type == 'MemberExpression'
                ['call', [
                    ['var', 'js/.()']
                    $(js.callee.object)
                    ['lit', js.callee.property.name]
                    ['call', [['var', '[]'], js.arguments.map($)...]]
                ]]
            else
                ['call', [$(js.callee), js.arguments.map($)...]]

        else unknown()