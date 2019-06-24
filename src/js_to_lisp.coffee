React = require 'react'
_l = require 'lodash'

babylon = require 'babylon'

export js_to_lispy = (js_source) ->
    js_ast = babylon.parse(js_source)
    return js_expr_to_lispy(js_ast.program)


export js_expr_to_lispy = (js) ->
    $ = js_expr_to_lispy
    $_ = (o, dfault) -> if o? then $(o) else dfault
    unknown = -> {unknown: js}

    lispy_expr = do => switch js.type
        when 'File' then $(js.program)

        # assumes straightline control flow, with a final return
        when 'Program' then ['call', [['var', '[]'], flatten_js_stmts(js.body).map(js_expr_to_lispy)...]]
        when 'BlockStatement' then stmt_list(js.body)

        # shouldn't be hit outside stmt_list.  Still...
        when 'VariableDeclaration' then stmt_list(js.declarations)
        when 'VariableDeclarator'
            return unknown() unless js.id.type == "Identifier"
            ['set', js.id.name, $_(js.init, ['lit', null])]

        when 'ExpressionStatement' then $(js.expression)
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

    # track the range in the source file
    lispy_expr.source_range = [js.start, js.end]

    return lispy_expr

flatten_js_stmts = (stmts) -> rec_flatten stmts, (stmt) ->
    switch stmt.type
        when 'VariableDeclaration' then stmt.declarations
        when 'BlockStatement' then stmt.body
        else null

stmt_list = (stmts) ->
    ['call', [['var', ';'], flatten_js_stmts(stmts).map(js_expr_to_lispy)...]]

# rec_flatten :: [A] -> (A -> Maybe [A]) -> [A]
rec_flatten = (root, replace_with) ->
    res = []
    rec = (node) ->
        replacement = replace_with(node)
        if not replacement?
            res.push(node)
        else
            rec(replacement_elem) for replacement_elem in replacement
    rec(root_elem) for root_elem in root
    return res
