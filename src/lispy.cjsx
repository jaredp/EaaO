React = require 'react'
createReactClass = require 'create-react-class'
_l = require 'lodash'
{ inspect } = require 'util'

###
Var :: String
Scope :: {parent: Scope|null, vars: {Var: Value}}
Expr :: (Lambda = ['lambda', [Var], Expr])
      | ['call',    [Expr]]     # first arg is the closure or native to call
      | ['var',     Var]
      | ['lit',     LitValue]
      | ['set',     Var, Expr]
Closure :: ['cl', Scope, Lambda, names: Set(String)?]
Native  :: ['nat', (Value...) -> Value, String?] # the string at the end is an optional human-readable label
Callable :: Closure | Native
LitValue :: Number | String # note the underlying JS implementations are immutable
Value :: Number | String     # aka LitValue.
       | Closure | Native    # aka Callable. first class functions.
       | Expr                # first class code rewriting.  think S-expr
       | JSObject            # any valid Javascript object can be passed around as a black box
                             # Note these are not currently boxed.
###

## Scope

# var_lookup :: Scope -> Var -> Value
var_lookup = (scope, varname) ->
    throw new Error("invalid var #{varname}") if scope == null
    return scope.vars[varname] if varname of scope.vars
    return var_lookup(scope.parent, varname)

# set_var :: Scope -> Var -> Value -> ()
set_var = (scope, varname, new_value) ->
    throw new Error("how'd you get here??") if scope == null

    # only sets vars already in scope.  Returns if the var was in scope.
    set_existing_var = (scope, varname, new_value) ->
        return false if scope == null
        if varname of scope.vars
            scope.vars[varname] = new_value
            return true
        else
            return set_existing_var(scope.parent, varname, new_value)

    var_existed = set_existing_var(scope, varname, new_value)
    if not var_existed
        # create the var in this scope
        scope.vars[varname] = new_value

push_scope = (parent, vars) -> {parent, vars}

## Interpreter

window.call_records = call_records = []
clear_call_records = ->
    call_records.splice(0, call_records.length)
clear_call_records()
interpreter_stack = []

# lispy_call :: (Closure|Native) -> [Value] -> Value
lispy_call = (fn, arg_values) ->
    cr = {fn, args: arg_values, stack: interpreter_stack.slice(), callees: []}
    call_records.push(cr)
    _l.last(interpreter_stack).callees.push(cr)
    interpreter_stack.push(cr)

    try
        throw new Error('called an object neither a lambda nor a native') unless _l.isArray(fn)
        retval = switch fn[0]
            when 'cl'
                [_cl_ty, captured_scope, [_lambdakwd, arg_names, body]] = fn
                throw new Error("called a closure made without a lambda?") if _lambdakwd != 'lambda'
                callee_scope = push_scope(captured_scope, _l.fromPairs _l.zip(arg_names, arg_values))

                # record the scope for future introspection
                cr.scope = callee_scope

                lispy_eval(callee_scope, body)

            when 'nat'
                [_nat_ty, native_impl] = fn
                native_impl(arg_values...)

            else
                throw new Error("called an object neither a lambda nor a native")

        cr.retval = retval
        return retval

    finally
        interpreter_stack.pop()
        arrays_equal = (a, b) -> a.length == b.length and _l.every [0...a.length], (i) -> a[i] == b[i]
        throw new Error("corrupted stack") unless arrays_equal(interpreter_stack, cr.stack)

# eval :: Scope -> Expr -> Value
lispy_eval = (scope, expr) ->
    [expr_ty, params...] = expr
    switch expr_ty
        when 'var'
            [varname] = params
            var_lookup(scope, varname)

        when 'lambda'
            [args, body] = params
            ['cl', scope, expr]

        when 'lit'
            [val] = params
            val

        when 'call'
            [subexprs] = params

            # postorder on the args!
            subexpr_vals = []
            for subexpr in subexprs
                val = lispy_eval(scope, subexpr)
                subexpr_vals.push(val)

            [fn, arg_vals...] = subexpr_vals
            return lispy_call(subexpr_vals[0], subexpr_vals.slice(1))

        when 'set'
            [varname, new_val_expr] = params
            new_value = lispy_eval(scope, new_val_expr)
            set_var(scope, varname, new_value)

            # track closures' names for debugging
            # approach: sketchily guess if it looks like a closure
            # Debugging tool idea: find all the names any object's been given, ever
            if _l.isArray(new_value) and new_value[0] == 'cl'
                (new_value.names ?= new Set()).add(varname)

            return new_value

        else throw new Error("tried to eval a non-expr")

# Analysis


immediate_subexprs_for_expr = (expr) ->
    switch expr[0]
        when 'var'    then []
        when 'lambda' then [expr[2]]
        when 'lit'    then []
        when 'call'   then expr[1]
        when 'set'    then [expr[2]]
        else throw new Error("expected an expr")


# all_exprs_in_source_order :: Expr -> [Expr]
# returns subexprs + original expr in order of their start in the source
window.all_exprs_in_source_order = all_exprs_in_source_order = (expr) ->
    [expr].concat _l.flatMap(immediate_subexprs_for_expr(expr), all_exprs_in_source_order)

window.all_exprs_in_eval_order = all_exprs_in_eval_order = (expr) ->
    _l.flatMap(immediate_subexprs_for_expr(expr), all_exprs_in_eval_order).concat [expr]

##

# builtin_native_fns :: {Var: (Value...) -> Value}
builtin_native_fns = {
    '+':  (a, b) -> a + b
    '-':  (a, b) -> a - b
    '*':  (a, b) -> a * b
    '/':  (a, b) -> a / b
    '<':  (a, b) -> a < b
    '<=': (a, b) -> a <= b
    '>':  (a, b) -> a > b
    '>=': (a, b) -> a >= b
    '==': (a, b) -> a == b
    '++': (a, b) -> a.concat(b)

    'if': (pred, if_true, if_false) ->
        winning_branch = if pred == true then if_true else if_false
        lispy_call(winning_branch, [])

    '[]': (args...) -> args
    call: (fn, args) -> lispy_call(fn, args)

    # naturally goes with `set!`; relies on order of evaluation
    ';': (stmts...) -> _l.last(stmts)

    str_to_int: (str) -> Number(str)
    range: (n) -> [0...n]
    map: (lst, fn) -> lst.map (e) -> lispy_call(fn, [e])
    '.': (o, member) -> o[member]
    '@': (o, method, args...) -> o[method](args...)
    '{}': (kvpairs...) ->
        if kvpairs.length % 2 != 0
            throw new Error("mismatched key/value pairs; is a key missing its value?")
        _l.fromPairs(_l.chunk(kvpairs, 2))

    record: (closure) ->
        lispy_call(closure, [])
        return _l.last(interpreter_stack).callees[0].callees
}

builtin_literal_values = {
    null: null
    console
}

# lispy_common_root_scope :: Scope
window.lcrs = lispy_common_root_scope = {
    parent: null,
    vars: _l.extend({},
        _l.mapValues(builtin_native_fns, (impl, name) -> ['nat', impl, name]),
        builtin_literal_values
    )
}

# lispy_run :: [Expr] -> [Value]
window.lispy_run = lispy_run = (exprs) ->
    clear_call_records()

    cr = {stack: [], args: [], fn: ['nat', (() -> null), 'eval-root'], callees: []}
    interpreter_stack.push(cr)

    try
        eval_scope = push_scope(lispy_common_root_scope, {})
        in_order_map = _l.map
        retvals = in_order_map exprs, (e) -> lispy_eval(eval_scope, e)

        return {retvals, record: cr}

    finally
        interpreter_stack.pop()
        throw new Error("corrupted stack") unless _l.isEmpty(interpreter_stack)

# jscall_lispy :: Closure -> JSClosure
# where JSClosure = (Value...) -> Value
window.jscall_lispy = jscall_lispy = (lispy_closure) -> (js_call_args...) ->
    lispy_call(lispy_closure, js_call_args)

## Syntax

# parse :: SourceString -> [Expr]
parse = (str) ->
    tokens = tokenize_source_str(str)
    asts = parse_asts_for_exprs(tokens)
    return asts.map(ast_to_expr)


# tokenize_source_str :: SourceString -> [Token]
# Token :: '(' | ')' | String | [LITERAL_TOKEN, LitValue]
LITERAL_TOKEN = {LITERAL_TOKEN: yes}
window.tokenize_source_str = tokenize_source_str = (str) ->
    MATCHERS = [
        [/^\(/, -> ['(']]
        [/^\)/, -> [')']]
        [/^\d+/, ([tok]) -> [[LITERAL_TOKEN, Number(tok)]]]                   # int literal
        [/^\"([^"\\]|\\.)*\"/, ([tok]) -> [[LITERAL_TOKEN, JSON.parse(tok)]]] # double qouted string literal
        [/^:([^\s\(\)]+)/, ([tok, cap]) -> [[LITERAL_TOKEN, cap]]]            # :symbol style string liteal
        [/^[^\s\(\)]+/, ([tok]) -> [tok]]
        [/^\s+/, -> []]
    ]

    token_stream = []
    cursor = 0
    while cursor < str.length
        for [regex_pattern, handler] in MATCHERS
            match = str.slice(cursor).match(regex_pattern)
            continue if match == null
            [cursor, token_start] = [cursor + match[0].length, cursor]
            # For now, we only make 0 or 1 token per match.  If we do more, we should have
            # more specific source ranges
            source_range = [token_start, cursor]
            token_stream.push({token, source_range}) for token in handler(match)
            # start over from the beginning for token matches
            break

    return token_stream

window.source_index_to_line_column = source_index_to_line_column = (cursor, source) ->
    prefix = source.slice(0, cursor)
    prefix_lines = prefix.split('\n')
    {line: prefix_lines.length, col: _l.last(prefix_lines).length}


window.source_range_to_lines_and_columns = source_range_to_lines_and_columns = ([start, end], source) ->
    return {
        start: source_index_to_line_column(start, source)
        end: source_index_to_line_column(end, source)
    }

# parse_asts_for_exprs :: [Token] -> [AST]
# AST :: [AST] | ['term', String] | ['term', [LITERAL_TOKEN, any]]
parse_asts_for_exprs = (tokens) ->
    stack = [[]]

    for {token, source_range} in tokens
        switch token
            when '('
                newexpr = []
                newexpr.source_range = [source_range[0], null] # fill in the end later
                stack.push(newexpr)

            when ')'
                throw new Error("mismatched parens: too many closed") if stack.length < 2

                completed = stack.pop()
                completed.source_range[1] = source_range[1]
                _l.last(stack).push(completed)

            else
                # _l.last(stack).push(token)
                term = ['term', token]
                term.source_range = source_range
                _l.last(stack).push(term)

    throw new Error("mismatched parens: too many open") unless stack.length == 1
    return stack[0]

# ast_to_expr :: AST -> Expr
ast_to_expr = (ast) ->
    # This super duper needs pattern matching, and robust error reporting
    is_term = (ast) -> ast.length == 2 and ast[0] == 'term'
    is_term_lit = (ast, str) -> is_term(ast) and ast[1] == str
    expr =
        if      ast.length == 2 and ast[0] == 'term' and _l.isString(ast[1]) then ['var', ast[1]]
        else if ast.length == 2 and ast[0] == 'term' and ast[1][0] == LITERAL_TOKEN then ['lit', ast[1][1]]
        else if ast.length == 3 and is_term_lit(ast[1], '->') then ['lambda', _l.map(ast[0], 1), ast_to_expr(ast[2])]
        else if ast.length == 3 and is_term_lit(ast[1], '=') then ['set', ast[0][1], ast_to_expr(ast[2])]
        else ['call', ast.map(ast_to_expr)]

    expr.source_range = ast.source_range
    return expr

# ppexpr :: Expr -> SourceString
window.ppexpr = ppexpr = (expr) ->
    # oneline = JSON.stringify(expr)
    # return oneline unless oneline.length > 80
    is_oneliner = (line) -> line.length < 50 and '\n' not in line
    indent_line = (line) -> "   " + line
    indent = (str) -> _l.join(str.split('\n').map(indent_line), '\n')

    switch expr[0]
        when 'var'
            expr[1]

        when 'lambda'
            args = expr[1].join(' ')
            body = ppexpr expr[2]
            oneline = "((#{args}) -> #{body})"
            return oneline if is_oneliner(oneline)
            """
            ((#{args}) ->
            #{indent body}
            )
            """

        when 'call'
            subexprs = expr[1].map(ppexpr)
            oneline = "(#{subexprs.join(' ')})"
            return oneline if is_oneliner(oneline)

            if expr[1][0][0] == 'var'
                return """
                    (#{subexprs[0]}
                    #{indent subexprs.slice(1).join('\n')}
                    )
                """

            """
            (
            #{indent subexprs.join('\n')}
            )
            """

        when 'lit'
            try
                JSON.stringify(expr[1])
            catch
                "<lit>"

        when 'set'
            "(#{expr[1]} = #{ppexpr expr[2]}"


##

window.lispy_code = lispy_code = """
(main = ((arg) -> (;
    (local = 4)
    (other = 5)
    (sum = (+ local arg))
    (product = (* other arg))
    ([] arg product local)
)))

(main 6)

(fib = ((n) ->
    (if (<= n 2)
        (() -> 1)
        (() -> (+
            (fib (- n 1))
            (fib (- n 2))
        ))
    )
))
(simple_object = ({}
    :foo "simple object #1"
    :baz 42
    :nested ([]
        ({} :a "qoux")
        ({} :b "nax")
    )
))
(@ console :log "foo" "Bar")
(++ :foo "Bar")
(++ "fo\\"o" :Bar)
(++ ([] 1) ([] 2))
(. simple_object :nested)
(map (range 12) fib)
(record (() -> (fib 4)))
"""

window.demo_parsed_lispy = demo_parsed_lispy = parse lispy_code
window.all_exprs = _l.flatMap(demo_parsed_lispy, all_exprs_in_source_order)
window.eval_results = eval_results = lispy_run(demo_parsed_lispy)

##

exports.Lispy = class Lispy
    init: (@react_root) ->
        window.ui = this
        @highlight_range = null # {start: {line: int, col: int}, end: {line: int, col: int}}

        # safely use window.innerWidth in render()
        window.addEventListener 'resize', => @react_root.forceUpdate()

    did_mount: ->
        @timelineDidMount()

    cycle_highlight_through_exprs: ->
        exprs = call_records.filter ({fn: [ty]}) -> ty == 'cl'
        tick_forever = (cycle_time_ms, fn) ->
            ticks = 0
            ((o) -> window.setInterval(o, cycle_time_ms)) () =>
                fn(ticks)
                ticks += 1
        index_in_inf_cycle = (lst, idx) -> lst[idx % lst.length]
        tick_forever 500, (tick) => @hl index_in_inf_cycle(exprs, tick).fn[2]


    # parsed_object is something that has a source_range, typically a token or expr
    hl: (parsed_object) ->
        {source_range} = parsed_object
        @highlight_range = (source_range ? null)
        @react_root.forceUpdate()

    unhighlight: ->
        @highlight_range = null
        @react_root.forceUpdate()

    be_safe: yes

    render: ->
        [margin, padding] = [20, 10]
        ppjson = (json) ->
            try
                JSON.stringify(json, null, '    ')
            catch e
                inspect(json)
                # "<Can't pp: #{e}>"

        pane_style = {
            padding
            display: 'block'
            backgroundColor: '#EFEFEF'
            borderRadius: 5
            whiteSpace: 'pre'
            fontSize: 14
            overflow: 'auto'
        }

        pane = ({contents, style}) ->
            <code style={_l.extend({}, pane_style, style)}>
                { contents }
            </code>

        chunk_delimiters = _l.map(demo_parsed_lispy, 'source_range.1').slice(0, -1)
        chunk_ranges = _l.zip([0].concat(chunk_delimiters), chunk_delimiters.concat([lispy_code.length]))
        chunks = chunk_ranges.map ([start, end]) -> [start, end, lispy_code.slice(start, end)]

        chunks_with_vals = _l.zip(chunks, eval_results.retvals)

        panes = <div>
            { chunks_with_vals.map ([[chunk_start, chunk_end, chunk_source_code], eval_result], i) =>
                while chunk_source_code[0] == '\n'
                    chunk_start += 1
                    chunk_source_code = chunk_source_code.slice(1)

                range_intersection = ([s1, e1], [s2, e2]) -> [Math.max(s1, s2), Math.min(e1, e2)]
                range_size = ([start, end]) -> Math.max(0, end - start)
                ranges_overlap = (r1, r2) -> range_size(range_intersection(r1, r2)) > 0
                range_offset = ([start, end], offset) -> [start + offset, end + offset]

                chunk_range = [chunk_start, chunk_end]
                chunk_highlight =
                    if @highlight_range and ranges_overlap(@highlight_range, chunk_range)
                        range_offset (range_intersection @highlight_range, chunk_range), -chunk_start
                    else null

                <div key={i} style={{
                    margin
                    # minHeight: "calc(100vh - 2*#{margin}px)"
                    display: 'flex'
                    flexDirection: 'row'
                    flex: '1 1'
                }}>
                    <code
                        style={_l.extend({}, pane_style, flex: 1)}
                        onClick={(e) =>
                            cursor_in_chunk = caret_in_dom_text_for_evt({evt: e, is_root_container: (dom) -> dom.tagName == 'CODE'})
                            (@unhighlight(); return) unless cursor_in_chunk?

                            cursor = chunk_start + cursor_in_chunk
                            tokens = tokenize_source_str(lispy_code)
                            clicked_token = _l.find tokens, ({source_range: [start, end]}) -> start <= cursor <= end

                            (@unhighlight(); return) unless clicked_token?
                            @hl(clicked_token)
                        }
                    >
                        {
                            if chunk_highlight == null
                                chunk_source_code

                            else
                                [highlight_start, highlight_end] = chunk_highlight
                                prefix      = chunk_source_code.slice(0, highlight_start)
                                highlighted = chunk_source_code.slice(highlight_start, highlight_end)
                                postfix     = chunk_source_code.slice(highlight_end)
                                <React.Fragment>
                                    {prefix}
                                    <span style={
                                        backgroundColor: '#bbbbf7'
                                        margin: '-2px -5px'
                                        padding: '2px 5px'
                                        borderRadius: 3
                                    }>
                                        {highlighted}
                                    </span>
                                    {postfix}
                                </React.Fragment>
                        }
                    </code>

                    <div style={width: margin} />

                    {pane({
                        style:
                            flex: 1
                        contents:
                            <div key={i}>
                                {ppjson(eval_result)}
                            </div>
                    })}
                </div>
            }
        </div>

        rk = (key) => (children) => <React.Fragment key={key} children={children} />

        layout_entry = (cr) =>
            lambda_name = switch cr.fn[0]
                when 'nat' then cr.fn[2]
                when 'cl'  then cr.fn.names?.values().next().value ? '<lambda>'

            leaf_size = {width: 80, height: 22}
            entry_spacing = {x: 3, y: 3}

            render_entry = ({x, y, width}) =>
                padding_size = 3
                height = leaf_size.height
                <div
                    children={lambda_name}
                    style={{
                        boxModel: 'border-box'
                        top: y, left: x,
                        width: width - 8, height: height - 8,
                        position: 'absolute',
                        backgroundColor: '#EEE', border: '2px solid #AAA'
                        padding: padding_size
                        fontFamily: 'monospace'
                        fontSize: 14
                    }}
                    onClick={=>
                        if cr.fn[0] == 'cl'
                            @hl(cr.fn[2])
                    }
                />

            if _l.isEmpty cr.callees
                {width: leaf_size.width, height: leaf_size.height, render: ({x, y}) ->
                    render_entry({x, y, width: leaf_size.width})
                }
            else
                children_layouts = cr.callees.map(layout_entry)
                width =  _l.sum(_l.map(children_layouts, 'width'))  + (children_layouts.length - 1) * entry_spacing.x
                height = _l.max(_l.map(children_layouts, 'height')) + leaf_size.height + entry_spacing.y
                { width, height, render: ({x, y}) ->
                    <React.Fragment>
                        {children_layouts.map (child, i) =>
                            rk(i) child.render({
                                x: x + _l.sum(_l.map(children_layouts, 'width').slice(0, i))  + i * entry_spacing.x
                                y: y + (leaf_size.height + entry_spacing.y)
                            })
                        }
                        {render_entry({x, y, width})}
                    </React.Fragment>
                }

        tree_layout = layout_entry(eval_results.record)
        timeline =
            <div
                style={
                    height: tree_layout.height + 20
                    width:  tree_layout.width + 20
                    position: 'relative'
                }
                children={tree_layout.render({x: 10, y: 10})}
            />


        @timelineDidMount = =>

        <div style={height: '100vh', display: 'flex', flexDirection: 'column'}>
            <div style={overflow: 'scroll', height: 250, borderBottom: '1px solid #bbbbbb'}>
                { timeline }
            </div>
            <div style={overflow: 'scroll', flex: 1}>
                { panes }
            </div>
        </div>


caret_in_dom_text_for_evt = ({evt, is_root_container}) ->
    dom_caret = document.caretRangeFromPoint(evt.clientX, evt.clientY)
    return null unless (
        dom_caret.startContainer == dom_caret.endContainer \
        and dom_caret.startContainer.nodeType == document.TEXT_NODE
    )

    cursor = dom_caret.startOffset
    dom_container = dom_caret.startContainer

    until dom_container.previousSibling == null and is_root_container(dom_container.parentNode)
        if dom_container.previousSibling
            dom_container = dom_container.previousSibling

        else
            dom_container = dom_container.parentNode.previousSibling

        cursor += dom_container.textContent.length

    return cursor
