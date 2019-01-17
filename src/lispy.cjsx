React = require 'react'
createReactClass = require 'create-react-class'
_l = require 'lodash'
{ inspect } = require 'util'
ordered_map = _l.map

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
Record :: {
    value: Value
    expr: Expr?, scope?: Scope      # unless an arg from a call into lispy from native
    args: [Record]?                 # when .expr[0] == 'call' or not .expr?
    body: Record?                   # when .args? and .args[0].value[0] == 'cl' or .expr[0] == 'set'
    callees: [Record]?              # when .args? and .args[0].value[0] == 'nat'
}
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

active_native_record = null

# lispy_call :: (Closure|Native) -> [Value] -> Value
lispy_call = (fn, arg_values) -> lispy_record_call(fn, arg_values).value

# lispy_record_call :: (Closure|Native) -> [Value] -> Record
lispy_record_call = (fn, arg_values) ->
    # if this is a callee of a native call, add this call's record to the
    # active native call's .callees

    [parent_native_call_record, active_native_record] = [active_native_record, null]
    try

        call_record = lispy_call_internal(fn, arg_values)
        call_record.args = [fn].concat(arg_values).map((val) -> {value: val})
        parent_native_call_record?.callees.push(call_record)
        return call_record

    finally
        active_native_record = parent_native_call_record

# lispy_call :: (Closure|Native) -> [Value] -> {value: Value, body: Record?, callees: [Record]?}
lispy_call_internal = (fn, arg_values) ->
    throw new Error('called an object neither a lambda nor a native') unless _l.isArray(fn)
    return switch fn[0]
        when 'cl'
            [_cl_ty, captured_scope, [_lambdakwd, arg_names, body]] = fn
            throw new Error("called a closure made without a lambda?") if _lambdakwd != 'lambda'
            callee_scope = push_scope(captured_scope, _l.fromPairs _l.zip(arg_names, arg_values))

            body_record = lispy_eval(callee_scope, body)
            {value: body_record.value, body: body_record}

        when 'nat'
            [_nat_ty, native_impl] = fn
            active_native_record = record = {callees: []}
            try
                active_native_record.value = native_impl(arg_values...)
                return record
            # TODO control flow on excecptions: native and lispy
            finally
                active_native_record = null

        else
            throw new Error("called an object neither a lambda nor a native")

# lispy_eval :: Scope -> Expr -> Record
lispy_eval = (scope, expr) ->
    [expr_ty, params...] = expr
    record = {scope, expr}
    record.value = switch expr_ty
        when 'var'
            [varname] = params
            var_lookup(scope, varname)

        when 'lambda'
            [args, body] = params
            ['cl', scope, expr]

        when 'lit'
            [val] = params
            val

        when 'set'
            [varname, new_val_expr] = params
            record.body = lispy_eval(scope, new_val_expr)
            set_var(scope, varname, record.body.value)

            # track closures' names for debugging
            # approach: sketchily guess if it looks like a closure
            # Debugging tool idea: find all the names any object's been given, ever
            if _l.isArray(record.body.value) and record.body.value[0] == 'cl'
                (record.body.value.names ?= new Set()).add(varname)

            record.body.value

        when 'call'
            [subexprs] = params

            # postorder on the args!
            record.args = ordered_map subexprs, (subexpr) -> lispy_eval(scope, subexpr)

            [fn, arg_values...] = _l.map(record.args, 'value')
            call_record = lispy_call_internal(fn, arg_values)
            _l.extend record, call_record
            call_record.value

        else throw new Error("tried to eval a non-expr")

    return record


# fresh_root_scope :: -> Scope
window.fresh_root_scope = fresh_root_scope = ->
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
            return lispy_record_call(closure, []).body
    }

    builtin_literal_values = {
        null: null
        console
    }

    return {
        vars: _l.extend({},
            _l.mapValues(builtin_native_fns, (impl, name) -> ['nat', impl, name]),
            builtin_literal_values
        )
        parent: null,
    }

# jscall_lispy :: Closure -> JSClosure
# where JSClosure = (Value...) -> Value
window.jscall_lispy = jscall_lispy = (lispy_closure) -> (js_call_args...) ->
    lispy_call(lispy_closure, js_call_args)


# Analysis

# callee_records :: Record -> [Record]
window.callee_records = callee_records = (record) ->
    throw new Error('expected call record') unless record.args?
    switch record.args[0].value[0]
        when 'cl' then immediate_call_records(record.body)
        when 'nat' then record.callees
        else throw new Error('bad call')

window.immediate_call_records = immediate_call_records = (record) ->
    _l.flatten _l.compact [
        _l.flatMap(record.args, immediate_call_records) if record.args?
        immediate_call_records(record.body)             if record.expr?[0] == 'set'
        ([record])                                      if record.expr?[0] == 'call'
    ]



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

window.recursive_records_in_eval_order = recursive_records_in_eval_order = (record) ->
    _l.flatten _l.compact [
        _l.flatMap(record.args, recursive_records_in_eval_order) if record.args? and record.expr?
        recursive_records_in_eval_order(record.body) if record.body? and record.expr?[0] == 'set'
        [record]
        recursive_records_in_eval_order(record.body) if record.body? and record.expr?[0] != 'set'
        _l.flatMap(record.callees, (callee) ->
            [callee].concat(recursive_records_in_eval_order(callee.body))
        ) if record.callees?
    ]


## Syntax

# parse :: SourceString -> [Expr]
window.parse = parse = (str) ->
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


(cons = ((hd tl) -> ((d) -> (d hd tl))))
(car = ((p) -> (p ((hd tl) -> hd))))
(cdr = ((p) -> (p ((hd tl) -> tl))))

(p = (cons :foo :bar))
(car p)
(cdr p)
"""

# demo_parsed_lispy :: [Expr]
window.demo_parsed_lispy = demo_parsed_lispy = parse lispy_code

# all_exprs :: [Expr]
window.all_exprs = _l.flatMap(demo_parsed_lispy, all_exprs_in_source_order)

# fake_multi_expr_as_one_for_ui :: Expr
fake_multi_expr_as_one_for_ui = do ->
    iife = (expr) -> ['call', [['lambda', [], expr]]]
    list_lit = (elem_exprs) -> ['call', [['var', '[]'], elem_exprs...]]
    return iife list_lit demo_parsed_lispy

window.root_record = root_record = lispy_eval(fresh_root_scope(), fake_multi_expr_as_one_for_ui)
window.all_records = all_records = recursive_records_in_eval_order(root_record)

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
        tick_forever = (cycle_time_ms, fn) ->
            ticks = 0
            ((o) -> window.setInterval(o, cycle_time_ms)) () =>
                fn(ticks)
                ticks += 1
        cycle_through_elems_forever = ({delay, lst, fn}) ->
            tick_forever delay, (tick) => fn lst[tick % lst.length]

        # FIXME: this doesn't seem to dip into fib's recursion
        cycle_through_elems_forever({
            delay: 500
            lst: all_records.filter (record) -> record.expr?
            fn: (record) => @hl record.expr
        })


    # parsed_object is something that has a source_range, typically a token or expr
    hl: (parsed_object) ->
        {source_range} = parsed_object
        @highlight_range = (source_range ? null)
        @react_root.forceUpdate =>
            @react_root.refs.highlighted_chunk?.scrollIntoView({behavior: "smooth"})

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

        chunks_with_vals = _l.zip(chunks, root_record.value)

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
                                    <span ref="highlighted_chunk" style={
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

        handle_click_on_record = (record) =>
            window.r = record
            if record.expr?
                @hl(record.expr)

            else if record.args?[0].value[0] == 'cl'
                @hl(record.args[0].value[2])

        layout_entry = (call_record) =>
            fn = call_record.args[0].value
            callees = callee_records(call_record)

            lambda_name = switch fn[0]
                when 'nat' then fn[2]
                when 'cl'  then fn.names?.values().next().value ? '<lambda>'

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
                    onClick={-> handle_click_on_record(call_record)}
                />

            if _l.isEmpty callees
                {width: leaf_size.width, height: leaf_size.height, render: ({x, y}) ->
                    render_entry({x, y, width: leaf_size.width})
                }

            else
                children_layouts = callees.map(layout_entry)
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

        tree_layout = layout_entry(root_record)
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
