const allowed_kv_spec_keys = (:expected_type, :expected_types, :default)

function found_value!(parser::KVExprParser, key::Symbol, (@nospecialize value))
    allow_overwrite = parser.allow_overwrite
    found_values = parser.found_values
    if haskey(found_values, key)
        !allow_overwrite && throw(ArgumentError("Multiple values specified for key `$(key)`"))
    end
    found_values[key] = value
    return nothing
end

@inline function parse_rhs_symbol(s::Symbol)
    if s === :nothing 
        return nothing, Nothing
    elseif s === :missing 
        return missing, Missing 
    else
        return not_provided, not_provided  
    end
end

function parse_kvs!(parser::KVExprParser, exprs; strict::Bool=false)
    @nospecialize
    parser_spec = parser.spec 
    found_values = parser.found_values
    ignore_unknown_keys = parser.ignore_unknown_keys

    non_kw_exprs = Any[]
    for expr in exprs 
        _kv = from_expr(KVExpr, expr; throw_error=strict)
        if isnothing(_kv) 
            push!(non_kw_exprs, expr)
            continue
        end
        key = _kv.lhs
        spec = get(parser_spec, key, nothing)
        if isnothing(spec) 
            if strict
                !ignore_unknown_keys && throw(ArgumentError("Unexpected key `$(key)`, expected keys = ($(join(sort!(string.(collect(keys(parser_spec)))), ", ")))"))
                return nothing 
            end
            push!(non_kw_exprs, expr)
            continue
        end
        accept_any = Any in spec.expected_types
        value = _kv.rhs
        valueT = typeof(value)
        local set_value 
        if accept_any || valueT in spec.expected_types
            set_value = value
        elseif valueT === NotProvided
            set_value = key 
        elseif valueT === Symbol 
            parsed_value, parsed_valueT = parse_rhs_symbol(value)
            if is_provided(parsed_valueT)
                if accept_any || parsed_valueT in spec.expected_types
                    set_value = parsed_value 
                end
            end
        end 
        if !Base.@isdefined set_value
            if (_eltypes = eltypes(spec); !isempty(_eltypes))
                for data in _eltypes 
                    values = from_expr(Vector{data.type}, value; throw_error=false)
                    if !isnothing(values)
                        set_value = !data.is_vector ? Set(values) : values
                        break
                    end
                end
            end
        end
        if Base.@isdefined set_value
            found_value!(parser, key, set_value)
        else
            throw(ArgumentError("In `$(key) = rhs` expression, rhs (= $(value)) has type $(typeof(value)), which is not one of the expected types ($(join(string.(spec.expected_types), ", ")))"))
        end
    end
    for (key, spec) in pairs(parser_spec)
        if !haskey(found_values, key)
            is_not_provided(spec.default_value) && throw(ArgumentError("No value provided for key `$key`"))
            
            found_value!(parser, key, spec.default_value)
        end
    end
    return non_kw_exprs
end

function parse_kwargs_expr(args...; allow_overwrite::Bool=false, ignore_unknown_keys::Bool=false, _sourceinfo=nothing)
    @nospecialize
    length(args) ≥ 2 || throw(ArgumentError("Must provide at least two arguments"))
    all_args = Expr(:vect, args[1:end-1]...)

    kwarg_spec_expr = args[end]
    kwarg_spec_expr isa Expr || throw(ArgumentError("kwarg_spec must be an Expr"))

    spec_exprs = KVSpecExpr[]
    if Meta.isexpr(kwarg_spec_expr, :block)
        for expr in kwarg_spec_expr.args 
            expr isa LineNumberNode && continue
            push!(spec_exprs, from_expr(KVSpecExpr, expr; throw_error=true))
        end
    else 
        push!(spec_exprs, from_expr(KVSpecExpr, kwarg_spec_expr; throw_error=true))
    end
    output_expr = Expr(:block, [:($(spec.key) = kv_parser.found_values[$(QuoteNode(spec.key))]) for spec in spec_exprs]...)
    
    output = Expr(:block)
    if !isnothing(_sourceinfo)
        push!(output.args, _sourceinfo)
    end
    push!(output.args, :(local kv_parser = $KVExprParser( $(to_expr.(spec_exprs)...); allow_overwrite=$allow_overwrite, ignore_unknown_keys=$ignore_unknown_keys)), :(local non_parsed_exprs = $parse_kvs!(kv_parser, $all_args)), output_expr, :(non_parsed_exprs)) 
    return output
end

"""
    @parse_kwargs [args] kwarg_spec

Parses the set of keyword expressions given in `args` according to a series of `kwarg_spec` specification and sets the corresponding keys in the calling scope. Returns a `Vector` of the expressions that were not parsed as keyword arguments.

`kwarg_spec` must be a block `Expr`, with each line consisting of an expression of the form 

```julia
    key = (expected_type = T)
```

which specifies that for a key-value expression in `args` of the form `key = value`, `value` must have type `T`.

An alternative form is 

```julia
    key = (expected_types = (T1, T2, ..., Tn))
```

which specifies that a key-value expression in `args` with `key = value` must have `typeof(value) in (T1, T2, ..., Tn)`

If the `default` key is provided, e.g., 

```julia
    key = (expected_types = (T1, T2, ..., Tn), default=default_value)
```

then `key` is set to `default_value` if `args` do not contain a `key = value` expression. 

If the `default` key is not provided, then args must contain a `key = value` expression or an `ArgumentError` will be thrown. 

An alternative, more compact, form to the above expressions is
```julia
    key::Union{T1, T2, ..., Tn} = default_value
```

"""
macro parse_kwargs(args...)
    parse_kwargs_expr(args...; _sourceinfo=__source__) |> esc
end
