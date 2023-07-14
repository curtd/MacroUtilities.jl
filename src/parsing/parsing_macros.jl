"""
    @return_if_exception(ex)

Evalutes the destructured assignment expression `ex` of the form `(; names...) = rhs` or `(names...) = rhs` and returns from the current function if `rhs` returns an `Exception`
"""
macro return_if_exception(ex)
    y = gensym("lhs")
    if (f = from_expr(MacroCall, ex); (!isnothing(f) && (f.name == Symbol("@unpack_option") || f.name == Expr(:., :MacroUtilities, Symbol("@unpack_option")))))
        h = from_expr(ExprWOptionalRhs{TypedVar}, f.args[end]; throw_error=true)
        g = DestructuredAssigmentExpr{Any}([h.name], :eq_tuple, y)
        eval_expr = ex
    else
        f = _from_expr(DestructuredAssigmentExpr{Any}, ex)
        f isa Exception && throw(f) 
        g = DestructuredAssigmentExpr{Any}(f.lhs_names, (f.destructure_type == :eq_namedtuple ? :eq_namedtuple : :eq_tuple), y)
        eval_expr = to_expr(f.rhs)
    end
    output = Expr(:block, :(local $y = $(eval_expr)), :($(y) isa Exception && return $(y)), to_expr(g)) 
    return output |> esc
end

function unpack_option_expr(options, unpack_expr; _sourceinfo=nothing, should_throw::Bool=false)
    f = from_expr(ExprWOptionalRhs{Any}, unpack_expr; throw_error=true)
    default_value = f.rhs 
    expr = f.lhs
    if (g = from_expr(TypedVar, expr); !isnothing(g))
        name = g.name 
        new_name = g.name
        type = g.type 
    elseif (g = from_expr(TypedExpr{PairExpr{Symbol,Symbol}}, expr); !isnothing(g))
        name = g.expr.lhs
        new_name = g.expr.rhs 
        type = g.type
    elseif (g = from_expr(PairExpr{Symbol,Symbol}, expr); !isnothing(g))
        name = g.lhs 
        new_name = g.rhs 
        type = not_provided
    else
        throw(@arg_error unpack_expr "Not a valid `@unpack_option` expression")
    end

    valid_types = Any[]
    if is_not_provided(type)
        push!(valid_types, Any)
    elseif (t = from_expr(UnionExpr, type); !isnothing(t))
        for arg in t.args 
            push!(valid_types, arg)
        end
    else
        push!(valid_types, type)
    end
    if is_provided(default_value)
        default_expr = :($new_name = $(default_value))
    else
        not_found_error = :(ArgumentError("Option `$($(QuoteNode(name)))` not found in $($(options))"))
        default_expr = should_throw ? :(throw($not_found_error)) : :(return $not_found_error)
    end
    if length(valid_types) == 1 
        unexpected_type_suffix = ", expected type $(only(valid_types))"
    else
        unexpected_type_suffix = ", expected one of types ($(join([string(T) for T in valid_types], ", ")))"
    end
    unexpected_type_error_from_options = :(ArgumentError("Option `$($(QuoteNode(name))) = $($(new_name))` has type $(typeof($(new_name)))"*$unexpected_type_suffix))
    unexpected_type_error_from_default = :(ArgumentError("Option `$($(QuoteNode(name)))` with default value `$($(new_name))` has type $(typeof($new_name))"*$unexpected_type_suffix))
    output = Base.remove_linenums!(quote 
        if haskey($options, $(QuoteNode(name)))
            $new_name = MacroUtilities.unwrap_value($options[$(QuoteNode(name))])
            if !($new_name isa Union{$(valid_types...)} )
                $(should_throw ? :(throw($unexpected_type_error_from_options)) : :(return $unexpected_type_error_from_options))
                $unexpected_type_error_from_options
            else
                $new_name
            end
        else
            $default_expr
            if !($new_name isa Union{$(valid_types...)} ) 
                $(should_throw ? :(throw($unexpected_type_error_from_default)) : :(return $unexpected_type_error_from_default))
                $unexpected_type_error_from_default
            else 
                $new_name
            end
        end
    end)
    pushfirst!(output.args, _sourceinfo)
    return output
end

"""
    @unpack_option [should_throw=false] options unpack_expr
    
Unpacks a value from `options` depending on the form of `unpack_expr`. 

If `unpack_expr` is of the form `name`, with `name::Symbol`, then this macro is 
equivalent to writing 

```julia 
\$name = options[\$name]
```

If `should_throw == true`, throws an `ArgumentError` if `options` does not contain key `\$name`.

Otherwise, returns an `ArgumentError` from the current function if `options` does not contain key `\$name`.

If `unpack_expr` is of the form 
- `name::T`, then `options[\$name]` must be of type `T` or an `ArgumentError` will be *returned*
- `name::Union{T1, T2, ..., Tn}`, then `options[\$name]` must be of type `T1, T2, ...,` or `Tn`
- `name = default`, then `default_value` will be used if `\$name` is not present in `options`
- `name => new_name`, is the equivalent to `\$new_name = options[\$name]`

An `unpack_expr` of the form `(name => new_name)::T`, `name::T = default` or `name::Union{T1, T2, ..., Tn} = default`, etc., also behaves as expected.
"""
macro unpack_option(options, unpack_expr)
   return unpack_option_expr(options, unpack_expr; _sourceinfo=__source__) |> esc
end

macro unpack_option(should_throw, options, unpack_expr)
    f = from_expr(ExprWOptionalRhs{Symbol}, should_throw; throw_error=true)
    f.lhs === :should_throw || throw(@arg_error should_throw "LHS must be `should_throw`")
    f.rhs isa Bool || throw(@arg_error should_throw ", LHS must be a `Bool`")
    return unpack_option_expr(options, unpack_expr; _sourceinfo=__source__, should_throw=f.rhs) |> esc
 end