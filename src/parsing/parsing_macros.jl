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
    f = from_expr(ExprWOptionalRhs{TypedVar}, unpack_expr; throw_error=true)
    name = f.lhs.name 
    type = f.lhs.type 
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
    if is_provided(f.rhs)
        default_expr = :($name = $(f.rhs))
    else
        not_found_error = :(ArgumentError("Option `$($(QuoteNode(name)))` not found in $($(options))"))
        default_expr = should_throw ? :(throw($not_found_error)) : :(return $not_found_error)
    end
    if length(valid_types) == 1 
        unexpected_type_suffix = ", expected type $(only(valid_types))"
    else
        unexpected_type_suffix = ", expected one of types ($(join([string(T) for T in valid_types], ", ")))"
    end
    unexpected_type_error = :(used_default ? ArgumentError("Option `$($(QuoteNode(name)))` with default value `$($(name))` has type $(typeof($name))"*$unexpected_type_suffix) : ArgumentError("Option `$($(QuoteNode(name))) = $($(name))` has type $(typeof($(name)))"*$unexpected_type_suffix))
    unexpected_type_expr = should_throw ? :(throw($unexpected_type_error)) : :(return $unexpected_type_error)
    output = Base.remove_linenums!(quote 
        local used_default::Bool
        local type_is_valid = false
        if haskey($options, $(QuoteNode(name)))
            $name = $options[$(QuoteNode(name))].value
            used_default = false
        else
            $default_expr
            used_default = true
        end
        for T in $(Expr(:tuple, valid_types...))
            if $name isa T 
                type_is_valid = true 
                break
            end
        end
        !type_is_valid && $unexpected_type_expr 
        $name
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

Otherwise, returns an `ArgumentError` if `options` does not contain key `\$name`.

If `unpack_expr` is of the form 
- `name::T`, then `options[\$name]` must be of type `T` or an `ArgumentError` will be *returned*
- `name::Union{T1, T2, ..., Tn}`, then `options[\$name]` must be of type `T1, T2, ...,` or `Tn`
- `name = default`, then `default_value` will be used if `\$name` is not present in `options`

An `unpack_expr` of the form `name::T = default` or `name::Union{T1, T2, ..., Tn} = default` also behaves as expected.
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