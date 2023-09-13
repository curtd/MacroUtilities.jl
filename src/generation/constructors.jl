"""
    kwarg_constructor(typename, fields::Vector{TypedVar}, default_vals; [lnn::Union{LineNumberNode, Nothing)=nothing], [whereparams=not_provided])

Returns a `FuncDef` keyword argument constructor for `typename` with `fields` and `default_vals` is any collection that implements `Base.get(default_vals, fieldname, default)`
"""
function kwarg_constructor(typename, fields::Vector{TypedVar}, default_vals; lnn::Union{LineNumberNode, Nothing}=nothing, whereparams=not_provided)
    isempty(fields) && return nothing
    header = FuncCall(; funcname=typename)
    body = FuncCall(; funcname=typename)
    for field in fields 
        default_val = get(default_vals, field.name, not_provided)
        header.kwargs[field.name] = FuncArg(field; value=is_provided(default_val) ? default_val : not_provided)
        push!(body.args, FuncArg(; name=field.name))
    end
    body = Expr(:block, to_expr(body))
    if !isnothing(lnn)
        pushfirst!(body.args, lnn)
    end
    return FuncDef(; header, head=:(=), body, whereparams)
end

"""
    kwarg_constructor(f::StructDef, default_vals; [lnn::Union{LineNumberNode, Nothing)=nothing], [whereparams=not_provided]))
"""
kwarg_constructor(f::StructDef, default_vals; kwargs...) = kwarg_constructor(f.typename, first.(f.fields), default_vals; whereparams=whereparams(f), kwargs...)

"""
    default_copy_expr(name, t) -> Expr

Copies the value from the input expression `t` if `typeof(t)` is not a `String`, `Symbol`, `Missing`, or `Nothing`
"""
function default_copy_expr(name, t)
    return Base.remove_linenums!(quote 
        local $name=$t
        if typeof($name) in (String, Symbol, Missing, Nothing)
            $name
        else
            Base.copy($name)
        end
    end)
end

"""
    copy_constructor(typename, fields::Vector{TypedVar}; [input_var::Symbol], [lnn::Union{LineNumberNode, Nothing}], [whereparams=not_provided], [copy_expr=default_copy_expr])

Returns a `FuncDef` copy constructor for `typename` with `fields`, i.e., a function of the form 

```julia
    \$typename(\$input_var::\$typename; field1::fieldtype1=Base.copy(getfield(\$input_var, :field1)), ...) = \$typename(field1, ...)
```
Supports an optionally provided `lnn` and `whereparams`

`copy_expr(name, input_value)` must return an `Expr` that determines how the field `name` will be copied from `input_value` 
"""
function copy_constructor(typename, fields::Vector{TypedVar}; input_var::Symbol=gensym("x"), lnn::Union{LineNumberNode, Nothing}=nothing, whereparams=not_provided, typename_w_params=typename, copy_expr=default_copy_expr)
    isempty(fields) && return nothing 

    header = FuncCall(; funcname=typename, args=[FuncArg(; name=input_var, type=typename_w_params)])
    body = FuncCall(; funcname=typename)
    for field in fields 
        header.kwargs[field.name] = FuncArg(; name=field.name, type=field.type, value=copy_expr(field.name, :(Base.getfield($input_var, $(QuoteNode(field.name))))))
        push!(body.args, FuncArg(; name=field.name))
    end
    body = Expr(:block, to_expr(body))
    if !isnothing(lnn)
        pushfirst!(body.args, lnn)
    end
    return FuncDef(; header=header, head=:(=), body=body, whereparams=whereparams)
end

"""
    copy_constructor(f::StructDef; [input_var::Symbol], [lnn::Union{LineNumberNode, Nothing}], [whereparams=not_provided])
"""
copy_constructor(f::StructDef; kwargs...) = copy_constructor(f.typename, first.(f.fields); whereparams=whereparams(f), kwargs..., typename_w_params=typename_w_params(f))