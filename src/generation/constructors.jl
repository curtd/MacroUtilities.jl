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
    return FuncDef(; header=header, head=:(=), body=body, whereparams=whereparams)
end

"""
    kwarg_constructor(f::StructDef, default_vals; [lnn::Union{LineNumberNode, Nothing)=nothing], [whereparams=not_provided]))
"""
kwarg_constructor(f::StructDef, default_vals; kwargs...) = kwarg_constructor(f.typename, first.(f.fields), default_vals; whereparams=whereparams(f), kwargs...)


"""
    copy_constructor(typename, fields::Vector{TypedVar}; [input_var::Symbol], [lnn::Union{LineNumberNode, Nothing}], [whereparams=not_provided])

Returns a `FuncDef` copy constructor for `typename` with `fields`, i.e., a function of the form 

```julia
    \$typename(\$input_var::\$typename; field1::fieldtype1=Base.copy(getfield(\$input_var, :field1)), ...) = \$typename(field1, ...)
```
Supports an optionally provided `lnn` and `whereparams`
"""
function copy_constructor(typename, fields::Vector{TypedVar}; input_var::Symbol=gensym("x"), lnn::Union{LineNumberNode, Nothing}=nothing, whereparams=not_provided, typename_w_params=typename)
    isempty(fields) && return nothing 

    header = FuncCall(; funcname=typename, args=[FuncArg(; name=input_var, type=typename_w_params)])
    body = FuncCall(; funcname=typename)
    for field in fields 
        value = :(Base.getfield($input_var, $(QuoteNode(field.name))))
        if !(field.type === :Symbol)
            value = :(Base.copy($value))
        end
        header.kwargs[field.name] = FuncArg(; name=field.name, type=field.type, value=value)
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