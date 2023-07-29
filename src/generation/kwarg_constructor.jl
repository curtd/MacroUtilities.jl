"""
    kwarg_constructor(typename, fields::Vector{TypedVar}, default_vals)

Returns a `FuncDef` keyword argument constructor for `typename` with `fields` and `default_vals` is any collection that implements `Base.get(default_vals, fieldname, default)`
"""
function kwarg_constructor(typename, fields::Vector{TypedVar}, default_vals)
    isempty(fields) && return nothing
    header = FuncCall(; funcname=typename)
    body = FuncCall(; funcname=typename)
    for field in fields 
        default_val = get(default_vals, field.name, not_provided)
        header.kwargs[field.name] = FuncArg(field; value=is_provided(default_val) ? default_val : not_provided)
        push!(body.args, FuncArg(; name=field.name))
    end
    return FuncDef(; header=header, head=:(=), body=Expr(:block, to_expr(body)))
end

"""
    kwarg_constructor(f::StructDef, default_vals)
"""
kwarg_constructor(f::StructDef, default_vals) = kwarg_constructor(f.typename, first.(f.fields), default_vals)