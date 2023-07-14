"""
    name_only(f::NamedTupleArg) -> NamedTupleArg

Returns a `NamedTupleArg` derived from `f` with its `value` removed
"""
name_only(f::NamedTupleArg) = NamedTupleArg(f; value=not_provided)

"""
    names_only(f::NamedTupleExpr) -> NamedTupleExpr

Returns a `NamedTupleExpr` derived from `f` with each arguments' `value` removed
"""
names_only(f::NamedTupleExpr) = NamedTupleExpr(name_only.(f.args))

"""
    map_args(f, expr::FuncCall) -> FuncCall
    map_args(f, expr::FuncDef) -> FuncCall

Transform the `expr` by applying `f(FuncArg) -> FuncArg` to each of its arguments
"""
function map_args(f, expr::FuncCall)
    new_args = map(f, expr.args)::Vector{FuncArg}
    return FuncCall(expr; args=new_args)
end

"""
    map_kwargs(f, expr::FuncCall) -> FuncCall
    map_kwargs(f, expr::FuncDef) -> FuncCall

Transform the `expr` by applying `f(FuncArg) -> FuncArg` to each of its keyword arguments
"""
function map_kwargs(f, expr::FuncCall)
    new_kwarg_vals = map(f, collect(values(expr.kwargs)))::Vector{FuncArg}
    any( is_not_provided(v.name) for v in new_kwarg_vals) && throw(ArgumentError("Cannot unset `name` in keyword argument map"))
    return FuncCall(expr; kwargs=OrderedDict{Symbol,FuncArg}( v.name => v for v in new_kwarg_vals))
end

function map_args(f, expr::FuncDef)
    new_header = map_args(f, expr.header)
    return FuncDef(expr; header=new_header)
end

function map_kwargs(f, expr::FuncDef)
    new_header = map_kwargs(f, expr.header)
    return FuncDef(expr; header=new_header)
end

"""
    names_only(f::FuncCall) -> FuncCall

Returns a `FuncCall` derived from `f` with all of its types + values removed from its arguments/keyword arguments 
"""
function names_only(expr::FuncCall)
    new_args = map(name_only, expr.args)::Vector{FuncArg}
    new_kwarg_vals = map(name_only, collect(values(expr.kwargs)))::Vector{FuncArg}
    return FuncCall(expr; args=new_args, kwargs=OrderedDict{Symbol,FuncArg}( v.name => v for v in new_kwarg_vals))
end


"""
    map_fields(f, def::StructDef) -> StructDef

Apply the function `f(::TypedVar) -> TypedVar` to each field definition in `def`
"""
function map_fields(f, def::StructDef)
    _fields = map(f, first.(def.fields))::Vector{TypedVar}
    new_field_defs = Vector{Tuple{TypedVar, LineNumberNode}}()
    for ((_, lnn), new_field) in zip(def.fields, _fields)
        push!(new_field_defs, (new_field, lnn))
    end
    return StructDef(def; fields=new_field_defs)
end

"""
    map_fields(f, def::GeneralizedStructDef{A,B,C}) -> GeneralizedStructDef

Apply the function `f(::A) -> A` to each field definition in `def`
"""
function map_fields(f, def::GeneralizedStructDef{A,B,C}) where {A, B, C}
    _fields = map(f, def.fields)::Vector{A}
    new_field_defs = Vector{Tuple{A, MaybeProvided{LineNumberNode}}}()
    for ((_, lnn), new_field) in zip(getfield(def, :fields), _fields)
        push!(new_field_defs, (new_field, lnn))
    end
    return GeneralizedStructDef(def; fields=new_field_defs)
end