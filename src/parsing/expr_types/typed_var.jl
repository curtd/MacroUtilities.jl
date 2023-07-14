"""
    TypedVar(; name, type)

Matches an expression of the form `name` or `name::type`

# Fields 
- `name::Symbol`
- `type::Union{Symbol, Expr, NotProvided} = not_provided`
"""
Base.@kwdef struct TypedVar <: AbstractExpr 
    name::Symbol
    type::Union{Symbol, Expr, NotProvided} = not_provided
end

"""
    TypedVar(f::TypedVar; [name, type])

Returns a new copy of `f`, with optional `name` or `type` overridden by the keyword arguments.
"""
function TypedVar(f::TypedVar; name::Symbol=f.name, type::Union{Symbol, Expr, NotProvided}=( f.type isa Expr ? deepcopy(f.type) : f.type))
    return TypedVar(name, type)
end

function _from_expr(::Type{TypedVar}, expr)
    @switch expr begin 
        @case Expr(:(::), name, type) && if name isa Symbol && (type isa Symbol || type isa Expr) end
            return TypedVar(; name, type)
        @case ::Symbol 
            return TypedVar(; name=expr)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid field definition")
    end
end

struct_field_name(f::TypedVar) = f.name 
struct_field_type(f::TypedVar) = f.type
struct_field_default_value(::TypedVar) = not_provided

function to_expr(f::TypedVar)
    if is_not_provided(f.type)
        return f.name 
    else
        return Expr(:(::), f.name, f.type)
    end
end

const StructDefField = TypedVar