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
            return ArgumentError("Input expression `$expr` is not a valid typed variable")
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

"""
    TypedExpr{E}(; expr::E, type::Union{Symbol, Expr, NotProvided})

Matches an expression of the form `expr` or `expr::type`
"""
Base.@kwdef struct TypedExpr{E} <: AbstractExpr 
    expr::E
    type::Union{Symbol, Expr, NotProvided} = not_provided
end

"""
    TypedExpr(f::TypedExpr; [expr, type])

Returns a new copy of `f`, with optional `expr` or `type` overridden by the keyword arguments.
"""
function TypedExpr(f::TypedExpr{E}; expr::E=copy(f.expr), type::Union{Symbol, Expr, NotProvided}=( f.type isa Expr ? copy(f.type) : f.type)) where {E}
    return TypedExpr(expr, type)
end

function _from_expr(::Type{TypedExpr{E}}, expr) where {E}
    @switch expr begin 
        @case Expr(:(::), inner, type) && if (type isa Symbol || type isa Expr) end
            ex = _from_expr(E, inner)
            ex isa Exception && return ex 
            return TypedExpr(ex, type)
        @case _ 
            ex = _from_expr(E, expr)
            ex isa Exception && return ex         
            return TypedExpr(ex, not_provided)
    end
end

function to_expr(f::TypedExpr)
    if is_not_provided(f.type)
        return to_expr_noquote(f.expr)
    else
        return Expr(:(::), to_expr_noquote(f.expr), f.type)
    end
end