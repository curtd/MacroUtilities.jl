
"""
    NestedDotExpr(; keys::Vector{Symbol})

Matches expressions of the form `A.B.C....`, with each symbol being stored in `keys`
"""
Base.@kwdef struct NestedDotExpr <: AbstractExpr
    keys::Vector{Symbol}
end

function _parse_dotted_expr!(result::Vector{Symbol}, expr)
    @switch expr begin 
        @case ::Symbol 
            push!(result, expr)
            return nothing
        @case ::QuoteNode && if expr.value isa Symbol end 
            push!(result, expr.value)
            return nothing
        @case Expr(:., arg1, arg2) && if arg2 isa QuoteNode end
            returned = _parse_dotted_expr!(result, arg1)
            returned isa Exception && return returned
            push!(result, arg2.value)
            return nothing
        @case _
            return ArgumentError("Expression $expr is an invalid dotted expression")
    end
end

function _from_expr(::Type{NestedDotExpr}, expr)
    if Meta.isexpr(expr, :.)
        result = Symbol[]
        returned = _parse_dotted_expr!(result, expr)
        returned isa Exception && return returned 
        return NestedDotExpr(result)
    else
        return ArgumentError("Input expression `$expr` is not a dot expression")
    end
end

function to_expr(f::NestedDotExpr)
    key1, rest = Iterators.peel(f.keys)
    key2, rest = Iterators.peel(rest)
    output = Expr(:., key1, QuoteNode(key2))
    for key in rest
        output = Expr(:., output, QuoteNode(key))
    end
    return output
end