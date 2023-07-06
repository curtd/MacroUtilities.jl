"""
    IdentityMap()(x) -> x 

Maps `x` to itself
"""
struct IdentityMap end 

(::IdentityMap)(x) = x 

"""
    TakeLastMap()(args...) -> args[end]

Maps the list of input arguments to the last one
"""
struct TakeLastMap end 

(::TakeLastMap)(args...) = args[end]

abstract type AbstractExpr end

"""
    BlockExpr(; args)

Matches a `:block` expression
"""
struct BlockExpr <: AbstractExpr
    args::Vector{Any}
    function BlockExpr(args::Vector{Any})
        return new(args)
    end
end

BlockExpr(arg) = BlockExpr(Any[arg])

function BlockExpr(arg1, arg2, args...) 
    expr = BlockExpr(arg1)
    push!(expr.args, arg2)
    for arg in args 
        push!(expr.args, arg)
    end
    return expr
end

function Base.show(io::IO, b::BlockExpr)
    print(io, "BlockExpr - ", to_expr(b))
end

function _from_expr(::Type{BlockExpr}, expr)
    @switch expr begin 
        @case Expr(:block, args...)
            return BlockExpr(convert(Vector{Any},collect(args)))
        @case _ 
            return ArgumentError("Input expression `$expr` is not a block `Expr`")
    end
end
to_expr(expr::BlockExpr) = Expr(:block, to_expr.(expr.args)...)

Base.vcat(expr::AbstractExpr, args...) = BlockExpr(expr, args...)

"""
    AssignExpr(; lhs, rhs, allow_kw)

Matches a `lhs = rhs` assignment expression. 

If `allow_key == true`, also matches an assignment expression with head `:kw`
"""
Base.@kwdef struct AssignExpr <: AbstractExpr
    lhs::Any 
    rhs::Any
    allow_kw::Bool
end

function _from_expr(::Type{AssignExpr}, expr; allow_kw::Bool=true)
    @switch expr begin 
        @case Expr(:(=), lhs, rhs) || (Expr(:kw, lhs, rhs) && if allow_kw end)
            return AssignExpr(; lhs, rhs, allow_kw)
        @case _ 
            return ArgumentError("Input expression `$expr` is not of the form `lhs = rhs`")
    end
end 

function to_expr(expr::AssignExpr)
    return Expr(expr.allow_kw ? :kw : :(=), to_expr_noquote(expr.lhs), to_expr(expr.rhs))
end

"""
    KVExpr(; key, value)

Matches expressions of the form `key::Symbol = value`
"""
Base.@kwdef struct KVExpr <: AbstractExpr
    key::Symbol
    value::Any
end

function _from_expr(::Type{KVExpr}, expr)
    @switch expr begin 
        @case (Expr(:(=), key, value) || Expr(:kw, key, value)) && if key isa Symbol end 
            return KVExpr(; key, value)
        @case _ 
            return ArgumentError("Expression `$expr` is not of the form `key = value`")
    end
end

to_expr(k::KVExpr) = Expr(:(=), k.key, to_expr(k.value))