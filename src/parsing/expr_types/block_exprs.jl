"""
    IfElseExpr(; [if_else_exprs], [else_expr])

Matches an if/if-else/if-elseif-else block

# Arguments 
- `if_else_exprs::Vector{Pair{Any,Any}} = Pair{Any, Any}[]`: Set of `condition` => `condition_expr` blocks. The first pair corresponds to the `if` statement, the rest are `elseif` statements 
- `else_expr::Any = not_provided`: The `else` statement of the block
"""
Base.@kwdef struct IfElseExpr <: AbstractExpr
    if_else_exprs::Vector{Pair{Any, Any}} = Pair{Any, Any}[]
    else_expr::Any = not_provided
end

function parse_elseif(expr)
    @switch expr begin 
        @case Expr(:elseif, cond, cond_expr, args...)
            if isempty(args)
                return cond, cond_expr, nothing 
            elseif length(args) == 1
                return cond, cond_expr, only(args)
            else
                return @arg_error expr "Not a valid elseif expression"
            end
        @case _
            return nothing, expr, nothing
    end
end

function _from_expr(::Type{IfElseExpr}, expr)
    output = Pair{Any,Any}[]
    else_expr = not_provided
    @switch expr begin 
        @case Expr(:if, cond, cond_expr, args...) 
            push!(output, Pair{Any,Any}(cond, cond_expr))
            if length(args) > 2
                @goto invalid 
            elseif length(args) == 1
                rest = only(args)
                while true 
                    y = parse_elseif(rest)
                    y isa Exception && return y 
                    next_cond, next_cond_expr, rest = y
                    if isnothing(next_cond)
                        else_expr = next_cond_expr
                        break
                    else
                        push!(output, Pair{Any,Any}(next_cond, next_cond_expr))
                        isnothing(rest) && break 
                    end
                end
            end
            return IfElseExpr(output, else_expr)
        @case _ 
            @goto invalid
    end
    @label invalid 
    return @arg_error expr "Not a valid IfElseExpr"
end

function to_expr(f::IfElseExpr)
    if !isempty(f.if_else_exprs)
        (if_cond, if_cond_expr), rest = Iterators.peel(f.if_else_exprs)
        expr = Expr(:if, to_expr_noquote(if_cond), to_expr_noquote(if_cond_expr))
        rest_of_expr = expr
        for (elseif_cond, elseif_cond_expr) in rest 
            ex = Expr(:elseif, to_expr_noquote(elseif_cond), to_expr_noquote(elseif_cond_expr))
            push!(rest_of_expr.args, ex)
            rest_of_expr = ex
        end
        if is_provided(f.else_expr)
            push!(rest_of_expr.args, to_expr_noquote(f.else_expr))
        end
        return expr
    else
        return :()
    end
end

"""
    BlockExpr(; args::Vector{Any})

Matches a `:block` expression
"""
struct BlockExpr <: AbstractExpr
    args::Vector{Any}
    function BlockExpr(args::Vector{Any})
        return new(args)
    end
end
BlockExpr() = BlockExpr(Any[])
BlockExpr(arg) = BlockExpr(Any[arg])

function BlockExpr(arg1, arg2, args...) 
    expr = BlockExpr(arg1)
    push!(expr.args, arg2)
    for arg in args 
        push!(expr.args, arg)
    end
    return expr
end

function _from_expr(::Type{BlockExpr}, expr)
    @switch expr begin 
        @case Expr(:block, args...)
            return BlockExpr(convert(Vector{Any},collect(args)))
        @case _ 
            return ArgumentError("Input expression `$expr` is not a block `Expr`")
    end
end
to_expr(expr::BlockExpr) = Expr(:block, to_expr_noquote.(expr.args)...)

Base.vcat(expr::AbstractExpr, args...) = BlockExpr(expr, args...)

Base.push!(expr::BlockExpr, arg) = push!(expr.args, arg)

