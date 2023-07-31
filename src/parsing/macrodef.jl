Base.@kwdef struct MacroDef <: AbstractExpr 
    header::FuncCall
    body::BlockExpr
end

function _from_expr(::Type{MacroDef}, expr)
    if Meta.isexpr(expr, :macro) && length(expr.args) == 2
        @return_if_exception header = _from_expr(FuncCall, expr.args[1])
        @return_if_exception body = _from_expr(BlockExpr, expr.args[2])
        return MacroDef(header, body)
    else
        return @arg_error expr "Not a valid `MacroDef` expression"
    end
end

to_expr(f::MacroDef) = Expr(:macro, to_expr(f.header), to_expr_noquote(f.body))