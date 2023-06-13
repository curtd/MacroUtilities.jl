function first_lnn_in_block(expr::Expr)
    if Meta.isexpr(expr, :block)
        for arg in expr.args 
            if arg isa LineNumberNode
                return arg 
            end
        end
    end
    return nothing 
end
first_lnn_in_block(x) = nothing