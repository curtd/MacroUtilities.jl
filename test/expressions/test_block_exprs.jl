@testset "Block expressions" begin 
    @testset "IfElseExpr" begin 
        ex = :(if a b end)
        f = from_expr(IfElseExpr, ex)
        @Test f.if_else_exprs[1] == (ex.args[1] => ex.args[2])
        @Test to_expr(f) == ex

        ex = :(if a b; else c end)
        f = from_expr(IfElseExpr, ex)
        @Test f.if_else_exprs[1] == (ex.args[1] => ex.args[2])
        @Test f.else_expr == ex.args[3]
        @Test to_expr(f) == ex

        ex = :(if a b; elseif c d; else e end)
        f = from_expr(IfElseExpr, ex)
        @Test f.if_else_exprs[1] == (ex.args[1] => ex.args[2])
        @Test f.if_else_exprs[2] == (ex.args[3].args[1] => ex.args[3].args[2])
        @Test f.else_expr == ex.args[3].args[3]
        @Test to_expr(f) == ex

        ex = :(if a b; elseif c d; elseif  e f; else g end)
        f = from_expr(IfElseExpr, ex)
        @Test f.if_else_exprs[1] == (ex.args[1] => ex.args[2])
        @Test f.if_else_exprs[2] == (ex.args[3].args[1] => ex.args[3].args[2])
        @Test f.if_else_exprs[3] == (ex.args[3].args[3].args[1] => ex.args[3].args[3].args[2])
        @Test f.else_expr == ex.args[3].args[3].args[3]
        @Test to_expr(f) == ex
    end 
    @testset "BlockExpr" begin
        expr = BlockExpr()
        @Test to_expr(expr) == Expr(:block)
        ex = quote a end 
        
        expr = from_expr(BlockExpr, ex)
        @Test to_expr(expr) == ex

        expr = BlockExpr(KVExpr(; lhs=:a, rhs=1))
        @Test to_expr(expr) == Expr(:block, :(a = 1))
        expr = [KVExpr(; lhs=:a, rhs=1); KVExpr(; lhs=:b, rhs=false)]
        @Test expr isa BlockExpr

        @Test to_expr(expr) == Expr(:block, :(a = 1), :(b = false))
    end
end