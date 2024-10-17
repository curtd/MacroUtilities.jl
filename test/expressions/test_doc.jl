@testitem "DocExpr" setup=[TestSetup] begin 
    @testset "DocExpr" begin 
        ex = quote 
            """
            some doc
            """
            f(x) = 1
        end
        f = from_expr(DocExpr{FuncDef}, ex.args[2])
        @Test f.line == ex.args[2].args[2]
        @Test f.docstr == ex.args[2].args[3]
        @Test f.expr == from_expr(FuncDef, ex.args[2].args[4])
        @Test to_expr(f) == ex.args[2]

        ex = quote 
            """
            doc
            """
            const a = 1
        end
        f = from_expr(DocExpr{Any}, ex.args[2])
        @Test f.line == ex.args[2].args[2]
        @Test f.docstr == ex.args[2].args[3]
        @Test f.expr == ex.args[2].args[4]
        @Test to_expr(f) == ex.args[2]

        ex = quote 
            """
            doc
            """
            @m f(x)
        end
        f = from_expr(DocExpr{MacroCall}, ex.args[2])
        @Test f.line == ex.args[2].args[2]
        @Test f.docstr == ex.args[2].args[3]
        @Test f.expr == from_expr(MacroCall, ex.args[2].args[4])
        @Test to_expr(f) == ex.args[2]

        ex = quote 
            """
                $a
            """
            f(x) = x
        end
        f = from_expr(DocExpr{FuncDef}, ex.args[2])
        @Test f.line == ex.args[2].args[2]
        @Test f.docstr == ex.args[2].args[3]
        @Test f.expr == from_expr(FuncDef, ex.args[2].args[4])
        @Test to_expr(f) == ex.args[2]

    end
end