macro ex_macro4(ex)
    f = from_expr(FuncDef, ex)
    return to_expr(__doc__macro(f)) |> esc 
end

"""
    some docs
"""
@ex_macro4 ex_macro4_func(x) = 1

@testset "Macro parsing" begin 
    @testset "MacroCall" begin 
        @test_cases begin 
            input                                   | output 
            :x                                      | nothing
            Expr(:macrocall, Symbol("@a"), nothing) | MacroCall(; name=Symbol("@a"), line=MacroUtilities.not_provided, args=[])
            Expr(:macrocall, Expr(:., :Mod, Symbol("@a")), LineNumberNode(1, :a), :b, :(f(x))) | MacroCall(; name=Expr(:., :Mod, Symbol("@a")), line=LineNumberNode(1, :a), args=Any[:b, :(f(x))])
            @test isequal(from_expr(MacroCall, input), output)
        end
        expr = KVExpr(lhs=:key1, rhs=false)
        m = MacroCall(; name=Symbol("@a"))
        result = m(expr)
        @Test result isa MacroCall
        @Test result.name == Symbol("@a")
        @Test MacroUtilities.is_not_provided(result.line)
        @Test result.args == Any[expr]
        @Test to_expr(result) == Expr(:macrocall, Symbol("@a"), nothing, :(key1 = false))
    end
    @testset "MacroDef" begin 
        ex = :(macro a(args...)
            return args[1] |> esc
        end)
        f = from_expr(MacroDef, ex)
        @Test to_expr(f) == ex

        ex = :(macro a(args...) args
        end)
        f = from_expr(MacroDef, ex)
        @Test to_expr(f) == ex
    end
end