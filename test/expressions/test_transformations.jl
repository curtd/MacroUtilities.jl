@testitem "Transformations" setup=[TestSetup] begin 
    @testset "Transformations" begin 
        ex = :(f(_))
        new_expr, was_replaced = replace_symbols(ex, [:_ => :x])
        @Test new_expr == :(f(x))
        @Test was_replaced
        
        new_expr, was_replaced = replace_symbols(ex, [:a => :x])
        @Test new_expr == ex 
        @Test !was_replaced

        ex = :(if f(_) _^2 else g(x) end )
        new_expr, was_replaced = replace_symbols(ex, [:_ => :a])
        @Test new_expr == Expr(:if, :(f(a)), Expr(:block, ex.args[2].args[1], :(a^2)), Expr(:block, ex.args[3].args[1], :(g(x))))
        @Test was_replaced == true
        

        f = ReplaceSymbols(:((g(_))::T), :_)
        @Test !isnothing(f)
        @Test f(:_ => :x) == :(g(x)::T)
        @Test f(:_ => :y) == :(g(y)::T)

        f = ReplaceSymbols(:((g(_))::T), :_, :g)
        @Test f(:_ => :x) == :(g(x)::T)
        @Test f(:_ => :x, :g => :h) == :(h(x)::T)
        
        f = ReplaceSymbols(:((g(_))::T), :a)
        @Test isnothing(f)

    end
end