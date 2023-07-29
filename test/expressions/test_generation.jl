@testset "Expr Generation" begin 
    @testset "kwarg_constructor" begin 
        ex = :(struct A 
            key1::String 
            key2::Int
            key3::Bool
        end)
        f = from_expr(StructDef, ex)

        c = kwarg_constructor(f, (key1 = "abc", key2 = :(g())))
        c_expr = to_expr(c)
        @Test c_expr.args[1] == :(A(; key1::String = "abc", key2::Int = g(), key3::Bool))
        @Test c_expr.args[2] == Expr(:block, :(A(key1, key2, key3)))

        f = from_expr(StructDef, :(struct A end))
        @Test isnothing(kwarg_constructor(f, (key1 = 1,)))
    end
end