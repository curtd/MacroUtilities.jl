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

        ex = :(struct A{B, C<:D, F}
            key1::B 
            key2::C
            key3::F
        end)
        f = from_expr(StructDef, ex)
        c = kwarg_constructor(f, (;))
        c_expr = to_expr(c)
        @Test c_expr.args[1] == :(A(; key1::B, key2::C, key3::F) where {B, C<:D, F})
        @Test c_expr.args[2] == Expr(:block, :(A(key1, key2, key3)))
    end
    @testset "copy_constructor" begin 
        ex = :(struct A 
            key1::String 
            key2::Int
            key3::Symbol
        end)
        f = from_expr(StructDef, ex)

        c = copy_constructor(f; input_var=:t)
        c_expr = to_expr(c)
        @Test c_expr.args[1] == :(A(t::A; key1::String=Base.copy(Base.getfield(t, :key1)), key2::Int=Base.copy(Base.getfield(t, :key2)), key3::Symbol=Base.getfield(t, :key3)))
        @Test c_expr.args[2] == Expr(:block, :(A(key1, key2, key3)))

        ex = :(struct A{B, C<:D, F}
            key1::B 
            key2::C
            key3::F
        end)
        f = from_expr(StructDef, ex)

        c = copy_constructor(f; input_var=:t)
        c_expr = to_expr(c)
        
        @Test c_expr.args[1] == :(A(t::A{B,C,F}; key1::B=Base.copy(Base.getfield(t, :key1)), key2::C=Base.copy(Base.getfield(t, :key2)), key3::F=Base.copy(Base.getfield(t, :key3))) where {B, C <: D, F})
        @Test c_expr.args[2] == Expr(:block, :(A(key1, key2, key3)))

    end
end