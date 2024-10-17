@testitem "Expr Generation" setup=[TestSetup] begin 
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
        @Test c.header.kwargs[:key1].name == :key1
        @Test c.header.kwargs[:key1].type == :String
        @Test c.header.kwargs[:key2].name == :key2
        @Test c.header.kwargs[:key2].type == :Int
        @Test c.header.kwargs[:key3].name == :key3
        @Test c.header.kwargs[:key3].type == :Symbol
        for key in (:key1, :key2, :key3)
            @Test c.header.kwargs[key].value == Base.remove_linenums!(quote
                local $key = $Base.getfield(t, $(QuoteNode(key)))
                if typeof($key) in ($String, $Symbol, $Missing, $Nothing)
                    $key
                else
                    $Base.copy($key)
                end
            end)
        end
      
        @Test c.body == Base.remove_linenums!(quote A(key1, key2, key3) end)

        ex = :(struct A{B, C<:D, F}
            key1::B 
            key2::C
            key3::F
        end)
        f = from_expr(StructDef, ex)

        c = copy_constructor(f; input_var=:t)
        @Test c.header.kwargs[:key1].name == :key1
        @Test c.header.kwargs[:key1].type == :B
        @Test c.header.kwargs[:key2].name == :key2
        @Test c.header.kwargs[:key2].type == :C
        @Test c.header.kwargs[:key3].name == :key3
        @Test c.header.kwargs[:key3].type == :F
        for key in (:key1, :key2, :key3)
            @Test c.header.kwargs[key].value == Base.remove_linenums!(quote
                local $key = $Base.getfield(t, $(QuoteNode(key)))
                if typeof($key) in ($String, $Symbol, $Missing, $Nothing)
                    $key
                else
                    $Base.copy($key)
                end
            end)
        end
        @Test c.body == Base.remove_linenums!(quote A(key1, key2, key3) end)
        
    end
end