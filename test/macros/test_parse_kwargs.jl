macro ex_macro(args...)
    @parse_kwargs args... begin 
        key1 = (expected_type = Int)
        key2 = (expected_types = (Bool, Symbol, Vector{Symbol}), default = false)
    end
    return quote 
        key1 = $key1 
        key2 = $key2
    end |> esc
end
macro ex_macro2(args...)
    @parse_kwargs args... begin 
        key1::Int
        key2::Union{Bool, Symbol, Vector{Symbol}} = false
    end
    return quote 
        key1 = $key1 
        key2 = $key2
    end |> esc
end

@testset "@parse_kwargs" begin 
    @testset "KVSpecExpr" begin 
        ex = :(key::String)
        f = from_expr(MacroUtilities.KVSpecExpr, ex)
        @Test f == MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String]))
        ex = :(key::Union{String,Int})
        f = from_expr(MacroUtilities.KVSpecExpr, ex)
        @Test f == MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int]))
        ex = :(key::Union{String,Int, Base.Float64})
        f = from_expr(MacroUtilities.KVSpecExpr, ex)
        @Test f == MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int, :(Base.Float64)]))
        ex = :(key::Union{String,Int, Base.Float64} = "abcd")
        f = from_expr(MacroUtilities.KVSpecExpr, ex)
        @Test f == MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int, :(Base.Float64)]), default_value="abcd")
        ex = :(key = (expected_type=String,))
        f = from_expr(MacroUtilities.KVSpecExpr, ex)
        @Test f == MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String]))
        ex = :(key = (expected_types=(String,Int), default=1))
        f = from_expr(MacroUtilities.KVSpecExpr, ex)
        @Test f == MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int]), default_value=1)
        
    end
    @testset "KVExprParser" begin 
        parser = MacroUtilities.KVExprParser(MacroUtilities.KVSpec(; key=:key1, expected_types=Set([Bool, Int])), MacroUtilities.KVSpec(; key=:key2, expected_types=Set([Vector{Symbol}, Vector{String}]), default_value=[:default]))

        parse_kvs!(parser, [:(key1=1)])
        @test parser.found_values == Dict{Symbol,Any}(:key1 => 1, :key2 => [:default])

        empty!(parser.found_values)
        parse_kvs!(parser, [:(key1=false), :(key2 = (a,b,c))])
        @test parser.found_values == Dict{Symbol,Any}(:key1 => false, :key2 => [:a, :b, :c])
        empty!(parser.found_values)

        @testthrows "No value provided for key `key1`" parse_kvs!(parser, [])

        empty!(parser.found_values)
        @testthrows "In `key1 = rhs` expression, rhs (= abc) has type String, which is not one of the expected types (Int64, Bool)" parse_kvs!(parser, [:(key1 = "abc")])

        empty!(parser.found_values)
        @testthrows "Unexpected key `key3`, expected keys = (key1, key2)" parse_kvs!(parser, [:(key3 = "abc")])
        
    end
   
    let 
        @ex_macro key1=0
        @test key1 == 0 
        @test key2 == false
    end
    let 
        @ex_macro key1=1 key2=[a,b,c]
        @test key1 == 1
        @test key2 == [:a,:b,:c]
    end
    let 
        @ex_macro2 key1=0
        @test key1 == 0 
        @test key2 == false
    end
    let 
        @ex_macro2 key1=1 key2=[a,b,c]
        @test key1 == 1
        @test key2 == [:a,:b,:c]
    end
end