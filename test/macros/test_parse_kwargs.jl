macro ex_macro(args...)
    rest = @parse_kwargs args... begin 
        key1 = (expected_type = Int)
        key2 = (expected_types = (Bool, Symbol, Vector{Symbol}), default = false)
    end
    return quote 
        key1 = $key1 
        key2 = $key2
        rest = $(Expr(:tuple, QuoteNode.(rest)...))
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

module TestParseKwargsOuter 
    using MacroUtilities

    module TestParseKwargsInner 
        import ..MacroUtilities
        import ..@parse_kwargs

        macro a(args...)
            @parse_kwargs args... begin 
                key1::String
            end
            return quote 
                a = $key1
            end |> esc
        end
    end
end

@testset "@parse_kwargs" begin 
    @testset "KVSpecExpr" begin 
        @test_cases begin 
            input                                   | output
            :(key::String)                          | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String]))
            :(key::Union{String,Int})               | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int]))
            :(key::Union{String,Int, Base.Float64}) |  MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int, :(Base.Float64)]))
            :(key::Union{String,Int, Base.Float64} = "abcd") | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int, :(Base.Float64)]), default_value="abcd")
            :(key = (expected_type=String,))        | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String]))
            :(key = (expected_types=(String,Int), default=1))  | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int]), default_value=1)
            @test from_expr(MacroUtilities.KVSpecExpr, input) == output
        end
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
        @testthrows "Unexpected key `key3`, expected keys = (key1, key2)" parse_kvs!(parser, [:(key3 = "abc")], strict=true)
        @testthrows "ArgumentError: No value provided for key `key1" parse_kvs!(parser, [:(key3 = "abc")])
    end
   
    let 
        @ex_macro key1=0
        @test key1 == 0 
        @test key2 == false
        @test isempty(rest)
    end
    let 
        @ex_macro key1=1 a=1 f(x)
        @test key1 == 1 
        @test key2 == false
        @test rest == (:(a=1), :(f(x)))
    end
    let 
        @ex_macro key1=1 key2=[a,b,c]
        @test key1 == 1
        @test key2 == [:a,:b,:c]
        @test isempty(rest)
    end
    let 
        @ex_macro2 key1=0
        @test key1 == 0 
        @test key2 == false
    end
    let 
        key2 = 1
        @ex_macro2 key1=0 key2
        @test key1 == 0 
        @test key2 == 1
    end
    let 
        @ex_macro2 key1=1 key2=[a,b,c]
        @test key1 == 1
        @test key2 == [:a,:b,:c]
    end
    let 
        TestParseKwargsOuter.TestParseKwargsInner.@a key1="abc"
        @test a == "abc"    
    end

end