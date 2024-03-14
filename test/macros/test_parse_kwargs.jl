macro parse_kwargs_macro1(args...)
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
macro parse_kwargs_macro2(args...)
    @parse_kwargs args... begin 
        key1::Int
        key2::Union{Bool, Symbol, Vector{Symbol}} = false
    end
    return quote 
        key1 = $key1 
        key2 = $key2
    end |> esc
end

macro parse_kwargs_macro3(args...)
    @parse_kwargs args... begin 
        key1::Union{String, Nothing}
        key2::Vector{String} = String[]
        key3::Vector{Symbol}
    end
    return quote
        key1 = $key1 
        key2 = $key2
        key3 = $key3
    end |> esc
end

macro parse_kwargs_macro4(args...)
    @parse_kwargs args... begin 
        key1::Any
        key2::Any = nothing
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
            input                                            | output
            :(key::String)                                   | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String]))
            :(key::Vector{String})                           | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:(Vector{String})]))
            :(key::Union{String,Int})                        | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int]))
            :(key::Union{Vector{String},Int})                | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:(Vector{String}), :Int]))
            :(key::Union{String,Int, Base.Float64})          |  MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int, :(Base.Float64)]))
            :(key::Union{String,Int, Base.Float64} = "abcd") | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int, :(Base.Float64)]), default_value="abcd")
            :(key = (expected_type=String,))                 | MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String]))
            :(key = (expected_types=(String,Int), default=1))| MacroUtilities.KVSpecExpr(; key=:key, expected_types=Set([:String, :Int]), default_value=1)
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
    @testset "Basic type parsing" begin 
        let 
            @parse_kwargs_macro1 key1=0
            @test key1 == 0 
            @test key2 == false
            @test isempty(rest)
        end
        let 
            @parse_kwargs_macro1 key1=1 a=1 f(x)
            @test key1 == 1 
            @test key2 == false
            @test rest == (:(a=1), :(f(x)))
        end
        let 
            @parse_kwargs_macro1 key1=1 key2=[a,b,c]
            @test key1 == 1
            @test key2 == [:a,:b,:c]
            @test isempty(rest)
        end
    end
    @testset "Basic type parsing w/ type shorthand" begin 
        let 
            @parse_kwargs_macro2 key1=0
            @test key1 == 0 
            @test key2 == false
        end
        let 
            key2 = 1
            @parse_kwargs_macro2 key1=0 key2
            @test key1 == 0 
            @test key2 == 1
        end
        let 
            @parse_kwargs_macro2 key1=1 key2=[a,b,c]
            @test key1 == 1
            @test key2 == [:a,:b,:c]
        end
    end
    @testset "Parsing vectors" begin 
        let 
            @parse_kwargs_macro3 key1="abc" key3=zzz
            @test key1 == "abc"
            @test key2 == String[]
            @test key3 == [:zzz]
        end
        let 
            @parse_kwargs_macro3 key1="a" key2="z" key3=b
            @test key1 == "a"
            @test key2 == ["z"]
            @test key3 == [:b]
        end
        let 
            @parse_kwargs_macro3 key1="a" key2=("z","y") key3=(x,y)
            @test key1 == "a"
            @test key2 == ["z","y"]
            @test key3 == [:x, :y]
        end
        let 
            @parse_kwargs_macro3 key1=nothing key2=["z","y"] key3=[x,y]
            @test key1 |> isnothing
            @test key2 == ["z","y"]
            @test key3 == [:x, :y]
        end

    end
    @testset "Parsing `Any`" begin 
        let 
            @parse_kwargs_macro4 key1=1
            @test key1 == 1
            @test key2 |> isnothing
        end
        let 
            @parse_kwargs_macro4 key1="a" key2="b"
            @test key1 == "a"
            @test key2 == "b"
        end
    end
    @testset "Avoid " begin 
        let 
            TestParseKwargsOuter.TestParseKwargsInner.@a key1="abc"
            @test a == "abc"    
        end
    end

end