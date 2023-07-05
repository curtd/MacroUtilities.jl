module TestMacroUtilities 
    using MacroUtilities
    using TestingUtilities, Test 

    mutable struct NoThrowTestSet <: Test.AbstractTestSet
        results::Vector
        NoThrowTestSet(desc) = new([])
    end
    Test.record(ts::NoThrowTestSet, t::Test.Result) = (push!(ts.results, t); t)
    Test.finish(ts::NoThrowTestSet) = ts.results
    test_results_match = (results, ref_results)-> all(result isa ref_result for (result, ref_result) in zip(results, ref_results) )


    if VERSION ≥ v"1.7"
        macro testthrows(msg, ex)
            return quote 
                Test.@test_throws $msg $ex
            end |> esc 
        end
    else
        macro testthrows(msg, ex)
            return quote
                Test.@test_throws ArgumentError($msg) $(ex)
            end |> esc
        end
    end

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

    @testset "MacroUtilities.jl" begin
        @testset "to_expr" begin 
            @test_cases begin 
                input                  |       output
                [1,2,3]                | :([1,2,3])
                [1.0,2.0,3.0]          | :([1.0,2.0,3.0])
                [false,true]           | :([false,true])
                ["a","b"]              | :(["a","b"])
                Dict(:a => 1, :b => 2) | :(Dict{$(Symbol),$(Int)}( :a => 1, :b => 2 ))
                @test isequal(to_expr(input), output)
            end
        end
        @testset "List expr" begin 
            @test_cases begin 
                input           |  T      | output
                :([])           |  Symbol | Symbol[]
                :(abc)          |  Symbol | [:abc]
                :([abc])        |  Symbol | [:abc]
                :([abc, def])   |  Symbol | [:abc, :def]
                :((abc, def))   |  Symbol | [:abc, :def]
                :((abc, def))   |  String | nothing
                :(("a", "b"))   |  String | ["a","b"]
                :((false,true)) |  Bool   | [false, true]
                :((1,2))        |  Int    | [1, 2]
                @test isequal(from_expr(Vector{T}, input, throw_error=false), output)
            end
            @testthrows "Argument `a` in expression `[a, b]` was not of expected type Bool" from_expr(Vector{Bool}, :([a,b]), throw_error=true)
            @testthrows "Input expression `f(a)` is not a list expression" from_expr(Vector{Bool}, :(f(a)), throw_error=true)
        end

        @testset "Keyword arguments from Expr" begin 
            @testset "Expression parsing" begin 
                @test_cases begin 
                    input           | output
                    :(key = value)  | KVExpr(; key=:key, value=:value)
                    :(key = false)  | KVExpr(; key=:key, value=false)
                    :(key)          | nothing 
                    @test isequal(from_expr(KVExpr, input), output)
                end
                @test_cases begin 
                    input                                               | output
                    :(key1 = (expected_type = Bool, default=false))     | MacroUtilities.KVSpecExpr(; key=:key1, expected_types=Set([:Bool]), default_value=false)
                    :(key1 = (expected_types = [Bool, Int, Symbol]))     | MacroUtilities.KVSpecExpr(; key=:key1, expected_types=Set([:Bool, :Int, :Symbol]), default_value=MacroUtilities.not_provided)
                    @test isequal(from_expr(MacroUtilities.KVSpecExpr, input), output)
                end
                @testthrows "Expression `f(x)` is not of the form `key = value`" from_expr(MacroUtilities.KVSpecExpr, :(f(x)); throw_error=true)
                @testthrows "In expression, `key = rhs`, rhs (= `f(x)`) is not a valid key-value specifier expression" from_expr(MacroUtilities.KVSpecExpr, :(key = f(x)), throw_error=true)
                @testthrows "Neither `expected_type` nor `expected_types` specified" from_expr(MacroUtilities.KVSpecExpr, :(key = ()), throw_error=true)
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
                @testthrows "KVExprParser does not have key `key3`, keys(KVExprParser) = (key1, key2)" parse_kvs!(parser, [:(key3 = "abc")])
                
            end
            @testset "@parse_kwargs" begin 
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

            end
        end
        @testset "Macro parsing" begin 
            @testset "MacroCall" begin 
                @test_cases begin 
                    input                                   | output 
                    :x                                      | nothing
                    Expr(:macrocall, Symbol("@a"), nothing) | MacroCall(; name=Symbol("@a"), line=MacroUtilities.not_provided, args=[])
                    Expr(:macrocall, Expr(:., :Mod, Symbol("@a")), LineNumberNode(1, :a), :b, :(f(x))) | MacroCall(; name=Expr(:., :Mod, Symbol("@a")), line=LineNumberNode(1, :a), args=Any[:b, :(f(x))])
                    @test isequal(from_expr(MacroCall, input), output)
                end
                expr = KVExpr(key=:key1, value=false)
                m = MacroCall(; name=Symbol("@a"))
                result = m(expr)
                @Test result isa MacroCall
                @Test result.name == Symbol("@a")
                @Test MacroUtilities.is_not_provided(result.line)
                @Test result.args == Any[expr]
                @Test to_expr(result) == Expr(:macrocall, Symbol("@a"), MacroUtilities.not_provided, :(key1 = false))
            end
        end
    
        @testset "Function parsing" begin 
            @testset "FuncArg" begin 
                @test_cases begin 
                    input                        | output
                    :(x)                         | FuncArg(; name=:x, type=MacroUtilities.not_provided, value=MacroUtilities.not_provided, is_splat=false)
                    :(x::T)                      | FuncArg(; name=:x, type=:T, value=MacroUtilities.not_provided, is_splat=false)
                    :(x::A.T)                    | FuncArg(; name=:x, type=:(A.T), value=MacroUtilities.not_provided, is_splat=false)
                    Expr(:kw, :(x::A.T), :(f()))                | FuncArg(; name=:x, type=:(A.T), value=:(f()), is_splat=false)
                    :(x::A.T...)                 | FuncArg(; name=:x, type=:(A.T), value=MacroUtilities.not_provided, is_splat=true)
                    :(args...)                   | FuncArg(; name=:args, type=MacroUtilities.not_provided, value=MacroUtilities.not_provided, is_splat=true)
                    1                            | FuncArg(; name=MacroUtilities.not_provided, type=MacroUtilities.not_provided, value=1, is_splat=false)
                    true                            | FuncArg(; name=MacroUtilities.not_provided, type=MacroUtilities.not_provided, value=true, is_splat=false)
                    :(f(1))                            | FuncArg(; name=MacroUtilities.not_provided, type=MacroUtilities.not_provided, value=:(f(1)), is_splat=false)
                    :(:x)                           | FuncArg(; name=MacroUtilities.not_provided, type=MacroUtilities.not_provided, value=:(:x), is_splat=false)
                    @test isequal(from_expr(FuncArg, input), output)
                    @test isequal(to_expr(output), input)
                end
                v = [1,2,3]
                t = :(A.T)
                f = FuncArg(; name=:a, type=t, value=v)
                g = FuncArg(f)
                @Test f == g 
                @Test !(f.value === g.value)
                @Test !(f.type === g.type)
                g = FuncArg(f; name=:b, value=1)
                @Test g.name == :b
                @Test g.type == t
                @Test !(g.type === t)
                @Test g.is_splat == f.is_splat
            end
            @testset "FuncCall" begin 
                @test_cases begin 
                    input                       | output
                    :(f(a,b))                   | FuncCall(; funcname=:f, args=[FuncArg(; name=:a), FuncArg(; name=:b)])
                    :(A.f())                    | FuncCall(; funcname=:(A.f))
                    :(A.f(z, true; a, b=2, c=x))                    | FuncCall(; funcname=:(A.f), args=[FuncArg(; name=:z), FuncArg(; value=true)], kwargs=MacroUtilities.OrderedDict( :a => FuncArg(; name=:a), :b => FuncArg(; name=:b, value=2), :c => FuncArg(; name=:c, value=:x )))
                    :(A.f(z, args...; a::Int, kwargs...))                    | FuncCall(; funcname=:(A.f), args=[FuncArg(; name=:z), FuncArg(; name=:args, is_splat=true)], kwargs=MacroUtilities.OrderedDict( :a => FuncArg(; name=:a, type=:Int), :kwargs => FuncArg(; name=:kwargs, is_splat=true)))

                    @test isequal(from_expr(FuncCall, input), output)
                    @test isequal(to_expr(output), input)
                end
                args = [FuncArg(; value=:(f(x)))]
                v = [1,2,3]
                kwargs = MacroUtilities.OrderedDict(:b => FuncArg(; name=:b, value=v))
                f = FuncCall(; funcname=:(A.f), args=args, kwargs=kwargs )
                g = FuncCall(f)
                @Test f == g 
                @Test !(f.funcname === g.funcname)
                @Test !(f.args[1] === g.args[1])
                @Test !(f.kwargs[:b] === g.kwargs[:b])
                new_args = [FuncArg(; value=:(g(x)))]
                g = FuncCall(f; args=new_args)
                @Test !(f.funcname === g.funcname)
                @Test f.funcname == g.funcname
                @Test g.args === new_args
                @Test !(f.kwargs[:b] === g.kwargs[:b])
                @Test f.kwargs[:b] == g.kwargs[:b]

                f = from_expr(FuncCall, :(A.f(1, 2, 3; k=1, l=2, m=3)))
                g = map_args(t->FuncArg(t; value=t.value+1), f)
                @Test to_expr(g) == :(A.f(2,3,4; k=1, l=2, m=3))
                g = map_kwargs(t->FuncArg(t; value=t.value+1), f)
                @Test to_expr(g) == :(A.f(1,2,3; k=2, l=3, m=4))
            end
            @testset "FuncDef" begin 
                ex = quote 
                    """
                        f(a, b; key1, kwargs...)
                    """
                    function f(a::T, b::Int; key1="abc", kwargs...) where {T}
                        return nothing
                    end
                end
                f = from_expr(FuncDef, ex)
                @Test isequal(f.header, FuncCall(; funcname=:f, args=[FuncArg(; name=:a, type=:T), FuncArg(; name=:b, type=:Int)], kwargs=MacroUtilities.OrderedDict(:key1 => FuncArg(; name=:key1, value="abc"), :kwargs => FuncArg(; name=:kwargs, is_splat=true))))
                @Test isequal(f.whereparams, [:T])
                @Test isequal(f.body, ex.args[2].args[4].args[2])
                @Test isequal(f.line, ex.args[1])
                @Test isequal(f.doc, ex.args[2].args[3])

                @Test to_expr(f) == ex

                g = map_args(t->FuncArg(t; name=(t.name == :a ? :c : :d)), f)
                @test to_expr(g).args[2].args[4].args[1].args[1] == :(f(c::T, d::Int; key1 = "abc", kwargs...))
                g = map_kwargs(t->FuncArg(t; name=Symbol(uppercase(string(t.name)))), f)
                @test to_expr(g).args[2].args[4].args[1].args[1] == :(f(a::T, b::Int; KEY1 = "abc", KWARGS...))

                ex = quote 
                    function (a, b::Int=1, args...; c)
                        return a
                    end
                end
                f = from_expr(FuncDef, ex)
                @Test f.head == :function
                @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a), FuncArg(; name=:args, is_splat=true)], kwargs=MacroUtilities.OrderedDict(:c => FuncArg(; name=:c), :b => FuncArg(; name=:b, value=1, type=:Int))))
                @Test isequal(f.whereparams, MacroUtilities.not_provided)
                @Test isequal(f.body, ex.args[2].args[2])
                @Test isequal(f.line, ex.args[1])
                @Test isequal(f.doc, MacroUtilities.not_provided)

                ref_ex = Expr(:block, 
                    ex.args[1], 
                    Expr(:function, 
                        Expr(:tuple, Expr(:parameters, :c, Expr(:kw, :(b::Int), 1)), :a, :(args...)),
                        ex.args[2].args[2]
                    )
                )
                @Test to_expr(f) == ref_ex

                ex = :( (a;b=0) -> a+b )
                f = from_expr(FuncDef, ex)
                @Test f.head == :->
                @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a)], kwargs=MacroUtilities.OrderedDict(:b => FuncArg(; name=:b, value=0))))
                @Test isequal(f.whereparams, MacroUtilities.not_provided)
                @Test isequal(f.body, ex.args[2])
                @Test isequal(f.line, ex.args[1].args[2])
                @Test isequal(f.doc, MacroUtilities.not_provided)
                @Test to_expr(f) == ex

                ex = :( H(b,c) = b )
                f = from_expr(FuncDef, ex)
                @Test f.head == :(=)
                @Test isequal(f.header, FuncCall(; funcname=:H, args=[FuncArg(; name=:b), FuncArg(; name=:c)],))
                @Test isequal(f.whereparams, MacroUtilities.not_provided)
                @Test isequal(f.body, ex.args[2])
                @Test isequal(f.line, ex.args[2].args[1])
                @Test isequal(f.doc, MacroUtilities.not_provided)

                @Test to_expr(f) == ex 
            end
        end
     
    end
end