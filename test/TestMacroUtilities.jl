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
            end 
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

    macro ex_macro3(arg1)
        @assert_type arg1 Int
        return quote 
            arg1 = $arg1 
        end |> esc
    end

    macro ex_macro4(ex)
        f = from_expr(FuncDef, ex)
        return to_expr(__doc__macro(f)) |> esc 
    end

    """
        some docs
    """
    @ex_macro4 ex_macro4_func(x) = 1

    @testset "MacroUtilities.jl" begin
        @testset "Utilities" begin 
            a = 1
            @Test @assert_type a Int
            @Test @assert_type a (Int,Float64,String)
            @testthrows "Expected type of `a` to be Float64, got typeof(a) = Int64" @assert_type a Float64
            @testthrows "Expected type of `a` to be one of (Float64, String), got typeof(a) = Int64" @assert_type a (Float64, String)
            let 
                @ex_macro3 2
                @test arg1 == 2
            end
            if VERSION ≥ v"1.7"
                @test_throws "Expected type of `arg1` to be Int, got typeof(arg1) = Float64" @eval module A 
                    import ..@ex_macro3 
                    @ex_macro3 1.0
                end
            end
        end

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
        @testset "Expressions" begin 
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
            @testset "NestedDotExpr" begin 
                ex = :(A.b)
                f = from_expr(NestedDotExpr, ex)
                @Test f.keys == [:A, :b]
                @Test to_expr(f) == ex

                ex = :(A.b.c)
                f = from_expr(NestedDotExpr, ex)
                @Test f.keys == [:A, :b, :c]
                @Test to_expr(f) == ex
                
                ex = :(A.b.c.d.e)
                f = from_expr(NestedDotExpr, ex)
                @Test f.keys == [:A, :b, :c, :d, :e]
                @Test to_expr(f) == ex
                @testthrows "Input expression `f(x)` is not a dot expression" from_expr(NestedDotExpr, :(f(x)); throw_error=true)
            end

            @testset "NamedTupleExpr" begin 
                ex = :((a=1, b=f(x)))
                f = from_expr(NamedTupleExpr, ex)
                @Test f.args == [NamedTupleArg(; key=:a, value=1, kw_head=false), NamedTupleArg(; key=:b, value=:(f(x)), kw_head=false)]
                @Test to_expr(f) == ex
                @Test Set(keys(f)) == Set([:a, :b])
                @Test f[:a] == NamedTupleArg(; key=:a, value=1, kw_head=false)
                @Test f[:b] == NamedTupleArg(; key=:b, value=:(f(x)), kw_head=false)
                @testthrows "NamedTupleExpr does not have key `c`" f[:c]
                @testthrows "Cannot set key `a` in NamedTupleExpr -- key already exists" f[] = NamedTupleArg(; key=:a, value=1, kw_head=false)
                f[] = NamedTupleArg(; key=:c, kw_head=true)
                @Test to_expr(f) == :((a=1, b=f(x), c))
                @Test pop!(f) == NamedTupleArg(; key=:c, kw_head=true)
                @Test to_expr(f) == ex
                f[1] = NamedTupleArg(; key=:a, value=2, kw_head=false)
                @Test to_expr(f) == :((a=2, b=f(x)))

                ex = :((; a, b=f(x)))
                f = from_expr(NamedTupleExpr, ex)
                @Test f.args == [NamedTupleArg(; key=:a, kw_head=true), NamedTupleArg(; key=:b, value=:(f(x)), kw_head=true)]
                @Test to_expr(f) == ex
                @Test Set(keys(f)) == Set([:a, :b])
                @Test f[:a] == NamedTupleArg(; key=:a, kw_head=true)
                @Test f[:b] == NamedTupleArg(; key=:b, value=:(f(x)), kw_head=true)
                @testthrows "NamedTupleExpr does not have key `c`" f[:c]
                @Test to_expr(f) == ex
                
                ex = :((a; c, b=f(x), d...))
                f = from_expr(NamedTupleExpr, ex)
                @Test f.args == [NamedTupleArg(; key=:c, kw_head=true), NamedTupleArg(; key=:b, value=:(f(x)), kw_head=true), NamedTupleArg(; key=:d, kw_head=true, is_splat=true), NamedTupleArg(; key=:a, kw_head=false)]
                @Test to_expr(f) == ex
            end

            @testset "Nested Exprs" begin 
                ex = :(key = (a=1, b=true))
                f = from_expr(ExprWOptionalRhs{Symbol}, ex)
                @Test propertynames(f) == (:lhs, :rhs)
                @Test f.lhs == :key 
                @Test f.rhs == ex.args[2]
                @Test to_expr(f) == ex 
              
                ex = :(key = (a=1, b=true))
                f = from_expr(ExprWOptions{Symbol}, ex)
                @Test propertynames(f) == (:lhs, :options)
                @Test f.lhs == :key 
                @Test f.options == from_expr(NamedTupleExpr, ex.args[2])
                @Test to_expr(f) == ex 
                @Test f[:a] == NamedTupleArg(; key=:a, value=1, kw_head=false)
                @Test f[:b] == NamedTupleArg(; key=:b, value=true, kw_head=false)
               
            end

            @testset "BlockExpr" begin
                expr = BlockExpr(KVExpr(; lhs=:a, rhs=1))
                @Test to_expr(expr) == Expr(:block, :(a = 1))
                expr = [KVExpr(; lhs=:a, rhs=1); KVExpr(; lhs=:b, rhs=false)]
                @Test expr isa BlockExpr
                @Test to_expr(expr) == Expr(:block, :(a = 1), :(b = false))
            end

            @testset "Keyword arguments from Expr" begin 
                @testset "Expression parsing" begin 
                    @test_cases begin 
                        input           | output
                        :(key = value)  | KVExpr(; lhs=:key, rhs=:value)
                        :(key = false)  | KVExpr(; lhs=:key, rhs=false)
                        :(key)          | KVExpr(; lhs=:key) 
                        :(f(x))         | nothing
                        @test isequal(from_expr(KVExpr, input), output)
                    end
                    @test_cases begin 
                        input                                               | output
                        :(key1 = (expected_type = Bool, default=false))     | MacroUtilities.KVSpecExpr(; key=:key1, expected_types=Set([:Bool]), default_value=false)
                        :(key1 = (expected_types = [Bool, Int, Symbol]))     | MacroUtilities.KVSpecExpr(; key=:key1, expected_types=Set([:Bool, :Int, :Symbol]), default_value=MacroUtilities.not_provided)
                        @test isequal(from_expr(MacroUtilities.KVSpecExpr, input), output)
                    end
                    @testthrows "Input expression `f(x)` is not a valid keyword argument specifier expression" from_expr(MacroUtilities.KVSpecExpr, :(f(x)); throw_error=true)
                    @testthrows "Input expression `f(x)` is not a NamedTuple expression" from_expr(MacroUtilities.KVSpecExpr, :(key = f(x)), throw_error=true)
                    @testthrows "Neither `expected_type` nor `expected_types` specified" from_expr(MacroUtilities.KVSpecExpr, :(key = ()), throw_error=true)
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
                    expr = KVExpr(lhs=:key1, rhs=false)
                    m = MacroCall(; name=Symbol("@a"))
                    result = m(expr)
                    @Test result isa MacroCall
                    @Test result.name == Symbol("@a")
                    @Test MacroUtilities.is_not_provided(result.line)
                    @Test result.args == Any[expr]
                    @Test to_expr(result) == Expr(:macrocall, Symbol("@a"), nothing, :(key1 = false))
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
                    h = name_only(f)
                    @Test h.name == :a 
                    @Test h.value |> is_not_provided
                    @Test h.type |> is_not_provided
                    @Test h.is_splat == false 
                    h = name_only(FuncArg(; name=:args, value=:([1,2,3]), is_splat=true))
                    @Test h.name == :args
                    @Test h.value |> is_not_provided
                    @Test h.type |> is_not_provided
                    @Test h.is_splat == true
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

                    g = names_only(f)
                    @Test to_expr(g) == :(A.f(1, 2, 3; k, l, m))

                    ex = :(A.f(args..., z, h=1; kwargs...))
                    f = from_expr(FuncCall, ex)
                    @Test to_expr(f) == ex
                    @Test to_expr(names_only(f)) == :(A.f(args..., z, h; kwargs...))
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
                    @Test propertynames(f) == (:funcname, :args, :kwargs, :header, :head, :whereparams, :return_type, :body, :line, :doc)
                    @Test isequal(f.funcname, :f)
                    @Test isequal(f.args, [FuncArg(; name=:a, type=:T), FuncArg(; name=:b, type=:Int)])
                    @Test isequal(f.kwargs, MacroUtilities.OrderedDict(:key1 => FuncArg(; name=:key1, value="abc"), :kwargs => FuncArg(; name=:kwargs, is_splat=true)))
                    @Test isequal(f.header, FuncCall(; funcname=:f, args=[FuncArg(; name=:a, type=:T), FuncArg(; name=:b, type=:Int)], kwargs=MacroUtilities.OrderedDict(:key1 => FuncArg(; name=:key1, value="abc"), :kwargs => FuncArg(; name=:kwargs, is_splat=true))))
                    @Test MacroUtilities.is_not_provided(f.return_type)
                    @Test isequal(f.whereparams, [:T])
                    @Test isequal(f.body, ex.args[2].args[4].args[2])
                    @Test isequal(f.line, ex.args[1])
                    @Test isequal(f.doc, ex.args[2].args[3])

                    @Test to_expr(f) == ex

                    g = names_only(f.header)
                    @Test isequal(g.funcname, :f)
                    @Test isequal(g.args, [FuncArg(; name=:a), FuncArg(; name=:b)])
                    @Test isequal(g.kwargs, MacroUtilities.OrderedDict(:key1 => FuncArg(; name=:key1), :kwargs => FuncArg(; name=:kwargs, is_splat=true)))
                    @Test to_expr(g) == :(f(a, b; key1, kwargs...))

                    g = map_args(t->FuncArg(t; name=(t.name == :a ? :c : :d)), f)
                    @test to_expr(g).args[2].args[4].args[1].args[1] == :(f(c::T, d::Int; key1 = "abc", kwargs...))
                    g = map_kwargs(t->FuncArg(t; name=Symbol(uppercase(string(t.name)))), f)
                    @test to_expr(g).args[2].args[4].args[1].args[1] == :(f(a::T, b::Int; KEY1 = "abc", KWARGS...))

                    ex = quote 
                        """
                            f(a, b; key1, kwargs...)
                        """
                        function f(a::T, b::Int; key1="abc", kwargs...)::Nothing where {T}
                            return nothing
                        end
                    end
                    f = from_expr(FuncDef, ex)
                    @Test isequal(f.header, FuncCall(; funcname=:f, args=[FuncArg(; name=:a, type=:T), FuncArg(; name=:b, type=:Int)], kwargs=MacroUtilities.OrderedDict(:key1 => FuncArg(; name=:key1, value="abc"), :kwargs => FuncArg(; name=:kwargs, is_splat=true))))
                    @Test isequal(f.return_type, :Nothing)
                    @Test isequal(f.whereparams, [:T])
                    @Test isequal(f.body, ex.args[2].args[4].args[2])
                    @Test isequal(f.line, ex.args[1])
                    @Test isequal(f.doc, ex.args[2].args[3])

                    @Test to_expr(f) == ex

                    ex = quote 
                        function (a, b::Int=1, args...; c)
                            return a
                        end
                    end
                    f = from_expr(FuncDef, ex)
                    @Test f.head == :function
                    @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a), FuncArg(; name=:b, type=:Int, value=1), FuncArg(; name=:args, is_splat=true)], kwargs=MacroUtilities.OrderedDict(:c => FuncArg(; name=:c))))
                    @Test is_not_provided(f.return_type)
                    @Test isequal(f.whereparams, MacroUtilities.not_provided)
                    @Test isequal(f.body, ex.args[2].args[2])
                    @Test isequal(f.line, ex.args[1])
                    @Test isequal(f.doc, MacroUtilities.not_provided)
                    @Test to_expr(f) == ex

                    f = from_expr(FuncDef, ex; normalize_kwargs=true)
                    @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a), FuncArg(; name=:args, is_splat=true)], kwargs=MacroUtilities.OrderedDict(:c => FuncArg(; name=:c), :b => FuncArg(; name=:b, value=1, type=:Int))))
                    @Test is_not_provided(f.return_type)
                    @Test is_not_provided(f.whereparams)
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
                    @test to_expr(f) == ref_ex

                    expr = :( (a;b=0) -> a+b )
                    f = from_expr(FuncDef, expr)
                    @Test f.head == :->
                    @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a)], kwargs=MacroUtilities.OrderedDict(:b => FuncArg(; name=:b, value=0))))
                    @Test isequal(f.whereparams, MacroUtilities.not_provided)
                    @Test isequal(f.return_type, MacroUtilities.not_provided)
                    @Test isequal(f.body, expr.args[2])
                    @Test isequal(f.line, expr.args[1].args[2])
                    @Test isequal(f.doc, MacroUtilities.not_provided)
                    @Test to_expr(f) == expr

                    expr = :( (a,c=1;b=0) -> a+b )
                    f = from_expr(FuncDef, expr)
                    @Test f.head == :->
                    @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a), FuncArg(; name=:c, value=1)], kwargs=MacroUtilities.OrderedDict(:b => FuncArg(; name=:b, value=0))))
                    @Test isequal(f.whereparams, MacroUtilities.not_provided)
                    @Test isequal(f.return_type, MacroUtilities.not_provided)
                    @Test isequal(f.body, expr.args[2])
                    @Test is_not_provided(f.line)
                    @Test isequal(f.doc, MacroUtilities.not_provided)
                    @Test to_expr(f) == expr


                    ex = :( (a;b=0)::typeof(a) -> a+b )
                    f = from_expr(FuncDef, ex)
                    @Test f.head == :->
                    @Test isequal(f.header, FuncCall(; funcname=MacroUtilities.not_provided, args=[FuncArg(; name=:a)], kwargs=MacroUtilities.OrderedDict(:b => FuncArg(; name=:b, value=0))))
                    @Test isequal(f.whereparams, MacroUtilities.not_provided)
                    @Test isequal(f.return_type, :(typeof(a)))
                    @Test isequal(f.body, ex.args[2])
                    @Test isequal(f.line, ex.args[1].args[1].args[2])
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
            @testset "Struct parsing" begin 
                ex = quote 
                    struct A end 
                end
                f = from_expr(StructDef, ex.args[2])
                @Test propertynames(f) == (:is_mutable, :header, :lnn, :fields, :constructors, :typename, :parameter, :supertype)
                @Test f.typename == :A
                @Test MacroUtilities.is_not_provided(f.supertype)
                @Test MacroUtilities.is_not_provided(f.parameter)
                @Test f.is_mutable == false 
                @Test isempty(f.fields)
                @Test isempty(f.constructors)

                @Test to_expr(f) == ex.args[2]

                ex = quote 
                    mutable struct A{B<:T} <: C{T}
                        key1::B
                        key2::Vector{Any}
                        key3
                        A(key1::B) where {B} = new{B}(key1, Any[], key3)
                        A() = new{Nothing}(nothing, Any[], nothing)
                    end 
                end
                f = from_expr(StructDef, ex.args[2])
                @Test f.typename == :A
                @Test f.supertype == :(C{T})
                @Test f.parameter == :(B<:T)
                @Test f.is_mutable == true 
                @Test f.fields == [(StructDefField(; name=:key1, type=:B), ex.args[2].args[3].args[1]), (StructDefField(; name=:key2, type=:(Vector{Any})), ex.args[2].args[3].args[3]), (StructDefField(; name=:key3), ex.args[2].args[3].args[5])]

                ref_constructor1 = FuncDef(; head=:(=), header=FuncCall(; funcname=:A, args=[FuncArg(; name=:key1, type=:B)]), whereparams=[:B], line=ex.args[2].args[3].args[7], body=ex.args[2].args[3].args[8].args[2])
                ref_constructor2 = FuncDef(; head=:(=), header=FuncCall(; funcname=:A,), line=ex.args[2].args[3].args[9], body=ex.args[2].args[3].args[10].args[2])
                
                @Test f.constructors == [(ref_constructor1, ex.args[2].args[3].args[7]), (ref_constructor2, ex.args[2].args[3].args[9])]

                @Test to_expr(f) == ex.args[2]
                
            end
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
    end
end