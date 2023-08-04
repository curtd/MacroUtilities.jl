module TestModule 
    function f end 
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

        ex = :($TestModule.f(x) = nothing)
        f = from_expr(FuncDef, ex)
        g = FuncDef(f; return_type=:Int)
        @Test to_expr(g) == Expr(:(=), :($(ex.args[1])::Int), ex.args[2])
    end
end