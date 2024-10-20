@testitem "Expressions" setup=[TestSetup] begin 
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
    @testset "NestedDotExpr" begin 
        @test_cases begin 
            expr        | f
            :A          | NestedDotExpr([:A])
            :(A.b)      | NestedDotExpr([:A, :b])
            :(A.b.c)    | NestedDotExpr([:A, :b, :c])
            :(A.b.c.d.e)| NestedDotExpr([:A, :b, :c, :d, :e])
            @test from_expr(NestedDotExpr, expr) == f 
            @test to_expr(f) == expr
        end

        @testthrows "Input expression `f(x)` is not a dot expression" from_expr(NestedDotExpr, :(f(x)); throw_error=true)
    end

    @testset "UnionExpr" begin 
        @test_cases begin 
            input           |   output 
            :(Union{A,B})   | UnionExpr(Any[:A, :B])
            :(Union{})   | UnionExpr(Any[])
            :(Union{A, F(B)})   | UnionExpr(Any[:A, :(F(B))])
            @test from_expr(UnionExpr, input) == output
            @test to_expr(output) == input
        end
        @Test from_expr(UnionExpr, :(f(x))) |> isnothing
    end
    @testset "TypeVarExpr" begin 
        @test_cases begin 
            input               | output
            :(A)                | TypeVarExpr{Symbol, Any,Any}(; typename=:A)
            :(A <: B)           | TypeVarExpr{Symbol, Any,Any}(; typename=:A, ub=:B)
            :(A <: B <: C)      | TypeVarExpr(; typename=:B, lb=:A, ub=:C)
            :(A >: B)           | TypeVarExpr(; typename=:A, lb=:B)
            @test from_expr(TypeVarExpr, input) == output
            @test from_expr(MacroUtilities.SymbolTypeVar, input) == output
            @test to_expr(output) == input
        end
        @test_cases begin
            input               | output
            :(Union{A, B} <: C) | TypeVarExpr{UnionExpr, Any, Any}(; typename=UnionExpr([:A, :B]), ub=:C)
            @test from_expr(TypeVarExpr{UnionExpr}, input) == output
            @test to_expr(output) == input
        end
        @test_cases begin 
            input               | output 
            :( <: C )           | TypeVarExpr{MaybeProvided{Symbol}, Any, Any}(; typename=not_provided, ub=:C)
            @test from_expr(TypeVarExpr{MaybeProvided{Symbol}}, input) == output
            @test to_expr(output) == input
        end
    end
    @testset "CurlyExpr" begin 
        @test_cases begin 
            input               |   output 
            :(Union{A,B})       | CurlyExpr{:Union, Any}(Any[:A, :B])
            :(Union{})          | CurlyExpr{:Union, Any}(Any[])
            :(Union{A, F(B)})   | CurlyExpr{:Union, Any}(Any[:A, :(F(B))])
            :(F{A, G(B)})       | CurlyExpr{:F, Any}(Any[:A, :(G(B))])
            :(H.F{A, G(B)})     | CurlyExpr{Expr, Any}(Any[:(H.F), :A, :(G(B))])
            @test from_expr(CurlyExpr, input) == output
            @test to_expr(output) == input
        end
        @Test to_expr(CurlyExpr(:A)) == :(A{})
        @Test to_expr(CurlyExpr(:A, UnionExpr(; args=[:T, :B]))) == :(A{Union{T, B}})
        @Test to_expr(CurlyExpr(:A, UnionExpr(; args=[:T, :B]), :C)) == :(A{Union{T, B}, C})
        @Test to_expr(CurlyExpr(NestedDotExpr(; keys=[:A, :B]), :C)) == :(A.B{C})
        @Test to_expr(CurlyExpr(:T, TypeVarExpr(; typename=:A, ub=:B), TypeVarExpr(; typename=:C, lb=:D))) == :(T{A<:B, C >: D})
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
        f[] = NamedTupleArg(; key=:a, value=2, kw_head=false)
        @Test f[:a] == NamedTupleArg(; key=:a, value=2, kw_head=false)
        f[] = NamedTupleArg(; key=:c, kw_head=true)
        @Test to_expr(f) == :((a=2, b=f(x), c))
        @Test pop!(f) == NamedTupleArg(; key=:c, kw_head=true)
        @Test to_expr(f) == :((a=2, b=f(x)))
        f[] = NamedTupleArg(; key=:a, value=1, kw_head=false)
        @Test to_expr(f) == ex

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

        f = NamedTupleExpr()
        f[:a] = 1
        f[:b] = false 
        @Test to_expr(f) == :((a=1, b=false))
        delete!(f, :a)
        @Test to_expr(f) == :((b=false,))

    end

    @testset "Composite Exprs" begin 
        ex = :(key = (a=1, b=true))
        f = from_expr(ExprWOptionalRhs{Symbol}, ex)
        @Test propertynames(f) == (:lhs, :rhs)
        @Test f.lhs == :key 
        @Test f.rhs == ex.args[2]
        @Test to_expr(f) == ex 
     
        @testset "DestructuredAssigmentExpr" begin 
            ex = :(a, b, c = f(x))
            f = from_expr(DestructuredAssigmentExpr{Any}, ex)
            @Test f.lhs_names == [:a, :b, :c]
            @Test f.destructure_type == :tuple
            @Test f.rhs == :(f(x))
            @Test to_expr(f) == ex

            ex = :((a, b, c) = f(x))
            f = from_expr(DestructuredAssigmentExpr{Any}, ex)
            @Test f.lhs_names == [:a, :b, :c]
            @Test f.destructure_type == :eq_tuple
            @Test f.rhs == :(f(x))
            @Test to_expr(f) == ex

            f = from_expr(DestructuredAssigmentExpr{FuncCall}, ex)
            @Test f.lhs_names == [:a, :b, :c]
            @Test f.destructure_type == :eq_tuple
            @Test f.rhs == FuncCall(; funcname=:f, args=[FuncArg(:x)])
            @Test to_expr(f) == ex

            ex = :((; a,b,c) = f(x))

            f = from_expr(DestructuredAssigmentExpr{FuncCall}, ex)
            @Test f.lhs_names == [:a, :b, :c]
            @Test f.destructure_type == :eq_namedtuple
            @Test f.rhs == FuncCall(; funcname=:f, args=[FuncArg(:x)])
            @Test to_expr(f) == ex
        end
        
        @testset "ExprWOptions" begin 
            ex = :(key = (a=1, b=true))
            f = from_expr(ExprWOptions{Symbol}, ex)
            @Test propertynames(f) == (:inner_expr, :options, :lhs, :rhs)
            @Test f.lhs == :key 
            @Test f.inner_expr == from_expr(AssignExpr{Symbol, NamedTupleExpr}, ex)
            @Test f.options == from_expr(NamedTupleExpr, ex.args[2])
            @Test to_expr(f) == ex 
            @Test f[:a] == NamedTupleArg(; key=:a, value=1, kw_head=false)
            @Test f[:b] == NamedTupleArg(; key=:b, value=true, kw_head=false)
            @Test Set(keys(f)) == Set([:a, :b])
            @Test haskey(f, :a)
            @Test haskey(f, :c) == false
            f[:c] = true
            @Test haskey(f, :c) 
            @Test f[:c] == NamedTupleArg(; key=:c, value=true, kw_head=false)

            ex = :(key)
            f = from_expr(ExprWOptions{Symbol}, ex)
            @Test f.lhs == :key 
            @Test f.inner_expr == AssignExpr(:key, NamedTupleExpr(), false)
            @Test isempty(keys(f))
            @Test to_expr(f) == ex 
            f[:a] = 1 
            f[:b] = true 
            @Test to_expr(f) == :(key = (a=1, b=true))
        end
        @testset "PairExpr" begin
            ex = :(A => B)
            f = from_expr(PairExpr{Symbol, Symbol}, ex)
            @Test f.lhs == :A
            @Test f.rhs == :B
            @Test to_expr(f) == ex 

            ex = :(f(A) => "f(A)")
            f = from_expr(PairExpr{FuncCall, String}, ex)
            @Test f.lhs == (from_expr(FuncCall, :(f(A))))
            @Test f.rhs == "f(A)"
            @Test to_expr(f) == ex 
        end
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
                input                                                | output
                :(key1 = (expected_type = Bool, default=false))      | MacroUtilities.KVSpecExpr(; key=:key1, expected_types=Set([:Bool]), default_value=false)
                :(key1 = (expected_types = [Bool, Int, Symbol]))     | MacroUtilities.KVSpecExpr(; key=:key1, expected_types=Set([:Bool, :Int, :Symbol]), default_value=MacroUtilities.not_provided)
                @test isequal(from_expr(MacroUtilities.KVSpecExpr, input), output)
            end
            @testthrows "Input expression `f(x)` is not a valid keyword argument specifier expression" from_expr(MacroUtilities.KVSpecExpr, :(f(x)); throw_error=true)
            @testthrows "Input expression `f(x)` is not a NamedTuple expression" from_expr(MacroUtilities.KVSpecExpr, :(key = f(x)), throw_error=true)
            @testthrows "Neither `expected_type` nor `expected_types` specified" from_expr(MacroUtilities.KVSpecExpr, :(key = ()), throw_error=true)
        end
      
    end
end