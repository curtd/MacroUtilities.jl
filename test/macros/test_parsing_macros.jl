@testitem "Parsing macros" setup=[TestSetup] begin 
    function test_unpack_options1(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options a::Int
        return a+1
    end
    function test_unpack_options2(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options b::Bool
        return !b
    end
    function test_unpack_options3(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options a::Int = 0
        return a
    end
    function test_unpack_options4(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options a::Union{Symbol, Int} = :abc
        return a
    end
    function test_unpack_options5(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options b::Bool = 1
        return !b
    end
    function test_unpack_options6(options)
        @unpack_option options a::Int 
        return a+1
    end
    function test_unpack_options7(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options a::Int 
        @unpack_option options b::Int 
        return a+b
    end
    function test_unpack_rename1(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options (a=>b)
        return (b, @isdefined a)
    end
    function test_unpack_rename2(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options (a=>b)::Int 
        return (b, @isdefined a)
    end
    function test_unpack_rename3(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option options (a=>b) = 1
        return b, @isdefined a
    end
    function test_unpack_options1throws(ex)
        options = from_expr(NamedTupleExpr, ex)
        @unpack_option should_throw=true options a::Int
        return a+1
    end

    
    macro ex_macro3(arg1)
        @assert_type arg1 Int
        return quote 
            arg1 = $arg1 
        end |> esc
    end

    function test_func_sub(ex)
        @switch ex begin 
            @case Expr(:(=), arg1, arg2)
                return (arg1, arg2)
            @case _ 
                return @arg_error ex "Uhoh"
        end
    end
    function test_func(ex)
        @return_if_exception arg1, arg2 = test_func_sub(ex)
        return (arg1, arg2)
    end

    function test_func2(ex)
        f = from_expr(NamedTupleExpr, ex)
        @return_if_exception @unpack_option f a::Int 
        return a+2
    end

    @testset "@return_if_exception" begin 
        y = test_func(:(a = b))
        @Test y == (:a, :b)
        @Test test_func(:a) isa ArgumentError
        
        y = test_func2(:(a = 1))
        @Test y == 3
        y = test_func2(:(a = false))
        @Test y isa ArgumentError
    end
    @testset "@unpack_option" begin 
        @test_cases begin 
            input              | result 
            :(a=1, b=false)    | 2
            :(b=false)         | ArgumentError("Option `a` not found in $(from_expr(NamedTupleExpr, :(b=false)))")
            :(a="abc", b=false)| ArgumentError("Option `a = abc` has type String, expected type Int")
            @test test_unpack_options1(input) == result
        end
        @test_cases begin 
            input              | result 
            :(a=1, b=false)    | true
            :(a=1)             | ArgumentError("Option `b` not found in $(from_expr(NamedTupleExpr, :(a=1)))")
            :(b="abc", a=false)| ArgumentError("Option `b = abc` has type String, expected type Bool")
            @test test_unpack_options2(input) == result
        end
        
        @test_cases begin 
            input              | result 
            :(a=1, b=false)    | 1
            :(b=false)         | 0
            :(a="abc", b=false)| ArgumentError("Option `a = abc` has type String, expected type Int")
            @test test_unpack_options3(input) == result
        end
        
        @test_cases begin 
            input              | result 
            :(a=1, b=false)    | 1
            :(a=ab, b=false)   | :ab
            :(b=false)         | :abc
            :(a="abc", b=false)|  ArgumentError("Option `a = abc` has type String, expected one of types (Symbol, Int)")
            @test test_unpack_options4(input) == result
        end
             
        @test_cases begin 
            input              | result 
            :(a=1, b=false)    | true
            :(a=ab)            | ArgumentError("Option `b` with default value `1` has type Int64, expected type Bool")
            :(a="abc", b="abc")|  ArgumentError("Option `b = abc` has type String, expected type Bool")
            @test test_unpack_options5(input) == result
        end

        @test_cases begin 
            input              | result 
            (input=(a=1, b=false), result=2)
            (input=(a=:ab, b=false), result= ArgumentError("Option `a = ab` has type Symbol, expected type Int"))
            (input=(b=false,), result = ArgumentError("Option `a` not found in (b = false,)"))
            @test test_unpack_options6(input) == result
        end
        @test_cases begin 
            input              | result 
            (input=:((a=1, b=2)), result=3)
            (input=:((a=ab, b=false)), result= ArgumentError("Option `a = ab` has type Symbol, expected type Int"))
            (input=:((b=false,)), result= ArgumentError("Option `a` not found in $(from_expr(NamedTupleExpr, :(b=false)))"))
            @test test_unpack_options7(input) == result
        end

        @testthrows "ArgumentError: Option `a = false` has type Bool, expected type Int" test_unpack_options1throws(:(a=false, b=false))
        @testthrows "ArgumentError: Option `a = false` has type Bool, expected type Int" test_unpack_options1throws(:(a=false, b=false))

        @testset "Variable renaming" begin 
            @test_cases begin 
                input              | result 
                :(a=1, b=false)    | 1
                :(a="abc")         | "abc"
                :(b = false)       | ArgumentError("Option `a` not found in $(from_expr(NamedTupleExpr, :(b=false)))")
                @test result isa ArgumentError ? test_unpack_rename1(input) == result : test_unpack_rename1(input) == (result, false)
            end
            @test_cases begin 
                input              | result 
                :(a=1, b=false)    | 1
                :(a="abc")         | ArgumentError("Option `a = abc` has type String, expected type Int")
                :(b = false)       | ArgumentError("Option `a` not found in $(from_expr(NamedTupleExpr, :(b=false)))")
                @test result isa ArgumentError ? test_unpack_rename2(input) == result : test_unpack_rename2(input) == (result, false)
            end
            @test_cases begin 
                input              | result 
                :(a=2, b=false)    | 2
                :(a="abc")         | "abc"
                :(b = false)       | 1
                @test result isa ArgumentError ? test_unpack_rename3(input) == result : test_unpack_rename3(input) == (result, false)
            end
        end
    end
    @testset "@assert_type" begin 
        a = 1
        @Test @assert_type a Int
        @Test @assert_type a (Int,Float64,String)
        @testthrows "Expected type of `a` to be Float64, got typeof(a) = Int64" @assert_type a Float64
        @testthrows "Expected type of `a` to be one of (Float64, String), got typeof(a) = Int64" @assert_type a (Float64, String)

        let 
            @ex_macro3 2
            @test arg1 == 2
        end

    
        @testthrows "Expected type of `arg1` to be Int, got typeof(arg1) = Float64" @eval module A 
            import ..@ex_macro3 
            @ex_macro3 1.0
        end
    

    end
end
