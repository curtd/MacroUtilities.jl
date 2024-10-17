@testitem "@method_def_constant" setup=[TestSetup] begin 

    function func1_for_constant end 

    @Test !(Base.@isdefined constant_from_func1)

    @method_def_constant func1_for_constant(::Val{::Symbol}) constant_from_func1

    @Test Base.@isdefined constant_from_func1

    function func2_for_constant end 

    @method_def_constant func2_for_constant(::Type{String}, ::Val{::Symbol}, ::Type{<:Real}) constant_from_func2

    const test_map_expr = VERSION ≥ v"1.7"

    if test_map_expr
        function func3_for_constant end
        constant_expr_map(output) = to_expr(NamedTupleExpr([NamedTupleArg(; key, value=:($func3_for_constant(Val{$(QuoteNode(key))}())), kw_head=false) for key in output]))
        @method_def_constant map_expr=constant_expr_map func3_for_constant(::Val{::Symbol}) constants_from_func3
    end

    @testset "@method_def_constant" begin 
        @Test hasmethod(constant_from_func1, Tuple{})
        @Test constant_from_func1() == ()
        @inferred constant_from_func1()
        @eval func1_for_constant(::Val{:key1}) = nothing
        @Test constant_from_func1() == (:key1,)
        @eval func1_for_constant(::Val{:key2}) = nothing
        consts = constant_from_func1()
        @Test consts isa Tuple && eltype(consts) == Symbol && length(consts) == 2
        @Test Set(consts) == Set((:key2, :key1))

        @Test constant_from_func2(String, Float64) == ()
        @inferred constant_from_func2(String, Float64)
        @eval func2_for_constant(::Type{String}, ::Val{:key1}, ::Type{Float64}) = nothing
        @eval func2_for_constant(::Type{String}, ::Val{:key2}, ::Type{Float64}) = nothing
        consts = constant_from_func2(String, Float64)
        @Test consts isa Tuple && eltype(consts) == Symbol && length(consts) == 2
        @Test Set(consts) == Set((:key2, :key1))
        @Test isempty(constant_from_func2(String, Int))

        if test_map_expr
            consts = @inferred constants_from_func3()  
            @Test isempty(consts)
            @eval func3_for_constant(::Val{:key1}) = String 
            consts = @inferred constants_from_func3()  
            @Test consts == (key1=String,)
            @eval func3_for_constant(::Val{:key3}) = Int 
            consts = @inferred constants_from_func3()  
            @Test Set(keys(consts)) == Set([:key1, :key3])
            @Test Set(values(consts)) == Set([String, Int])
        end
        
    end
end