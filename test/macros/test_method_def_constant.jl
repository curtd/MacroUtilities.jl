function func1_for_constant end 

@Test !(Base.@isdefined constant_from_func1)

@method_def_constant func1_for_constant(::Val{::Symbol}) constant_from_func1

@Test Base.@isdefined constant_from_func1

function func2_for_constant end 

@method_def_constant func2_for_constant(::Type{String}, ::Val{::Symbol}, ::Type{<:Real}) constant_from_func2

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
end