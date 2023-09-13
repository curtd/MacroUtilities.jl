@testset "Struct parsing" begin 
    @testset "StructDefHeader" begin 
        @test_cases begin 
            input                   |       output 
            :A                      | StructDefHeader(; typename=:A)
            :(A <: B)               | StructDefHeader(; typename=:A, supertype=:B)
            :(A{C} <: B)            | StructDefHeader(; typename=:A, parameters=[TypeVarExpr(; typename=:C)], supertype=:B)
            :(A{C <: D} <: B)       | StructDefHeader(; typename=:A, parameters=[TypeVarExpr(; typename=:C, ub=:D)], supertype=:B)
            :(A{C <: D} <: B)       | StructDefHeader(; typename=:A, parameters=[:(C <: D)], supertype=:B)
            :(A{C <: D, E} <: B)    | StructDefHeader(; typename=:A, parameters=[TypeVarExpr(; typename=:C, ub=:D), TypeVarExpr(; typename=:E)], supertype=:B)
            @test from_expr(StructDefHeader, input) == output
            @test to_expr(output) == input
        end
    end

    ex = quote 
        struct A end 
    end
    f = from_expr(StructDef, ex)
    @Test propertynames(f) == (:is_mutable, :header, :lnn, :fields, :constructors, :typename, :parameters, :supertype)
    @Test f.typename == :A
    @Test isempty(f.parameters)
    @Test is_not_provided(f.supertype)
    @Test f.is_mutable == false 
    @Test isempty(f.fields)
    @Test isempty(f.constructors)
    @Test f.lnn == ex.args[1]
    @Test MacroUtilities.free_params(f) |> isempty
    @Test to_expr(f) == ex


    ex = quote 
        mutable struct A{B<:T} <: C{T}
            key1::B
            key2::Vector{Any}
            key3
            A(key1::B) where {B} = new{B}(key1, Any[], key3)
            A() = new{Nothing}(nothing, Any[], nothing)
        end 
    end
    f = from_expr(StructDef, ex)
    @Test f.typename == :A
    @Test f.supertype == :(C{T})
    @Test f.parameters == [ TypeVarExpr(; typename=:B, ub=:T) ]
    @Test f.is_mutable == true 
    @Test f.fields == [(TypedVar(; name=:key1, type=:B), ex.args[2].args[3].args[1]), (TypedVar(; name=:key2, type=:(Vector{Any})), ex.args[2].args[3].args[3]), (TypedVar(; name=:key3), ex.args[2].args[3].args[5])]

    ref_constructor1 = FuncDef(; head=:(=), header=FuncCall(; funcname=:A, args=[FuncArg(; name=:key1, type=:B)]), whereparams=[:B], line=ex.args[2].args[3].args[7], body=ex.args[2].args[3].args[8].args[2])
    ref_constructor2 = FuncDef(; head=:(=), header=FuncCall(; funcname=:A,), line=ex.args[2].args[3].args[9], body=ex.args[2].args[3].args[10].args[2])
    
    @Test f.constructors == [(ref_constructor1, ex.args[2].args[3].args[7]), (ref_constructor2, ex.args[2].args[3].args[9])]

    @Test to_expr(f) == ex

    @Test MacroUtilities.free_params(f) == [:B]

    ex = quote 
        struct A{B<:T, S, F<:S} <: C{T}
            key1::B
            key2::Vector{S}
            key3::F
        end 
    end
    f = from_expr(StructDef, ex)
    @Test to_expr(f) == ex
    @Test Set(MacroUtilities.free_params(f)) == Set([:B, :S, :F])

end
@testset "Generalized Struct parsing" begin 
    A = ExprWOptions{TypedVar}
    B = @NamedTuple{constructors::FuncDef}
    ex = quote 
        struct A end 
    end
    f = from_expr(GeneralizedStructDef{A, B}, ex)
    @Test f.typename == :A 
    @Test isempty(keys(f.fields))
    @Test isempty(f.fields)
    @Test isempty(f.constructors)
    @Test to_expr(f) == ex

    ex = quote 
        mutable struct A{B<:T} <: C{T}
            key1::B = (option1=true, option2=1)
            key2::Vector{Any} = (default=Any[], option2=0)
            key3
            A(key1::B) where {B} = new{B}(key1, Any[], key3)
            A() = new{Nothing}(nothing, Any[], nothing)
        end 
    end
    f = from_expr(GeneralizedStructDef{A, B}, ex)
    @Test f.typename == :A 
    @Test keys(f.fields) == Set([:key1, :key2, :key3])
    @Test f.fields[:key1].options == NamedTupleExpr(:option1 => true, :option2 => 1)
    @Test f.fields[:key2].options == NamedTupleExpr(:default => :(Any[]), :option2 => 0)
    @Test f.fields[1].options == NamedTupleExpr(:option1 => true, :option2 => 1)
    @Test f.fields[2].options == NamedTupleExpr(:default => :(Any[]), :option2 => 0)
    @Test f.fields[3].options == NamedTupleExpr()

    ref_constructor1 = FuncDef(; head=:(=), header=FuncCall(; funcname=:A, args=[FuncArg(; name=:key1, type=:B)]), whereparams=[:B], line=ex.args[2].args[3].args[7], body=ex.args[2].args[3].args[8].args[2])
    ref_constructor2 = FuncDef(; head=:(=), header=FuncCall(; funcname=:A,), line=ex.args[2].args[3].args[9], body=ex.args[2].args[3].args[10].args[2])
    
    @Test f.constructors == [(ref_constructor1, ex.args[2].args[3].args[7]), (ref_constructor2, ex.args[2].args[3].args[9])]
    @Test to_expr(f) == ex

    f_plain = StructDef(f)
    @Test f_plain.typename == :A 
    @Test f_plain.fields[1] == (TypedVar(; name=:key1, type=:B), getfield(f, :fields)[1] |> last)
    @Test f_plain.fields[2] == (TypedVar(; name=:key2, type=:(Vector{Any})), getfield(f, :fields)[2] |> last)
    @Test f_plain.fields[3] == (TypedVar(; name=:key3, type=not_provided), getfield(f, :fields)[3] |> last)

    @Test f_plain.is_mutable == f.is_mutable
    @Test f_plain.header == f.header 
    @Test f_plain.lnn == f.lnn
    @Test f_plain.constructors == f.constructors
    
    g = map_fields(t->ExprWOptions(t; rhs=NamedTupleExpr()), f)
    @Test g.fields[:key1].options |> isempty 
    @Test g.fields[:key2].options |> isempty 
    @Test g.fields[:key3].options |> isempty 
    @Test all(isequal(getproperty(f, k), getproperty(g, k)) for k in propertynames(f) if k != :fields)


end