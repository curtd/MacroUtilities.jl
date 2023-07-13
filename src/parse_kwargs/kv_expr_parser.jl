Base.@kwdef mutable struct KVSpecExpr 
    key::Symbol
    expected_types::Set{Any} = Set()
    default_value::Any = not_provided
end

Base.:(==)(x::KVSpecExpr, y::KVSpecExpr) =  all(getfield(x,k) == getfield(y,k) for k in fieldnames(KVSpecExpr))

function parse_kv_spec_from_type_expr(expr)
    f = _from_expr(TypedVar, expr)
    f isa Exception && return f 
    key = f.name 
    expected_types = Set()
    @switch f.type begin 
        @case Expr(:curly, :Union, args...)
            for arg in args 
                push!(expected_types, arg)
            end
        @case Expr(:., arg1, arg2) 
            push!(expected_types, f.type)
        @case ::Symbol
            push!(expected_types, f.type)
        @case _ 
            return ArgumentError("Unknown Key-Value specification from type expression $(f.type)")
    end
    return KVSpecExpr(; key=key, expected_types=expected_types)
end

function _from_expr(::Type{KVSpecExpr}, expr)
    (_key, to_parse) = @switch expr begin 
        @case Expr(:(=), lhs, rhs)
            if lhs isa Symbol
                _kv = _from_expr(KVExpr, expr)
            else
                _kv = _from_expr(ExprWOptionalRhs{Expr}, expr)
            end
            _kv isa Exception && return _kv 
            to_parse = @switch _kv.lhs begin 
                @case Expr(:(::), arg1, arg2)
                    kv_spec = parse_kv_spec_from_type_expr(_kv.lhs)
                    if kv_spec isa KVSpecExpr
                        kv_spec.default_value = _kv.rhs 
                    end
                    return kv_spec
                @case ::Symbol 
                    _kv.rhs
                @case _ 
                    return ArgumentError("Invalid key-value specification in `$expr`")
                end
            (lhs, to_parse)
        @case Expr(:(::), lhs, rhs)
            return parse_kv_spec_from_type_expr(expr)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid keyword argument specifier expression")
    end
    args = _from_expr(NamedTupleExpr, to_parse)
    args isa Exception && return args 
    spec = KVSpecExpr(; key=_key)

    for kv in args 
        key = kv.key 
        value = kv.value

        is_not_provided(value) && return ArgumentError("Expected `key = value` expression, but got `$key` expression instead")
        if key ∉ allowed_kv_spec_keys 
            return ArgumentError("RHS key `$(key)` must be one of `$(allowed_kv_spec_keys)`")
        end
        if (key === :expected_type || key === :expected_types) 
            if !isempty(spec.expected_types)
                return ArgumentError("Cannot specify `expected_type` and `expected_types` keys simultaneously or multiple instances of each")
            end

            if key === :expected_type
                if value isa Symbol || value isa Expr
                    push!(spec.expected_types, value)
                else
                    return ArgumentError("In `expected_type = rhs` expression, rhs must be a `Symbol` or an `Expr`, got typeof(rhs) = $(typeof(value))")
                end
            else
                if value isa Expr
                    for t in _from_expr(Vector{Union{Symbol, Expr}}, value)
                        push!(spec.expected_types, t)
                    end
                else
                    return ArgumentError("In `expected_types = rhs` expression, rhs must be a list `Expr`, got typeof(rhs) = $(typeof(value))")
                end
            end
        elseif key === :default 
            if is_provided(spec.default_value)
                return ArgumentError("Cannot specify `default` key multiple times")
            end
            spec.default_value = value
        end
    end
    if isempty(spec.expected_types) 
        return ArgumentError("Neither `expected_type` nor `expected_types` specified")
    else
        return spec
    end
end

to_expr(spec::KVSpecExpr) = :(MacroUtilities.KVSpec(; key=$(QuoteNode(spec.key)), expected_types=Set([$(spec.expected_types...)]), default_value=$(is_not_provided(spec.default_value) ? :(MacroUtilities.not_provided) : spec.default_value )))

Base.@kwdef struct KVSpec
    key::Symbol
    expected_types::Set{Type}
    default_value::Any = not_provided
    function KVSpec(key::Symbol, expected_types::Set{<:Type}, default_value)
        (is_not_provided(default_value) || typeof(default_value) in expected_types) || error("Provided `default_value = $(default_value)` has type $(typeof(default_value)), which must be one of ($(join(string.(expected_types), ", ")))")

        return new(key, expected_types, default_value)
    end
end

function eltypes(spec::KVSpec)
    output = @NamedTuple{is_vector::Bool, type::Type}[]
    for type in spec.expected_types
        if type <: Vector 
            push!(output, (; is_vector=true, type=eltype(type)))
        elseif type <: Set 
            push!(output, (; is_vector=false, type=eltype(type)))
        end
    end
    return output
end

Base.@kwdef struct KVExprParser
    spec::Dict{Symbol, KVSpec} = Dict{Symbol, KVSpec}()
    allow_overwrite::Bool = false
    ignore_unknown_keys::Bool = false
    found_values::Dict{Symbol, Any} = Dict{Symbol, Any}()
end

function KVExprParser(specs::KVSpec...; allow_overwrite::Bool=false, ignore_unknown_keys::Bool=false)
    parser = KVExprParser(; allow_overwrite=allow_overwrite, ignore_unknown_keys=ignore_unknown_keys)
    for spec in specs
        haskey(parser.spec, spec.key) && throw(ArgumentError("Cannot specify key `$(spec.key)` multiple times"))
        parser.spec[spec.key] = spec
    end
    return parser
end