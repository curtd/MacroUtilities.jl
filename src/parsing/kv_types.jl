"""
    KVExpr(; key, value)

Matches expressions of the form `key::Symbol = value`
"""
Base.@kwdef struct KVExpr 
    key::Symbol
    value::Any
end

function _from_expr(::Type{KVExpr}, expr)
    @switch expr begin 
        @case (Expr(:(=), key, value) || Expr(:kw, key, value)) && if key isa Symbol end 
            return KVExpr(; key, value)
        @case _ 
            return ArgumentError("Expression `$expr` is not of the form `key = value`")
    end
end

to_expr(k::KVExpr) = Expr(:(=), k.key, k.value)

Base.@kwdef mutable struct KVSpecExpr 
    key::Symbol
    expected_types::Set{Any} = Set()
    default_value::Any = not_provided
end

function _from_expr(::Type{KVSpecExpr}, expr)
    _kv = _from_expr(KVExpr, expr)
    if _kv isa Exception
        return _kv
    else
        _key = _kv.key::Symbol
        _value = _kv.value
    end
    spec = KVSpecExpr(; key=_key)

    args = @switch _value begin 
        @case Expr(:tuple, args...) 
           args
        @case Expr(:(=), key, value)
            [_value]
        @case _ 
            return ArgumentError("In expression, `$(_kv.key) = rhs`, rhs (= `$(_kv.value)`) is not a valid key-value specifier expression")
    end
    for kwarg in args 
        kv = _from_expr(KVExpr, kwarg)
        if kv isa Exception 
            return kv 
        else
            key = kv.key
            value = kv.value
        end
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
                if kv.value isa Expr
                    for t in _from_expr(Vector{Union{Symbol, Expr}}, value)
                        push!(spec.expected_types, t)
                    end
                else
                    return ArgumentError("In `expected_types = rhs` expression, rhs must be a list `Expr`, got typeof(rhs) = $(typeof(value))")
                end
            end
        elseif key === :default 
            if !(spec.default_value === not_provided) 
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

Base.:(==)(x::KVSpecExpr, y::KVSpecExpr) = all(getfield(x,k) == getfield(y,k) for k in fieldnames(KVSpecExpr))

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
    parser = KVExprParser(; allow_overwrite, ignore_unknown_keys)
    for spec in specs
        haskey(parser.spec, spec.key) && throw(ArgumentError("Cannot specify key `$(spec.key)` multiple times"))
        parser.spec[spec.key] = spec
    end
    return parser
end