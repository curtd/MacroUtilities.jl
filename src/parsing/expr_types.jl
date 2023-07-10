"""
    IdentityMap()(x) -> x 

Maps `x` to itself
"""
struct IdentityMap end 

(::IdentityMap)(x) = x 

"""
    TakeLastMap()(args...) -> args[end]

Maps the list of input arguments to the last one
"""
struct TakeLastMap end 

(::TakeLastMap)(args...) = args[end]

abstract type AbstractExpr end

function Base.show(io::IO, ex::AbstractExpr)
    print(io, string(typeof(ex)), " - ", to_expr(ex))
end

Base.:(==)(x::T, y::T) where {T<:AbstractExpr} =  all(getfield(x,k) == getfield(y,k) for k in fieldnames(T))

"""
    BlockExpr(; args)

Matches a `:block` expression
"""
struct BlockExpr <: AbstractExpr
    args::Vector{Any}
    function BlockExpr(args::Vector{Any})
        return new(args)
    end
end

BlockExpr(arg) = BlockExpr(Any[arg])

function BlockExpr(arg1, arg2, args...) 
    expr = BlockExpr(arg1)
    push!(expr.args, arg2)
    for arg in args 
        push!(expr.args, arg)
    end
    return expr
end

function _from_expr(::Type{BlockExpr}, expr)
    @switch expr begin 
        @case Expr(:block, args...)
            return BlockExpr(convert(Vector{Any},collect(args)))
        @case _ 
            return ArgumentError("Input expression `$expr` is not a block `Expr`")
    end
end
to_expr(expr::BlockExpr) = Expr(:block, to_expr.(expr.args)...)

Base.vcat(expr::AbstractExpr, args...) = BlockExpr(expr, args...)

Base.push!(expr::BlockExpr, arg) = push!(expr.args, arg)

"""
    AssignExpr(; lhs, rhs, allow_kw)

Matches a `lhs = rhs` assignment expression. 

If `allow_key == true`, also matches an assignment expression with head `:kw`
"""
Base.@kwdef struct AssignExpr <: AbstractExpr
    lhs::Any 
    rhs::Any
    allow_kw::Bool
end

function _from_expr(::Type{AssignExpr}, expr; allow_kw::Bool=true)
    @switch expr begin 
        @case Expr(:(=), lhs, rhs) || (Expr(:kw, lhs, rhs) && if allow_kw end)
            return AssignExpr(; lhs, rhs, allow_kw)
        @case _ 
            return ArgumentError("Input expression `$expr` is not of the form `lhs = rhs`")
    end
end 

function to_expr(expr::AssignExpr)
    return Expr(expr.allow_kw ? :kw : :(=), to_expr_noquote(expr.lhs), to_expr(expr.rhs))
end

Base.@kwdef struct NamedTupleArg <: AbstractExpr 
    key::Symbol
    value::Any = not_provided
    is_splat::Bool = false
    kw_head::Bool
end

"""
    NamedTupleArg(f::NamedTupleArg; [name, type, value, is_splat])

Returns a new copy of `f`, with optional `name`, `type`, `value`, or `is_splat` overridden by the keyword argumnets. 

"""
function NamedTupleArg(f::NamedTupleArg; key::Symbol=f.key, value=deepcopy(f.value), is_splat::Bool=f.is_splat, kw_head::Bool=f.kw_head)
    return NamedTupleArg(key, value, is_splat, kw_head)
end

Base.copy(f::NamedTupleArg) = NamedTupleArg(f)

function _from_expr(::Type{NamedTupleArg}, expr; is_kw::Bool=false)    
    (key, value, is_splat, kw_head) = @switch expr begin 
        @case Expr(:(=), lhs, rhs) && if lhs isa Symbol end 
            (lhs, rhs, false, false)
        @case Expr(:kw, lhs, rhs) && if lhs isa Symbol end 
            (lhs, rhs, false, true)
        @case ::Symbol 
            (expr, not_provided, false, is_kw)
        @case Expr(:..., lhs) && if lhs isa Symbol end 
            (lhs, not_provided, true, is_kw)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a known NamedTuple argument")
    end
    return NamedTupleArg(key, value, is_splat, kw_head)
end

function to_expr(f::NamedTupleArg)
    expr = f.key
    if f.is_splat
        expr = Expr(:..., expr)
    elseif is_provided(f.value)
        expr = Expr(f.kw_head ? :kw : :(=) , f.key, f.value)
    end
    return expr
end

"""
    NamedTupleExpr(; args::Vector{NamedTupleArg})

Matches a named tuple expression
"""
Base.@kwdef struct NamedTupleExpr <: AbstractExpr 
    args::Vector{NamedTupleArg} = NamedTupleArg[]
end

function NamedTupleExpr(f::NamedTupleExpr; args::Vector{NamedTupleArg}=[copy(arg) for arg in f.args])
    return NamedTupleExpr(args)
end

function _from_expr(::Type{NamedTupleExpr}, expr)
    @switch expr begin 
        @case Expr(:(=), lhs, rhs) && if lhs isa Symbol end 
            arg = _from_expr(NamedTupleArg, expr; is_kw=false)
            arg isa Exception && return arg
            return NamedTupleExpr([arg])
        @case Expr(:tuple)
            return NamedTupleExpr()
        @case Expr(:tuple, arg1, args...)
            output_args = NamedTupleArg[]
            @switch arg1 begin 
                @case Expr(:parameters, kwargs...)
                    for kwarg in kwargs 
                        parsed = _from_expr(NamedTupleArg, kwarg; is_kw=true)
                        parsed isa Exception && return parsed 
                        push!(output_args, parsed)
                    end
                @case _
                    parsed = _from_expr(NamedTupleArg, arg1)
                    parsed isa Exception && return parsed 
                    push!(output_args, parsed)           
            end
            for arg in args
                parsed = _from_expr(NamedTupleArg, arg)
                parsed isa Exception && return parsed 
                push!(output_args, parsed)           
            end
            return NamedTupleExpr(output_args)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a NamedTuple expression")
    end
end

function to_expr(f::NamedTupleExpr)
    output = Expr(:tuple)
    remaining_args_start = 1 
    num_args = length(f.args)
    if num_args ≥ 1 && f.args[1].kw_head
        params = Expr(:parameters)
        for (i,arg) in enumerate(f.args)
            if !f.args[i].kw_head 
                remaining_args_start = i 
                break 
            end
            if i == num_args
                remaining_args_start = num_args+1
            end
            push!(params.args, to_expr(arg))
        end
        push!(output.args, params)
    end
    for arg in f.args[remaining_args_start:end]
        push!(output.args, to_expr(arg))
    end
    return output
end

Base.keys(f::NamedTupleExpr) = getproperty.(f.args, :key)

Base.getindex(f::NamedTupleExpr, i::Int) = f.args[i]

function Base.findfirst(f::NamedTupleExpr, key::Symbol)
    for (i,arg) in enumerate(f.args)
        arg.key == key && return i 
    end
    return nothing
end

Base.haskey(f::NamedTupleExpr, key::Symbol) = !isnothing(Base.findfirst(f, key))

function Base.getindex(f::NamedTupleExpr, key::Symbol)
    index = findfirst(f, key)
    if !isnothing(index)
        return f[index]
    end
    error("NamedTupleExpr does not have key `$key`")
end

for f in (:length, :iterate, :pop!)
    @eval Base.$f(f::NamedTupleExpr) = Base.$f(f.args) 
end
for f in (:iterate, :getindex, :push!)
    @eval Base.$f(f::NamedTupleExpr, arg) = Base.$f(f.args, arg) 
end

Base.setindex!(f::NamedTupleExpr, val::NamedTupleArg, i::Int) = Base.setindex!(f.args, val, i)

function Base.setindex!(f::NamedTupleExpr, val::NamedTupleArg)
    index = findfirst(f, val.key)
    if isnothing(index)
        return Base.push!(f, val)
    else
        error("Cannot set key `$(val.key)` in NamedTupleExpr -- key already exists")
    end
end

"""
    ExprWOptionalRhs{E <: AbstractExpr}(; lhs::E, default=not_provided)

Matches an expression of the form `lhs = default`, where `lhs` is matched by `E`, or an expression matched by `E`
"""
Base.@kwdef struct ExprWOptionalRhs{E <: Union{AbstractExpr, Symbol, Expr}} <: AbstractExpr
    lhs::E
    rhs::Any = not_provided
end

function _from_expr(::Type{ExprWOptionalRhs{E}}, expr) where {E}
    (lhs, rhs) = @switch expr begin 
        @case Expr(:(=), lhs, rhs)
            parsed_lhs = _from_expr(E, lhs)
            parsed_lhs isa Exception && return parsed_lhs
            (parsed_lhs, rhs)
        @case _ 
            parsed_lhs = _from_expr(E, expr)
            parsed_lhs isa Exception && return ArgumentError("Input expression `$expr` is neither a expression matching `$E = rhs` nor an expression matching $E")

            (parsed_lhs, not_provided)
    end
    return ExprWOptionalRhs{E}(; lhs, rhs)
end

function to_expr(f::ExprWOptionalRhs)
    expr = to_expr_noquote(f.lhs)
    if is_provided(f.rhs)
        expr = Expr(:(=), expr, f.rhs)
    end
    return expr
end

Base.propertynames(::ExprWOptionalRhs{Symbol}) = (:lhs, :rhs)
Base.propertynames(f::ExprWOptionalRhs{E}) where {E} = (:lhs, :rhs, propertynames(f.lhs)...)

function Base.getproperty(s::ExprWOptionalRhs, name::Symbol)
    if name in (:lhs, :rhs)
        return Base.getfield(s, name)
    else
        return Base.getfield(Base.getfield(s, :lhs), name)
    end
end

"""
    KVExpr(; lhs::Symbol, rhs)

Matches an expression of the form `lhs = rhs` or `lhs`
"""
const KVExpr = ExprWOptionalRhs{Symbol}

"""
    ExprWOptions{E}(; lhs::E, options::NamedTupleExpr)

Matches an expression of the form `lhs = (key1=value1, ...)` or `lhs`, where `lhs` matches `E`
"""
Base.@kwdef struct ExprWOptions{E <: Union{Symbol,AbstractExpr}} <: AbstractExpr
    lhs::E
    options::NamedTupleExpr
end

function _from_expr(::Type{ExprWOptions{E}}, expr) where {E}
    (lhs, options) = @switch expr begin 
        @case Expr(:(=), lhs, rhs)
            parsed_lhs = _from_expr(E, lhs)
            parsed_lhs isa Exception && return parsed_lhs
            parsed_rhs = _from_expr(NamedTupleExpr, rhs)
            parsed_rhs isa Exception && return parsed_rhs 
            (parsed_lhs, parsed_rhs)
        @case _ 
            parsed_lhs = _from_expr(E, expr)
            parsed_lhs isa Exception && return ArgumentError("Input expression `$expr` is neither a field type definition nor a field type with options definition")
            (parsed_lhs, NamedTupleExpr())
    end
    return ExprWOptions{E}(; lhs, options)
end

function to_expr(f::ExprWOptions)
    expr = to_expr_noquote(f.lhs)
    if is_provided(f.options)
        expr = Expr(:(=), expr, to_expr(f.options))
    end
    return expr
end

Base.propertynames(::ExprWOptions{Symbol}) = (:lhs, :options)
Base.propertynames(f::ExprWOptions{E}) where {E} = (:lhs, :options, propertynames(f.lhs)...)

function Base.getproperty(s::ExprWOptions, name::Symbol)
    if name in (:lhs, :options)
        return Base.getfield(s, name)
    else
        return Base.getfield(Base.getfield(s, :lhs), name)
    end
end

Base.keys(f::ExprWOptions) = Base.keys(f.options)
Base.getindex(f::ExprWOptions, k::Symbol) = Base.getindex(f.options, k)
Base.setindex!(f::ExprWOptions, arg::NamedTupleArg) = Base.setindex!(f.options, arg)

""" 
    KeyWOptions(; lhs::Symbol, options::NamedTupleExpr)

Matches an expression of the form `lhs = (key1=value1, ...)` or `lhs`
"""
const KeyWOptions = ExprWOptions{Symbol}

"""
    NestedDotExpr(; keys::Vector{Symbol})

Matches expressions of the form `A.B.C....`, with each symbol being stored in `keys`
"""
Base.@kwdef struct NestedDotExpr <: AbstractExpr
    keys::Vector{Symbol}
end

function _parse_dotted_expr!(result::Vector{Symbol}, expr)
    @switch expr begin 
        @case ::Symbol 
            push!(result, expr)
            return nothing
        @case ::QuoteNode && if expr.value isa Symbol end 
            push!(result, expr.value)
            return nothing
        @case Expr(:., arg1, arg2) && if arg2 isa QuoteNode end
            returned = _parse_dotted_expr!(result, arg1)
            returned isa Exception && return returned
            push!(result, arg2.value)
            return nothing
        @case _
            return ArgumentError("Expression $expr is an invalid dotted expression")
    end
end

function _from_expr(::Type{NestedDotExpr}, expr)
    if Meta.isexpr(expr, :.)
        result = Symbol[]
        returned = _parse_dotted_expr!(result, expr)
        returned isa Exception && return returned 
        return NestedDotExpr(result)
    else
        return ArgumentError("Input expression `$expr` is not a dot expression")
    end
end

function to_expr(f::NestedDotExpr)
    key1, rest = Iterators.peel(f.keys)
    key2, rest = Iterators.peel(rest)
    output = Expr(:., key1, QuoteNode(key2))
    for key in rest
        output = Expr(:., output, QuoteNode(key))
    end
    return output
end