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
Base.copy(x::T) where {T<:AbstractExpr} = T(x)

"""
    NamedTupleArg(; key::Symbol, value=not_provided, is_splat::Bool=false, kw_head::Bool)

    NamedTupleArg(key::Symbol; kw_head::Bool)

Matches a `key = value` or a `key` argument inside a `NamedTuple` expression
"""
Base.@kwdef struct NamedTupleArg <: AbstractExpr 
    key::Symbol
    value::Any = not_provided
    is_splat::Bool = false
    kw_head::Bool
end

NamedTupleArg(key::Symbol; kw_head::Bool) = NamedTupleArg(; key, kw_head)

"""
    NamedTupleArg(f::NamedTupleArg; [name, type, value, is_splat])

Returns a new copy of `f`, with optional `name`, `type`, `value`, or `is_splat` overridden by the keyword arguments. 
"""
function NamedTupleArg(f::NamedTupleArg; key::Symbol=f.key, value=deepcopy(f.value), is_splat::Bool=f.is_splat, kw_head::Bool=f.kw_head)
    return NamedTupleArg(key, value, is_splat, kw_head)
end

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
    NamedTupleExpr(args::Vector{NamedTupleArg})
    NamedTupleExpr(; args::Vector{NamedTupleArg})

Matches a (named) tuple expression

The `key`s and `value`s of this expression can be accessed, modified, and queried with `Base.getindex`, `Base.setindex!`, `Base.iterate`, `Base.findfirst`, `Base.keys`, `Base.haskey`, `Base.length`, `Base.pop!`, and `Base.push!`
"""
Base.@kwdef struct NamedTupleExpr <: AbstractExpr 
    args::Vector{NamedTupleArg} = NamedTupleArg[]
end

function NamedTupleExpr(f::NamedTupleExpr; args::Vector{NamedTupleArg}=[copy(arg) for arg in f.args])
    return NamedTupleExpr(args)
end

"""
    NamedTupleExpr(names::Vector{Symbol}; kw_head::Bool=false) 

Returns a `NamedTupleExpr` built from `names`. 

If `kw_head == true`, this expression is of the form `(; names...)`, and otherwise
it is of the form `(names...)`
"""
NamedTupleExpr(names::Vector{Symbol}; kw_head::Bool=false) = NamedTupleExpr([NamedTupleArg(; key=name, kw_head=kw_head) for name in names])

function NamedTupleExpr(arg1::Union{Symbol,Pair{Symbol, <:Any}}, args::Union{Symbol, Pair{Symbol, <:Any}}...; kw_head::Bool=false)
    output_args = NamedTupleArg[]
    if arg1 isa Symbol 
        push!(output_args, NamedTupleArg(; key=arg1, kw_head=kw_head))
    else
        push!(output_args, NamedTupleArg(; key=first(arg1), value=last(arg1), kw_head=kw_head))
    end
    for arg in args 
        if arg isa Symbol 
            push!(output_args, NamedTupleArg(; key=arg, kw_head=kw_head))
        else
            push!(output_args, NamedTupleArg(; key=first(arg), value=last(arg), kw_head=kw_head))
        end
    end
    return NamedTupleExpr(output_args)
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

struct KeyEquals
    ref_key::Symbol
end
(k::KeyEquals)(arg::NamedTupleArg) = arg.key == k.ref_key

function Base.findfirst(f, ex::NamedTupleExpr)
    for (i,arg) in enumerate(ex.args)
        f(arg) && return i 
    end
    return nothing
end

function Base.getindex(f::NamedTupleExpr, key::Symbol)
    index = findfirst(KeyEquals(key), f)
    if !isnothing(index)
        return f[index]
    end
    error("NamedTupleExpr does not have key `$key`")
end

Base.haskey(f::NamedTupleExpr, key::Symbol) = !isnothing(findfirst(KeyEquals(key) , f))

for f in (:length, :iterate, :pop!)
    @eval Base.$f(f::NamedTupleExpr) = Base.$f(f.args) 
end
for f in (:iterate, :getindex, :push!, :deleteat!)
    @eval Base.$f(f::NamedTupleExpr, arg) = Base.$f(f.args, arg) 
end

function Base.delete!(f::NamedTupleExpr, key::Symbol)
    index = findfirst(KeyEquals(key), f)
    if !isnothing(index)
        deleteat!(f.args, index)
        return nothing
    else
        error("NamedTupleExpr does not have key `$key`")
    end
end

Base.setindex!(f::NamedTupleExpr, val::NamedTupleArg, i::Int) = Base.setindex!(f.args, val, i)

function Base.setindex!(f::NamedTupleExpr, value, key::Symbol) 
    index = findfirst(KeyEquals(key), f)
    rhs = NamedTupleArg(; key=key, value=value, kw_head=false)
    if isnothing(index)
        return push!(f, rhs)
    else
        return f[index] = rhs
    end
end

function Base.setindex!(f::NamedTupleExpr, val::NamedTupleArg)
    index = findfirst(KeyEquals(val.key), f)
    if isnothing(index)
        return push!(f, val)
    else
        return f[index] = val
    end
end

