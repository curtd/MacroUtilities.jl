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
    TypeVarExpr <: AbstractExpr

Matches expressions of the form
- `typename`
- `typename <: UB` 
- `typename >: LB`
- `LB <: typename <: UB`

## Constructors 
```
    TypeVarExpr{LB, UB}(; typename::Symbol, lb::Union{LB, NotProvided}=not_provided, ub::Union{UB, NotProvided}=not_provided)

    TypeVarExpr(typename::Symbol, [lb], [ub])

    TypeVarExpr{LB, UB}(t::TypeVarExpr)
```

"""
struct TypeVarExpr{LB, UB} <: AbstractExpr 
    typename::Symbol 
    lb::MaybeProvided{LB}
    ub::MaybeProvided{UB}
    function TypeVarExpr{LB, UB}(typename::Symbol, lb::MaybeProvided{LB}, ub::MaybeProvided{UB}) where {LB, UB}
        return new{LB, UB}(typename, lb, ub)
    end
end

function TypeVarExpr{LB, UB}(; typename::Symbol, lb::MaybeProvided{LB}=not_provided, ub::MaybeProvided{UB}=not_provided) where {LB, UB}
    return TypeVarExpr{LB, UB}(typename, lb, ub)
end

TypeVarExpr(; kwargs...) = TypeVarExpr{Any,Any}(; kwargs...)

function TypeVarExpr(typename::Symbol, lb=not_provided, ub=not_provided)
    if is_not_provided(lb)
        LB = Any 
    else
        LB = typeof(lb)
    end
    if is_not_provided(ub)
        UB = Any 
    else
        UB = typeof(ub)
    end
    return TypeVarExpr{LB, UB}(typename, lb, ub)
end

function _from_expr(::Type{TypeVarExpr{LB, UB}}, expr) where {LB, UB}
    @switch expr begin 
        @case Expr(:(<:), lhs, rhs) && if lhs isa Symbol end
            ub = _from_expr(UB, rhs)
            ub isa Exception && return ub
            return TypeVarExpr{LB, UB}(; typename=lhs, ub)
        @case Expr(:(>:), lhs, rhs) && if lhs isa Symbol end 
            lb = _from_expr(LB, rhs)
            lb isa Exception && return lb
            return TypeVarExpr{LB, UB}(; typename=lhs, lb)
        @case Expr(:comparison, lhs, :(<:), typename, :(<:), rhs) && if typename isa Symbol end
            lb = _from_expr(LB, lhs)
            lb isa Exception && return lb
            ub = _from_expr(UB, rhs)
            ub isa Exception && return ub
            return TypeVarExpr{LB, UB}(; typename, lb, ub)
        @case ::Symbol 
            return TypeVarExpr{LB, UB}(; typename=expr)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid TypeVarExpr{$LB, $UB}")
    end
end

_from_expr(::Type{TypeVarExpr}, expr) = _from_expr(TypeVarExpr{Any,Any}, expr)

function to_expr(t::TypeVarExpr)
    if is_provided(t.lb)
        if is_provided(t.ub)
            return Expr(:comparison, to_expr_noquote(t.lb), :(<:), t.typename, :(<:), to_expr_noquote(t.ub))
        else
            return Expr(:(>:), t.typename, to_expr_noquote(t.lb))
        end
    else
        if is_provided(t.ub)
            return Expr(:(<:), t.typename, to_expr_noquote(t.ub))
        else
            return t.typename
        end
    end
end

TypeVarExpr(t::TypeVarExpr{LB, UB}; typename=t.typename, lb::MaybeProvided{LB}=copy_value(t.lb), ub::MaybeProvided{UB}=copy_value(t.ub)) where {LB, UB} = TypeVarExpr{LB, UB}(typename, lb, ub)

TypeVarExpr{LB, UB}(t::TypeVarExpr{LB, UB}) where {LB, UB} = TypeVarExpr(t)

"""
    CurlyExpr{FirstArg, E} <: AbstractExpr

Matches an expression of the form `FirstArg{args...}`, if `FirstArg` is a `Symbol`, or `args[1]{args[2:end]...}` otherwise.

## Constructors 
```
    CurlyExpr{FirstArg, E}(; args::Vector{E}) 
```
"""
Base.@kwdef struct CurlyExpr{FirstArg, E} <: AbstractExpr 
    args::Vector{E} = E[]
end

function _from_expr(::Type{CurlyExpr{FirstArg, E}}, expr) where {FirstArg, E}
    @switch expr begin 
        @case Expr(:curly, first_arg, args...) && if first_arg == FirstArg end 
            output = E[]
            for arg in args 
                e = _from_expr(E, arg)
                e isa Exception && return e
                push!(output, e)
            end
            return CurlyExpr{FirstArg, E}(output)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid CurlyExpr{$FirstArg} expression")
    end
end

function _from_expr(::Type{CurlyExpr{Symbol, E}}, expr) where {E}
    @switch expr begin 
        @case Expr(:curly, first_arg, args...) && if first_arg isa Symbol end 
            output = E[]
            for arg in args 
                e = _from_expr(E, arg)
                e isa Exception && return e
                push!(output, e)
            end
            return CurlyExpr{first_arg, E}(output)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid CurlyExpr{$FirstArg} expression")
    end
end

function _from_expr(::Type{CurlyExpr{E}}, expr) where {E}
    @switch expr begin 
        @case Expr(:curly, args...)
            output = E[] 
            for arg in args 
                e = _from_expr(E, arg)
                e isa Exception && return e
                push!(output, e)
            end
            return CurlyExpr{Expr, E}(output)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid CurlyExpr{$E} expression")
    end
end

function _from_expr(::Type{CurlyExpr}, expr)
    @switch expr begin 
        @case Expr(:curly, first_arg, args...)
            output = convert(Vector{Any}, collect(args))
            if first_arg isa Symbol 
                FirstArg = first_arg 
            else
                FirstArg = Expr
                pushfirst!(output, first_arg)
            end
            return CurlyExpr{FirstArg, Any}(output)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid CurlyExpr expression")
    end
end

first_arg(f::CurlyExpr{Expr, E}) where {E} = first(f.args)
first_arg(::CurlyExpr{FirstArg, E}) where {FirstArg, E} = FirstArg

to_expr(f::CurlyExpr{FirstArg, E}) where {FirstArg, E} = Expr(:curly, FirstArg, map(to_expr_noquote, f.args)...)
to_expr(f::CurlyExpr{Expr, E}) where {E} = Expr(:curly, map(to_expr_noquote, f.args)...)

CurlyExpr(f::CurlyExpr{FirstArg, E}; args::Vector{E}=copy_value(f.args), first_arg=FirstArg) where {FirstArg, E} = CurlyExpr{first_arg, E}(; args)

"""
    CurlyExpr(first_arg::Symbol, args...)
"""
function CurlyExpr(first_arg::Symbol, args...) 
    E = mapfoldl(typeof, promote_type, args; init=Union{})
    return CurlyExpr{first_arg, E}(convert(Vector{E}, collect(args)))
end

function CurlyExpr(args...) 
    E = mapfoldl(typeof, promote_type, args, init=Union{})
    return CurlyExpr{Expr, E}(convert(Vector{E}, collect(args)))
end
"""
    UnionExpr(; args::Vector{Union{Symbol, Expr}})

Matches an expression of the form `Union{T1, T2, ...}`
"""
const UnionExpr = CurlyExpr{:Union, Union{Symbol, Expr}}

const TypeExpr{E} = CurlyExpr{:Type, E}

"""
    NamedTupleArg(; key::Symbol, value=not_provided, is_splat::Bool=false, kw_head::Bool)

    NamedTupleArg(key::Symbol; kw_head::Bool)

Matches a `key = value` or a `key` argument inside a `NamedTuple` expression

If `kw_head == true`, the expression head is set to `:kw`, otherwise `:(=)`
"""
Base.@kwdef struct NamedTupleArg <: AbstractExpr 
    key::Symbol
    value::Any = not_provided
    is_splat::Bool = false
    kw_head::Bool
end

NamedTupleArg(key::Symbol; kw_head::Bool) = NamedTupleArg(; key, kw_head)

"""
    NamedTupleArg(f::NamedTupleArg; [key::Symbol], [value], [is_splat::Bool], [kw_head])

Returns a new copy of `f`, with optional `name`, `type`, `value`, or `is_splat` overridden by the keyword arguments. 
"""
function NamedTupleArg(f::NamedTupleArg; key::Symbol=f.key, value=copy_value(f.value), is_splat::Bool=f.is_splat, kw_head::Bool=f.kw_head)
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

unwrap_value((@nospecialize x)) = x 
unwrap_value(x::NamedTupleArg) = x.value

"""
    NamedTupleExpr(args::Vector{NamedTupleArg})
    NamedTupleExpr(; args::Vector{NamedTupleArg})

Matches a (named) tuple expression

The `key`s and `value`s of this expression can be accessed, modified, and queried with `Base.getindex`, `Base.setindex!`, `Base.iterate`, `Base.findfirst`, `Base.keys`, `Base.haskey`, `Base.length`, `Base.pop!`, and `Base.push!`
"""
Base.@kwdef struct NamedTupleExpr <: AbstractExpr 
    args::Vector{NamedTupleArg} = NamedTupleArg[]
end

function NamedTupleExpr(f::NamedTupleExpr; args::Vector{NamedTupleArg}=[copy_value(arg) for arg in f.args])
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

for with_func in (false, true)
    @eval begin 
        """
            Base.findfirst(f, ex::NamedTupleExpr) -> Union{Nothing, Int}

        Returns the first index `i` such that `f(::NamedTupleArg) -> Bool` returns `true`, or `nothing` if no such index exists. 
        """
        function Base.findfirst($(with_func ? :(f::Function) : :f), ex::NamedTupleExpr)
            for (i,arg) in enumerate(ex.args)
                f(arg) && return i 
            end
            return nothing
        end
    end
end

"""
    Base.getindex(f::NamedTupleExpr, key::Symbol) -> NamedTupleArg

Returns the `NamedTupleArg` associated with `key`, or throws an `ErrorException` if none exists
"""
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

"""
    Base.setindex!(f::NamedTupleExpr, val::NamedTupleArg, i::Int)
"""
Base.setindex!(f::NamedTupleExpr, val::NamedTupleArg, i::Int) = Base.setindex!(f.args, val, i)

"""
    Base.setindex!(f::NamedTupleExpr, value, key::Symbol)

Sets the `NamedTupleArg` in `f` with `key`, `value` in `f`
"""
function Base.setindex!(f::NamedTupleExpr, value, key::Symbol) 
    index = findfirst(KeyEquals(key), f)
    rhs = NamedTupleArg(; key=key, value=value, kw_head=false)
    if isnothing(index)
        return push!(f, rhs)
    else
        return f[index] = rhs
    end
end

"""
    Base.setindex!(f::NamedTupleExpr, arg::NamedTupleArg)

Sets the `NamedTupleArg` in `f` with key `arg.key` to `arg`
"""
function Base.setindex!(f::NamedTupleExpr, arg::NamedTupleArg)
    index = findfirst(KeyEquals(arg.key), f)
    if isnothing(index)
        return push!(f, arg)
    else
        return f[index] = arg
    end
end

