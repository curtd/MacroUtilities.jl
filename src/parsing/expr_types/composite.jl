"""
    AssignExpr{L,R}(; lhs::L, rhs::R, allow_kw)

Matches a `lhs = rhs` assignment expression. 

If `allow_key == true`, also matches an assignment expression with head `:kw`
"""
Base.@kwdef struct AssignExpr{L,R} <: AbstractExpr
    lhs::L 
    rhs::R
    allow_kw::Bool
end

function _from_expr(::Type{AssignExpr{L, R}}, expr; allow_kw::Bool=false) where {L, R}
    @switch expr begin 
        @case Expr(:(=), lhs_expr, rhs_expr) || (Expr(:kw, lhs_expr, rhs_expr) && if allow_kw end)
            lhs = _from_expr(L, lhs_expr)
            lhs isa Exception && return lhs 
            
            rhs = _from_expr(R, rhs_expr)
            rhs isa Exception && return rhs 

            return AssignExpr{L, R}(; lhs, rhs, allow_kw=expr.head === :kw)
        @case _ 
            return ArgumentError("Input expression `$expr` is not of the form `lhs = rhs`")
    end
end 

function to_expr(f::AssignExpr)
    lhs = to_expr_noquote(f.lhs)
    rhs = to_expr_noquote(f.rhs)
    if f.allow_kw
        return Expr(:kw, lhs, rhs)
    else
        return Expr(:(=), lhs , rhs)
    end
end

"""
    AssignExpr(f::AssignExpr{L,R}; [lhs::L], [rhs::R], [allow_kw::Bool])

Returns a new copy of `f`, with optional `lhs`, `rhs`, and `allow_kw` overriden by the keyword arguments.
"""
function AssignExpr(f::AssignExpr{L, R}; lhs::L=copy(f.lhs), rhs::R=copy(f.rhs), allow_kw::Bool=f.allow_kw) where {L, R}
    return AssignExpr(lhs, rhs, allow_kw)
end

"""
    PairExpr{L,R}(; lhs::L, rhs::R)

Matches an expression of the form `lhs => rhs` 
"""
Base.@kwdef struct PairExpr{L,R} <: AbstractExpr
    lhs::L 
    rhs::R
end

function _from_expr(::Type{PairExpr{L, R}}, expr) where {L, R}
    @switch expr begin 
        @case Expr(:call, :(=>), lhs_expr, rhs_expr)
            lhs = _from_expr(L, lhs_expr)
            lhs isa Exception && return lhs 
            
            rhs = _from_expr(R, rhs_expr)
            rhs isa Exception && return rhs 

            return PairExpr{L, R}(; lhs, rhs)
        @case _ 
            return ArgumentError("Input expression `$expr` is not of the form `lhs => rhs`")
    end
end 

function to_expr(f::PairExpr)
    lhs = to_expr_noquote(f.lhs)
    rhs = to_expr_noquote(f.rhs)
    return Expr(:call, :(=>), lhs, rhs)
end

"""
    PairExpr(f::PairExpr{L,R}; [lhs::L], [rhs::R])

Returns a new copy of `f`, with optional `lhs` and `rhs` overriden by the keyword arguments.
"""
function PairExpr(f::PairExpr{L, R}; lhs::L=copy(f.lhs), rhs::R=copy(f.rhs)) where {L, R}
    return PairExpr(lhs, rhs)
end

"""
    ExprWOptionalRhs{E <: AbstractExpr}(; lhs::E, default=not_provided)

Matches an expression of the form `lhs = default`, where `lhs` is matched by `E`, or an expression matched by `E`
"""
Base.@kwdef struct ExprWOptionalRhs{E} <: AbstractExpr
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
    ExprWOptions{E}(; inner_expr::AssignExpr{E, NamedTupleExpr})
    ExprWOptions(lhs::E, options::NamedTupleExpr; allow_kw::Bool=false)

Matches an expression of the form `lhs = (key1=value1, ...)` or `lhs`, where `lhs` matches `E`

The underlying options in the expression can be accessed/iterated over/modified via the `Base.haskey`, `Base.keys`, `BAse.iterate`, `Base.getindex`, and `Base.setindex!` functions.
"""
struct ExprWOptions{E <: Union{Symbol,AbstractExpr}} <: AbstractExpr
    inner_expr::AssignExpr{E, NamedTupleExpr}
end

function ExprWOptions(; lhs=not_provided, options=not_provided, inner_expr::MaybeProvided{AssignExpr{<:Any, NamedTupleExpr}})
    if is_provided(lhs) && is_provided(options) && options isa NamedTupleExpr
        return ExprWOptions(AssignExpr(lhs, options))
    elseif is_provided(inner_expr)
        return ExprWOptions(inner_expr)
    else
        throw(ArgumentError("Must provide either `lhs` and `options` or `inner_expr` keyword arguments"))
    end
end

ExprWOptions(lhs, options::NamedTupleExpr; allow_kw::Bool=false) = ExprWOptions(AssignExpr(lhs, options, allow_kw))

"""
    ExprWOptions(f::ExprWOptions{E}; [lhs::E], [rhs::NamedTupleExpr], [allow_kw::Bool])

Returns a new copy of `f`, with optional `lhs`, `rhs`, and `allow_kw` overridden by the keyword arguments.
"""
function ExprWOptions(f::ExprWOptions{E}; lhs::E=copy(f.lhs), rhs::NamedTupleExpr=copy(r.rhs), allow_kw::Bool=f.inner_expr.allow_kw) where {E}
    return ExprWOptions(lhs, rhs; allow_kw)
end

function _from_expr(::Type{ExprWOptions{E}}, expr; allow_kw::Bool=false) where {E}
    if (f = _from_expr(AssignExpr{E, NamedTupleExpr}, expr; allow_kw=allow_kw); !(f isa Exception))
        return ExprWOptions{E}(f)
    elseif (f = _from_expr(E, expr); !(f isa Exception))
        return ExprWOptions{E}(AssignExpr(f, NamedTupleExpr(), allow_kw))
    else
        return @arg_error expr " is neither an expression of type $E nor an expression of type $E with options"
    end
end

function to_expr(f::ExprWOptions)
    if !(isempty(f.rhs))
        return to_expr(f.inner_expr)
    else 
        return to_expr_noquote(f.inner_expr.lhs)
    end
end

Base.propertynames(::ExprWOptions) = (:inner_expr, :options, :lhs, :rhs)

function Base.getproperty(s::ExprWOptions, name::Symbol)
    if name in (:inner_expr,)
        return Base.getfield(s, name)
    elseif name == :options 
        return Base.getfield(Base.getfield(s, :inner_expr), :rhs)
    else
        return Base.getfield(Base.getfield(s, :inner_expr), name)
    end
end
Base.haskey(f::ExprWOptions, key::Symbol) = Base.haskey(f.options, key)
Base.keys(f::ExprWOptions) = Base.keys(f.options)
Base.iterate(f::ExprWOptions) = Base.iterate(f.options)
Base.iterate(f::ExprWOptions, state) = Base.iterate(f.options, state)
Base.getindex(f::ExprWOptions, k::Symbol) = Base.getindex(f.options, k)
Base.setindex!(f::ExprWOptions, arg::NamedTupleArg) = Base.setindex!(f.options, arg)
Base.setindex!(f::ExprWOptions, value, key::Symbol) = Base.setindex!(f.options, NamedTupleArg(; key=key, value=value, kw_head=false))

""" 
    KeyWOptions(; lhs::Symbol, options::NamedTupleExpr)

Matches an expression of the form `lhs = (key1=value1, ...)` or `lhs`
"""
const KeyWOptions = ExprWOptions{Symbol}

"""
    DestructuredAssigmentExpr{E}(; lhs_names::Vector{Symbol}, destructure_type::Symbol, rhs::E)

Matches an expression of the form `lhs = rhs`, where 

- if `destructure_type == :tuple`, `lhs` is of the form `Expr(:tuple, lhs_names[1:end-1]..., Expr(:(=), lhs_names[end], rhs)`
- if `destructure_type == :eq_tuple`, `lhs` is of the form `(lhs_names...) = rhs`
- if `destructure_type == :eq_namedtuple`, `lhs` is of the form `(; lhs_names...) = rhs`

"""
Base.@kwdef struct DestructuredAssigmentExpr{E} <: AbstractExpr 
    lhs_names::Vector{Symbol}
    destructure_type::Symbol
    rhs::E
end

"""
    DestructuredAssigmentExpr(f::DestructuredAssigmentExpr{E}; lhs_names::Vector{Symbol}, destructure_type::Symbol, rhs::E)

Returns a new copy of `f`, with optional `lhs_names`, `destructure_type`, and `rhs` overriden by the keyword arguments.
"""
function DestructuredAssigmentExpr(f::DestructuredAssigmentExpr{E}; lhs_names::Vector{Symbol}=copy(f.lhs_names), destructure_type::Symbol=f.destructure_type, rhs::E=copy(f.rhs)) where {E}
    return DestructuredAssigmentExpr(lhs_names, destructure_type, rhs) 
end

function _from_expr(::Type{DestructuredAssigmentExpr{E}}, expr) where {E}
    lhs_names = Symbol[]
    @switch expr begin 
        @case Expr(:(=), lhs_expr, rhs_expr)
            rhs = _from_expr(E, rhs_expr)
            rhs isa ArgumentError && return rhs 

            if lhs_expr isa Symbol
                lhs = lhs_expr
                push!(lhs_names, lhs)
                destructure_type = :eq_tuple
            else
                lhs = _from_expr(NamedTupleExpr, lhs_expr)
                isnothing(lhs) && return @arg_error expr "LHS must be a Symbol or a NamedTupleExpr, got $lhs" 
                isempty(lhs) && return @arg_error expr "LHS cannot be empty"
                for arg in lhs 
                    is_provided(arg.value) && return @arg_error expr arg "Cannot provide RHS value"
                    arg.is_splat && return @arg_error expr arg "Cannot be a splat argument"
                    push!(lhs_names, arg.key)
                end
                if all(arg.kw_head for arg in lhs)
                    destructure_type = :eq_namedtuple 
                else
                    destructure_type = :eq_tuple 
                end
            end
            return DestructuredAssigmentExpr(lhs_names, destructure_type, rhs)
        @case Expr(:tuple, args...)
            isempty(args) && return @arg_error expr "Tuple expr must not be empty"
            last_arg = args[end]
            f = _from_expr(ExprWOptionalRhs{Symbol}, last_arg)
            f isa ArgumentError && return f 
            is_not_provided(f.rhs) && return @arg_error expr last_arg "Last argument must be of the form `lhs = rhs`"
            rhs = _from_expr(E, f.rhs)
            rhs isa ArgumentError && return rhs 

            for arg in args[1:end-1]
                arg isa Symbol || return @arg_error expr arg "Argument must be a Symbol, got typeof(arg) = $(typeof(arg))"
                push!(lhs_names, arg)
            end
            push!(lhs_names, f.lhs)
            return DestructuredAssigmentExpr(lhs_names, :tuple, rhs)
        @case _ 
            return @arg_error expr "Expression is not a valid DestructuredAssigmentExpr{$E}"
    end
end

function to_expr(f::DestructuredAssigmentExpr)
    rhs = to_expr_noquote(f.rhs)
    if f.destructure_type in (:eq_namedtuple, :eq_tuple)
        if f.destructure_type == :eq_tuple && length(f.lhs_names) == 1 
            lhs = only(f.lhs_names)
        else
            lhs = to_expr(NamedTupleExpr(f.lhs_names; kw_head=(f.destructure_type == :eq_namedtuple)))
        end
        return Expr(:(=), lhs, rhs)
    else
        return Expr(:tuple, f.lhs_names[1:end-1]..., Expr(:(=), f.lhs_names[end], rhs))
    end
end