function _from_expr end

function _from_expr(::Type{Vector{T}}, expr::S) where {T, S}
    @switch expr begin 
        @case (Expr(:vect, args...) || Expr(:tuple, args...))
            output = T[]
            for arg in args 
                if arg isa T
                    push!(output, arg)
                else
                    return ArgumentError("Argument `$(arg)` in expression `$(expr)` was not of expected type $T")
                end
            end
            return output
        @case if S <: T end 
            return [expr]
        @case _
            return ArgumentError("Input expression `$(expr)` is not a list expression")
    end
end

function _from_expr(::Type{Symbol}, expr)
    if expr isa Symbol 
        return expr 
    else
        return ArgumentError("Input `$expr` is not a `Symbol`, got typeof(input) = $(typeof(expr))")
    end
end

_from_expr(::Type{Expr}, expr::Expr) = expr 
for T in (:Symbol, :Expr)
    @eval _from_expr(::Type{Union{Symbol, Expr}}, expr::$T) = expr
end

_from_expr(::Type{Any}, expr) = expr 
_from_expr(::Type{T}, expr::S) where {T, S<:T} = expr

"""
    from_expr(::Type{T}, expr; throw_error::Bool=false, kwargs...)

Parses an `expr` into an object of type `T`

The provided `kwargs` are passed to the underlying parsing function. 

If `expr` cannot be parsed into an object of type `T`
    - if `throw_error == true`, throws an `ArgumentError`
    - otherwise, returns `nothing`  

====================

    from_expr(::Type{Vector{T}}, expr::Expr; throw_error::Bool=false)

Parses a `tuple` or `vect` `expr` as a `Vector{T}`. Each argument of `expr` must be of type `T`

====================

    from_expr(::Type{Vector{T}}, input::T; throw_error::Bool=false)

Returns a singleton `Vector{T}` containing `input`

====================

    from_expr(::Type{FuncCall}, expr; normalize_kwargs::Bool=false)

Returns a parsed `FuncCall` from `expr`

If `normalize_kwargs = true`, trailing equality expressions (e.g., `f(a, b, c=1)` will be parsed as keyword arguments. 

"""
function from_expr(::Type{T}, expr; throw_error::Bool=false, kwargs...) where {T}
    result = _from_expr(T, expr; kwargs...)
    if result isa Exception 
        throw_error && throw(result)
    elseif (T <: Vector && result isa Vector && eltype(result) <: eltype(T)) || result isa T || (T isa UnionAll ) || (!isempty(T.parameters) && result isa T.name.wrapper)
        return result 
    end
    return nothing
end


"""
    to_expr(x) -> Expr 

Converts `x` to an `Expr` representation
"""
function to_expr(x)
    return :($x)
end
to_expr(x::Symbol) = QuoteNode(x)
to_expr(x::Expr) = x

function to_expr(v::Vector)
    return Expr(:vect, to_expr.(v)...)
end

function to_expr(d::Dict)
    K = keytype(d)
    V = valtype(d)
    return :(Dict{$K, $V}($([ Expr(:call, :(=>), to_expr(k), to_expr(v)) for (k,v) in pairs(d) ]...)))
end

to_expr_noquote(x) = to_expr(x)
to_expr_noquote(x::Symbol) = x