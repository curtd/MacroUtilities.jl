function _from_expr end

"""
    from_expr(::Type{T}, expr; throw_error::Bool=false, kwargs...)

Parses an `expr` into an object of type `T`

The provided `kwargs` are passed to the underlying parsing function. 

If `expr` cannot be parsed into an object of type `T`
    - if `throw_error == true`, throws an `ArgumentError`
    - otherwise, returns `nothing`  

====================

    from_expr(::Type{Vector{T}}, expr::Expr; throw_error::Bool=false)

Parses a `tuple` or `vect` `expr` as a `Vector{T}`. Each argument of `expr` must be of type `T`.
"""
function from_expr(::Type{T}, expr; throw_error::Bool=false, kwargs...) where {T}
    result = _from_expr(T, expr; kwargs...)
    if result isa T
        return result 
    elseif result isa Exception
        throw_error && throw(result)
    end
    return nothing
end

"""
    to_expr(x) -> Expr 

Converts `x` to an `Expr` representation
"""
function to_expr end


"""
    from_expr(Vector{T}, expr; [throw_error=false]) -> Union{Nothing, Vector{T}}
    
If `expr` is of the form `[arg1, arg2, ...]` or `(arg1, arg2, ...)` and each `argi` is of type `T`, returns `T[arg1, arg2...]`.

"""
function _from_expr(::Type{Vector{T}}, expr) where {T}
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
        @case _
            return ArgumentError("Input expression `$(expr)` is not a list expression")
    end
end

"""
    NotProvided

Placeholder type to represent the absence of a field.
"""
struct NotProvided end 

const not_provided = NotProvided()

Base.show(io::IO, ::NotProvided) = print(io, "not_provided")

"""
    is_not_provided(x) -> Bool 

Returns `true` if the field corresponding to `x` is not provided, `false` otherwise. 
"""
is_not_provided(::NotProvided) = true 
is_not_provided(x) = false