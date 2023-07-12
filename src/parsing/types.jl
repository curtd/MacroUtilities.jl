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

"""
    is_provided(d) -> Bool 

Returns `true` if the field corresponding to `x` is provided, `false` otherwise. 
"""
is_provided(x) = !is_not_provided(x)

const MaybeProvided{T} = Union{NotProvided, T}
