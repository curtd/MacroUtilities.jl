"""
    @assert_type var types

Ensures that `typeof(var)` is in the list of provided `types`.

`types` can be either a single type or a `tuple` or `vect` expression of types
"""
macro assert_type(var, types)
    type_list = from_expr(Vector{Union{Symbol,Expr}}, types)
    type_list_str = join([string(t) for t in type_list], ", ")
    first_type, rest = Iterators.peel(type_list)
    type_valid = :($var isa $(first_type))
    type_list_valid = Expr(:block, :($(first_type) isa Type || throw(ArgumentError("Input `"*($(string(first_type)))*"` is not a valid type"))))
    for type in rest 
        push!(type_list_valid.args, :($(type) isa Type || throw(ArgumentError("Input `"*($(string(type)))*"` is not a valid type"))))
        type_valid = Expr(:||, :($var isa $(type)), type_valid)
    end
    var_str = string(var)
    if length(type_list) == 1
        assert_message = "Expected type of `$var_str` to be $(type_list_str), got typeof($var_str) = "
    else
        assert_message = "Expected type of `$var_str` to be one of ($(type_list_str)), got typeof($var_str) = "
    end

    output = Expr(:block, __source__, type_list_valid, :( $(type_valid) || throw(ArgumentError($assert_message * string(typeof($var)))) ) )
    return output |> esc
end