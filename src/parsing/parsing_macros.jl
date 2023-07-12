"""
    @return_if_exception(ex)

Evalutes the destructured assignment expression `ex` of the form `(; names...) = rhs` or `(names...) = rhs` and returns from the current function if `rhs` returns an `Exception`
"""
macro return_if_exception(ex)
    f = _from_expr(DestructuredAssigmentExpr{Any}, ex)
    f isa Exception && throw(f) 
    y = gensym("lhs")
    g = DestructuredAssigmentExpr{Any}(f.lhs_names, (f.destructure_type == :eq_namedtuple ? :eq_namedtuple : :eq_tuple), y)
    output = Expr(:block, :(local $y = $(to_expr(f.rhs))), :($(y) isa Exception && return $(y)), to_expr(g)) 
    return output |> esc
end
