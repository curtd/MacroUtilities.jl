function replace_symbols(x::Symbol, replace_values::Vector{<:Pair{Symbol,<:Any}}) 
    for (old,new) in replace_values
        if x === old 
            return new, true
        end
    end
    return x, false
end
replace_symbols(x, replace_values) = (x, false)

"""
    replace_symbols(expr, replace_values::Vector{<:Pair{Symbol, <:Any}}) -> (new_expr, was_replaced::Bool)

For each `x_old => x_new` in `replace_values`, recursively replace each instance of `x_old` in any argument of `expr` with `x_new`
"""
function replace_symbols(x::Expr, replace_values::Vector{<:Pair{Symbol,<:Any}})
    replaced = false
    new_expr = Expr(x.head)
    for arg in x.args 
        new_arg, arg_replaced = replace_symbols(arg, replace_values)
        push!(new_expr.args, new_arg)
        replaced |= arg_replaced
    end
    return new_expr, replaced
end


"""
    ReplaceSymbols(expr::Expr, to_replace::Set{Symbol})

    ReplaceSymbols(expr::Expr, to_replace::Symbol...)

Functional form of `replace_symbols`, i.e., given a set of `Symbol`s `x_old` in `to_replace`, 

this type acts as a function of the form `(x_old1 => x_new1, ..., x_oldk => x_newk) -> Expr`, where each instance of `x_oldj` in `expr` is replaced with `x_newj`, applied recursively. 

# Examples 
```julia-repl 
julia> f = ReplaceSymbols(:(g(_)::T), :_);

julia> f(:_ => :x)
:(g(x)::T)
```
"""
struct ReplaceSymbols
    expr
    to_replace::Set{Symbol}
    function ReplaceSymbols(expr, to_replace::Set{Symbol})
        _, was_arg_replaced = replace_symbols(expr, [r => r for r in to_replace])
        !was_arg_replaced && return nothing 
        return new(expr, to_replace)
    end
end

ReplaceSymbols(expr, to_replace::Symbol...) = ReplaceSymbols(expr, Set(to_replace))

function (r::ReplaceSymbols)(new_to_replace::Pair{Symbol, <:Any}...)
    replace_values = Pair{Symbol, Any}[]
    for p in new_to_replace
        if first(p) in r.to_replace
            push!(replace_values, Pair{Symbol, Any}(first(p), last(p)))
        end
    end
    return replace_symbols(r.expr, replace_values)[1]
end
