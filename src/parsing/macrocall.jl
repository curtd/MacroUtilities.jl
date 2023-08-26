"""
    MacroCall(; name::Union{Symbol, Expr, GlobalRef}, line::Union{LineNumberNode, NotProvided} = not_provided, args::Vector{Any} = [])

Matches a macro call expression. A `MacroCall` object can be applied to one or more expressions, yielding another `MacroCall`. 
"""
Base.@kwdef struct MacroCall <: AbstractExpr
    name::Union{Symbol, QuoteNode, Expr, GlobalRef}
    line::Union{LineNumberNode, NotProvided} = not_provided
    args::Vector{Any} = Any[]
end

function Base.show(io::IO, m::MacroCall)
    print(io, "MacroCall - ", to_expr(m))
end

function _from_expr(::Type{MacroCall}, expr)
    if Meta.isexpr(expr, :macrocall) && length(expr.args) ≥ 2
        name = expr.args[1]
        if !(name isa Symbol || name isa Expr || name isa GlobalRef)
            return ArgumentError("In expression `$expr`, macro name `$name` must be a Symbol, Expr, or a GlobalRef, got typeof(name) = $(typeof(name))")
        end
        line = expr.args[2]
        if !(line isa LineNumberNode || isnothing(line))
            return ArgumentError("In expression `$expr`, macro line number `$line` must be `nothing` or a `LineNumberNode`, got typeof(line) = $(typeof(line))")
        end
        line = something(line, not_provided)
        args = convert(Vector{Any}, expr.args[3:end])
        return MacroCall(; name, line, args)
    else 
        return ArgumentError("Input expression `$expr` is not a valid macro invocation")
    end
end

function to_expr(m::MacroCall)
    return Expr(:macrocall, m.name, is_not_provided(m.line) ? nothing : m.line, to_expr_noquote.(m.args)...)
end

const doc_macro_arg = GlobalRef(Core, Symbol("@doc"))

"""
    doc_macro

A `MacroCall` corresponding to `Core.@doc`
"""
const doc_macro = MacroCall(; name=doc_macro_arg)

"""
    __doc__macro

A `MacroCall` corresponding to `Core.@__doc__`
"""
const __doc__macro = MacroCall(; name=GlobalRef(Core, Symbol("@__doc__")))

function (m::MacroCall)(expr, exprs...)
    new_args = copy_value(m.args)
    add_expr = true
    line = m.line
    if m.name == doc_macro.name
        if is_not_provided(line)
            if expr isa FuncDef
                line = expr.line
            elseif expr isa LineNumberNode
                line = expr 
                add_expr = false
            elseif isnothing(expr)
                line = not_provided 
                add_expr = false
            else
                line = not_provided 
            end
        end
    end
    if add_expr
        push!(new_args, expr, exprs...)
    else
        push!(new_args, exprs...)
    end
    return MacroCall(m.name, line, new_args)
end

@static if isdefined(Base, Symbol("@assume_effects"))
    const assume_effects = MacroCall(; name=Expr(:., :Base, QuoteNode(Symbol("@assume_effects"))))
    const assume_foldable = assume_effects(QuoteNode(:foldable))
else
    const assume_effects = TakeLastMap()
    const assume_foldable = IdentityMap()
end

"""
    assume_effects

A `MacroCall` corresponding to `Base.@assume_effects`
"""
assume_effects

"""
    assume_foldable

A `MacroCall` corresponding to `Base.@assume_effects :foldable`
"""
assume_foldable

"""
    DocExpr{E}(; line::LineNumberNode, docstr::Union{String, Expr}, expr::E)

Matches a standard documented expression `expr`, e.g.,

```julia
"documentation"
expr
```
"""
Base.@kwdef struct DocExpr{E} <: AbstractExpr 
    line::LineNumberNode
    docstr::Union{String, Expr}
    expr::E
end

function _from_expr(::Type{DocExpr{E}}, expr) where {E}
    if Meta.isexpr(expr, :macrocall) && length(expr.args) == 4 && expr.args[1] isa GlobalRef && expr.args[1].mod == doc_macro_arg.mod && expr.args[1].name === doc_macro_arg.name && expr.args[2] isa LineNumberNode && (expr.args[3] isa String || expr.args[3] isa Expr)
        inner_expr = _from_expr(E, expr.args[4])
        inner_expr isa ArgumentError && return inner_expr
        line = expr.args[2]
        docstr = expr.args[3]
        return DocExpr(line, docstr, inner_expr)
    else
        return @arg_error expr "Must be a `Core.@doc` macro"
    end
end

function to_expr(m::DocExpr)
    return Expr(:macrocall, doc_macro_arg, m.line, m.docstr, to_expr_noquote(m.expr))
end