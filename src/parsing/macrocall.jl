"""
    MacroCall(; name::Union{Symbol, Expr, GlobalRef}, line::Union{LineNumberNode, NotProvided} = not_provided, args::Vector{Any} = [])

Matches a macro call expression. A `MacroCall` object can be applied to one or more expressions, yielding another `MacroCall`. 
"""
Base.@kwdef struct MacroCall 
    name::Union{Symbol, QuoteNode, Expr, GlobalRef}
    line::Union{LineNumberNode, NotProvided} = not_provided
    args::Vector{Any} = Any[]
end

function Base.show(io::IO, m::MacroCall)
    print(io, "MacroCall - ", to_expr(m))
end

Base.:(==)(x::MacroCall, y::MacroCall) = all(getfield(x,k) == getfield(y,k) for k in fieldnames(MacroCall))

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

"""
    doc_macro

A `MacroCall` corresponding to `Core.@doc`
"""
const doc_macro = MacroCall(; name=GlobalRef(Core, Symbol("@doc")), line=not_provided, args=[])

function (m::MacroCall)(expr, exprs...)
    new_args = copy(m.args)
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
                line = nothing 
                add_expr = false
            else
                line = nothing 
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