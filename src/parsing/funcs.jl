"""
    FuncArg(; name, type, value::Any, is_splat::Bool)

Matches a function argument expression

# Fields 
- `name::Union{NotProvided, Symbol} = not_provided`: Name of the argument
- `type::Union{NotProvided, Symbol, Expr} = not_provided`: Annotated type of the argument. Only valid when `value` is not provided. 
- `value::Any = not_provided`: Value of the argument. If `name` is provided, this corresponds to the default value of `name` in a method definition. Otherwise, this is the value passed as a function argument 
- `is_splat::Bool = false`: `true` if this argument is splatted, `false` otherwise
"""
Base.@kwdef struct FuncArg 
    name::Union{NotProvided, Symbol} = not_provided
    type::Union{NotProvided, Symbol, Expr} = not_provided
    value::Any = not_provided
    is_splat::Bool = false
    function FuncArg(name::Union{NotProvided, Symbol}, type::Union{NotProvided, Symbol, Expr}, value, is_splat::Bool)
        if is_not_provided(value)
            if is_not_provided(name)
                is_not_provided(type) && throw(ArgumentError("`name`, `type`, and `value` cannot all be simultaneously `not_provided`"))
                is_splat && throw(ArgumentError("Cannot have `is_splat` == true when `name` and `value` are `not_provided`"))
            end
        end
        return new(name, type, value, is_splat)
    end
end

"""
    FuncArg(f::FuncArg; [name, type, value, is_splat])

Returns a new copy of `f`, with optional `name`, `type`, `value`, or `is_splat` overridden by the keyword argumnets. 

"""
function FuncArg(f::FuncArg; name::Union{NotProvided, Symbol}=f.name, type::Union{NotProvided, Symbol, Expr}=( f.type isa Expr ? deepcopy(f.type) : f.type), value=deepcopy(f.value), is_splat::Bool=f.is_splat)
    return FuncArg(name, type, value, is_splat)
end

Base.copy(f::FuncArg) = FuncArg(f)

Base.:(==)(x::FuncArg, y::FuncArg) = all(getfield(x,k) == getfield(y,k) for k in fieldnames(FuncArg))

function Base.show(io::IO, f::FuncArg)
    print(io, "FuncArg")
    if is_not_provided(f.name)
        if is_not_provided(f.type)
            print(io, " - value = ", f.value)
        else
            print(io, " - ::", f.type)
        end
    else
        if f.is_splat
            print(io, " - ", f.name, "...")
        else
            print(io, " - ", f.name)
            if is_not_provided(f.type)
                if !(f.value === not_provided)
                    print(io, " = ", f.value)
                end
            else
                print(io, "::", f.type)
                if !(f.value === not_provided)
                    print(io, " = ", f.value)
                end
            end
        end
    end
end

function _from_expr(::Type{FuncArg}, expr)
    _assigned = from_expr(AssignExpr, expr; throw_error=false, allow_kw=true)
    if isnothing(_assigned)
        value = not_provided 
        arg_expr = expr
    else
        value = _assigned.rhs
        arg_expr = _assigned.lhs
    end
    (arg_expr, is_splat) = @switch arg_expr begin 
        @case Expr(:..., arg)
            (arg, true)
        @case _ 
            (arg_expr, false)
    end
    @switch arg_expr begin 
        @case :(::$type) && if (type isa Symbol || type isa Expr) end 
            return FuncArg(; name=not_provided, type, value, is_splat)
        @case :($name::$type) && if name isa Symbol && (type isa Symbol || type isa Expr) end 
            return FuncArg(; name, type, value, is_splat)
        @case ::Symbol
            return FuncArg(; name=arg_expr, type=not_provided, value, is_splat)
        @case _ 
            return FuncArg(; name = not_provided, type=not_provided, value=arg_expr, is_splat)
    end
end

function to_expr(arg::FuncArg; kw_head::Symbol=:kw)
    if is_not_provided(arg.type)
        if is_not_provided(arg.name)
            base_ex = arg.value 
        else
            if is_not_provided(arg.value)
                base_ex = arg.name
            else
                base_ex = Expr(kw_head, arg.name, arg.value)
            end
        end
    else
        if !(arg.name === not_provided)
            if !(arg.value === not_provided)
                base_ex = Expr(kw_head, :($(arg.name)::$(arg.type)), arg.value)
            else
                base_ex = :($(arg.name)::$(arg.type))
            end
        else
            base_ex = :(::$(arg.type))
        end
    end

    if arg.is_splat
        return Expr(:..., base_ex)
    else
        return base_ex
    end
end

"""
    FuncCall(; funcname, args, kwargs)

Matches a function call expression

# Fields 
- `funcname::Union{NotProvided, Symbol, Expr}`
- `args::Vector{FuncArg} = Vector{FuncArg}()`
- `kwargs::OrderedDict{Symbol, FuncArg} = OrderedDict{Symbol, FuncArg}()`
"""
Base.@kwdef struct FuncCall <: AbstractExpr
    funcname::Union{NotProvided, Symbol, Expr}
    args::Vector{FuncArg} = Vector{FuncArg}()
    kwargs::OrderedDict{Symbol, FuncArg} = OrderedDict{Symbol, FuncArg}()
end

"""
    FuncCall(f::FuncCall; [funcname, args, kwargs])

Returns a new copy of `f`, with optional `funcname`, `args`, or `kwargs` overridden by the keyword argumnets. 
"""
function FuncCall(f::FuncCall; funcname::Union{NotProvided, Symbol, Expr}=(f.funcname isa Expr ? deepcopy(f.funcname) : f.funcname), args::Vector{FuncArg} = [copy(arg) for arg in f.args], kwargs::OrderedDict{Symbol, FuncArg} = OrderedDict{Symbol, FuncArg}( k => copy(v) for (k,v) in pairs(f.kwargs)))
    return FuncCall(funcname, args, kwargs)
end

Base.:(==)(x::FuncCall, y::FuncCall) = all(getfield(x,k) == getfield(y,k) for k in fieldnames(FuncCall))

function _from_expr(::Type{FuncCall}, expr)
    (funcname, in_args) = @switch expr begin 
        @case Expr(:call, funcname, in_args...)
            (funcname, in_args)
        @case Expr(:tuple, in_args...) || Expr(:block, in_args...)
            (not_provided, in_args)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a function call expression")
    end
    in_args = [in_arg for in_arg in in_args if !(in_arg isa LineNumberNode)] 
    args = FuncArg[]
    kwargs = OrderedDict{Symbol, FuncArg}()

    if !isempty(in_args) 
        first_arg, rest = Iterators.peel(in_args)
        if Meta.isexpr(first_arg, :parameters)
            for kwarg in first_arg.args 
                parsed = _from_expr(FuncArg, kwarg)
                parsed isa Exception && return parsed 
                is_not_provided(parsed.name) && return ArgumentError("Expression `$kwarg` is not a valid keyword argument expression -- no argument name provided")
                kwargs[parsed.name] = parsed
            end
        else
            rest = in_args
        end

        for arg in rest
            parsed = _from_expr(FuncArg, arg)
            parsed isa Exception && return parsed 
            if is_not_provided(parsed.value) || is_not_provided(parsed.name)
                push!(args, parsed)
            else
                is_not_provided(parsed.name) && return ArgumentError("Expression `$arg` is not a valid keyword argument expression -- no argument name provided")
                kwargs[parsed.name] = parsed
            end
        end
    end
    return FuncCall(; funcname, args, kwargs) 
end

function to_expr(f::FuncCall)
    if is_not_provided(f.funcname)
        output = Expr(:tuple)
    else
        output = Expr(:call, f.funcname)
    end
    if !isempty(f.kwargs)
        push!(output.args, Expr(:parameters, [to_expr(v) for v in values(f.kwargs)]...))
    end
    for arg in f.args
        push!(output.args, to_expr(arg))
    end
    return output
end

"""
    map_args(f, expr::FuncCall) -> FuncCall
    map_args(f, expr::FuncDef) -> FuncCall

Transform the `expr` by applying `f(FuncArg) -> FuncArg` to each of its arguments
"""
function map_args(f, expr::FuncCall)
    new_args = map(f, expr.args)::Vector{FuncArg}
    return FuncCall(expr; args=new_args)
end

"""
    map_kwargs(f, expr::FuncCall) -> FuncCall
    map_kwargs(f, expr::FuncDef) -> FuncCall

Transform the `expr` by applying `f(FuncArg) -> FuncArg` to each of its keyword arguments
"""
function map_kwargs(f, expr::FuncCall)
    new_kwarg_vals = map(f, collect(values(expr.kwargs)))::Vector{FuncArg}
    any( is_not_provided(v.name) for v in new_kwarg_vals) && throw(ArgumentError("Cannot unset `name` in keyword argument map"))
    return FuncCall(expr; kwargs=OrderedDict{Symbol,FuncArg}( v.name => v for v in new_kwarg_vals))
end

function Base.show(io::IO, f::FuncCall)
    print(io, "FuncCall - ", to_expr(f))
end

"""
    FuncDef(; header, head, whereparams, return_type, body, line, doc)

Matches a function definition expression

# Fields 
- `header::FuncCall`
- `head::Symbol`
- `whereparams::Any = not_provided`
- `return_type::Any = not_provided`
- `body::Any`
- `line::Union{LineNumberNode, NotProvided} = not_provided`
- `doc::Union{String, NotProvided} = not_provided`
"""
Base.@kwdef struct FuncDef <: AbstractExpr
    header::FuncCall
    head::Symbol
    whereparams::Any = not_provided
    return_type::Any = not_provided
    body::Any
    line::Union{LineNumberNode, NotProvided} = not_provided
    doc::Union{String, NotProvided} = not_provided
end

"""
    FuncDef(f::FuncDef; [header, head, whereparams, return_type, body, line, doc)

Returns a new copy of `f`, with optional `header`, `head`, `whereparams`, `return_type`, `body`, `line`, or `doc` values overridden by the keyword arguments. 
"""
function FuncDef(f::FuncDef; header::FuncCall=FuncCall(f.header), head::Symbol=f.head, whereparams=deepcopy(f.whereparams), return_type=deepcopy(f.return_type), body=deepcopy(f.body), line::Union{LineNumberNode, NotProvided}=f.line, doc::Union{String, NotProvided}=f.doc)
    return FuncDef(header, head, whereparams, return_type, body, line, doc)
end

Base.:(==)(x::FuncDef, y::FuncDef) = all(getfield(x,k) == getfield(y,k) for k in fieldnames(FuncDef))

function map_args(f, expr::FuncDef)
    new_header = map_args(f, expr.header)
    return FuncDef(expr; header=new_header)
end

function map_kwargs(f, expr::FuncDef)
    new_header = map_kwargs(f, expr.header)
    return FuncDef(expr; header=new_header)
end

Base.propertynames(::FuncDef) = (:funcname, :args, :kwargs, :header, :head, :whereparams, :return_type, :body, :line, :doc)

function Base.getproperty(f::FuncDef, name::Symbol)
    if name in (:funcname, :args, :kwargs)
        return Base.getfield(getfield(f, :header), name)
    else
        return Base.getfield(f, name)
    end
end

function _from_expr(::Type{FuncDef}, expr)
    line = not_provided
    doc = not_provided
    if Meta.isexpr(expr, :block)
        length(expr.args) ≤ 2 || return ArgumentError("Input block expression `$expr` can have at most two block arguments")
        for arg in expr.args 
            if arg isa LineNumberNode
                line = arg 
            else
                m = _from_expr(MacroCall, arg)
                if m isa MacroCall && m.name == doc_macro.name && length(m.args) == 2
                    doc = m.args[1]::String 
                    expr = m.args[2]
                else 
                    expr = arg
                end
            end
        end
    end

    (head, args) = @switch expr begin
        @case Expr(head, args...) && if head in (:function, :->, :(=)) end 
            (head, args)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a function definition")
    end
    length(args) == 2 || return ArgumentError("Input expression `$expr` must have two sub-arguments")
    body = args[2]
    (call_expr, whereparams) = @switch args[1] begin 
        @case Expr(:where, func, params...) 
            (func, params)
        @case _ 
            (args[1], not_provided)    
    end
    (call_expr, return_type) = @switch call_expr begin 
        @case Expr(:(::), func, return_type)
            (func, return_type)
        @case _ 
            (call_expr, not_provided)
    end
    header = _from_expr(FuncCall, call_expr)
    header isa Exception && return header 
    if head === :-> && is_not_provided(line)
        line = something(first_lnn_in_block(call_expr), line)
    elseif head === :(=) && is_not_provided(line)
        line = something(first_lnn_in_block(body), line)
    end
    return FuncDef(; header, head, return_type, whereparams, body, line, doc)
end

function to_expr(f::FuncDef)
    if f.head === :-> 
        header_expr = Expr(:block)
        for arg in f.args 
            push!(header_expr.args, to_expr(arg))
        end
        if f.line isa LineNumberNode
            push!(header_expr.args, f.line)
        end
        for kwarg in values(f.kwargs)
            push!(header_expr.args, to_expr(kwarg; kw_head=:(=)))
        end
    else 
        header_expr = to_expr(f.header)
    end
    if !(f.return_type === not_provided)
        header_expr = Expr(:(::), header_expr, f.return_type)
    end
    if !(f.whereparams === not_provided)
        header_expr = Expr(:where, header_expr, f.whereparams...)
    end
    func_expr = Expr(f.head, header_expr, f.body)
    if f.doc isa String 
        func_expr = to_expr(doc_macro( f.line isa LineNumberNode ? f.line : nothing, f.doc, func_expr ))
        func_expr = Expr(:block, func_expr)
    end
    if f.line isa LineNumberNode && f.head === :function
        if !Meta.isexpr(func_expr, :block)
            func_expr = Expr(:block, func_expr)
        end
        pushfirst!(func_expr.args, f.line)
    end
    return func_expr
end

function Base.show(io::IO, f::FuncDef)
    print(io, "FuncDef - ", to_expr(f))
end