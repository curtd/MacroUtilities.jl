inner_param(::Type{Val{S}}) where {S} = S 
inner_param(::Type{MIME{S}}) where {S} = S 
inner_param(@nospecialize x) = x

get_constant_func((@nospecialize x)) = nothing
constant_key_type((@nospecialize x)) = Any 

"""
    default_extract_const_expr(constants) -> Expr 

Returns a `Expr` which generates a tuple from the given constants
"""
default_extract_const_expr(constants) = Expr(:tuple, map( eltype(constants) === Symbol ? QuoteNode : identity, constants)...)

function method_def_constants_expr( ref_method, get_constant_method; map_expr=nothing, ValType::Union{Symbol, Expr}=:Val, concrete_type_matches_only::Bool=false, _sourceinfo::Union{Nothing, LineNumberNode}=nothing)
    @nospecialize
    if isnothing(map_expr)
        map_expr = :($default_extract_const_expr)
    end
    f = from_expr(FuncCall, ref_method)
    nargs = length(f.args)
    val_arg_index::Union{Nothing,Int} = nothing 
    local val_arg_expr::CurlyExpr{ValType, Any}
    for i in 1:nargs 
        g = _from_expr(CurlyExpr{ValType, Any}, f.args[i].type)
        if !(g isa Exception)
            val_arg_expr = g
            val_arg_index = i 
            break
        end
    end
    (isnothing(val_arg_index) || length(val_arg_expr.args) != 1) && throw(@arg_error ref_method "Must be a function call signature of the form `f(args..., ::$ValType{::Type}, args...)`")
    if Meta.isexpr(val_arg_expr.args[1], :(::), 1)
        ConstantType = only(val_arg_expr.args[1].args)
    else
        throw(@arg_error ref_method "Must be a function call signature of the form `f(args..., ::$ValType{::Type}, args...)`")
    end

    ref_method_name = f.funcname
    
    _constant_method = gensym(get_constant_method)
    get_constant_where_params = [Symbol("T$i") for i in 1:nargs]
    get_constant_args = [FuncArg(; type=:(Type{$(get_constant_where_params[i])})) for i in 1:nargs if i != val_arg_index]
    get_constant_body = FuncCall(; funcname=_constant_method, 
        args=[FuncArg(; value=:(Tuple{$([ i == val_arg_index ? ValType : :(Type{$(get_constant_where_params[i])}) for i in 1:nargs]...)}))]
    )
    get_constant_method_def = FuncDef(; head=:(=), 
        header=FuncCall(; funcname=get_constant_method, args=get_constant_args), 
        whereparams= nargs > 1 ? [get_constant_where_params[i] for i in 1:nargs if i != val_arg_index] : not_provided,
        body=Expr(:block, _sourceinfo, to_expr(get_constant_body)), 
        line=something(_sourceinfo, not_provided))

    get_constant_method_def = __doc__macro(get_constant_method_def)

    methods_if_expr = concrete_type_matches_only ? :(all(Base.fieldtype(m.sig, i) != Any for i in 1:nargs if i != $val_arg_index)) : :true
    
    if VERSION ≥ v"1.10.0-DEV.609"
        __constant_method = Symbol(:_, _constant_method)
        constant_method_def = quote 
            function $__constant_method(world, source, T, self, _T)
                @nospecialize
                output = $ConstantType[$inner_param($Base.fieldtype(m.sig, $(val_arg_index+1))) for m in $Tricks._methods(typeof($ref_method_name), T, nothing, world) if $(methods_if_expr)]
                ci = $Tricks.create_codeinfo_with_returnvalue([Symbol("#self#"), :_T], [:T], (:T,), $(map_expr)(output))
                ci.edges = $Tricks._method_table_all_edges_all_methods(typeof($ref_method_name), T, world)
                return ci
            end

            @eval function $_constant_method((@nospecialize _T::Type{T})) where {T<:Tuple} 
                $(Expr(:meta, :generated, __constant_method))
                $(Expr(:meta, :generated_only))
            end
        end
    else
        if VERSION ≥ v"1.7"
            output_expr = :($(map_expr)(output))
        else
            output_expr = :(Expr(:tuple, map($(ConstantType === :Symbol ? :QuoteNode : :($Base.identity)), output)...))
        end
        constant_method_def = :(@inline @generated function $_constant_method(_T::Type{T}) where {T<:Tuple}
            $_sourceinfo
            output = $ConstantType[$inner_param($Base.fieldtype(m.sig, $(val_arg_index+1))) for m in $Tricks._methods(typeof($ref_method_name), T) if $(methods_if_expr)]
            ci = $Tricks.create_codeinfo_with_returnvalue([Symbol("#self#"), :_T], [:T], (:T,), $(output_expr))
            ci.edges = $Tricks._method_table_all_edges_all_methods(typeof($ref_method_name), T)
            return ci
        end)
    end

    return quote 
        if isnothing($get_constant_func($ref_method_name))
            $constant_method_def

            $(to_expr(get_constant_method_def))
            $MacroUtilities.get_constant_func(::typeof($ref_method_name)) = $_constant_method
            $MacroUtilities.constant_key_type(::typeof($ref_method_name)) = $ConstantType
        end
    end
end

"""
    @method_def_constant [ValType=Val] [map_expr] method_call get_constant_method

Given a `method_call` expression of the form `f(::Type{T1}, ::Type{T2}, ..., ::Type{Ti}, ::ValType{::S}, ::Type{Ti+1}, ..., ::Type{Tn})`, generates a method definition for `get_constant_method` which returns an iterable with element type `S` of all of the compile-time constants contained in the `ValType` parameter of each definition of `f`. 

`get_constant_method` is a generated function and thus incurs no runtime overhead, but also contains backedges to each method instance of `f`, and so the output is recompiled when a new method definition of `f` is added. 

`ValType` must be a singleton type with a single parameter. Define `MacroUtilities.inner_param` to extract the innermost type parameter for your own custom types. 

If `map_expr` is provided, it must resolve to a function mapping the collection of constants of type `Vector{S}` to a `Expr`. Defaults to `MacroUtilities.default_extract_const_expr`. (Requires at least Julia 1.7)

"""
macro method_def_constant(args...)
    length(args) ≥ 2 || error("Must provide at least two arguments")
    @parse_kwargs args[1:end-2]... begin 
        ValType::Union{Symbol, Expr, Nothing} = nothing
        map_expr::Union{Symbol, Expr, Nothing} = nothing
    end
    method_call, get_constant_method_name = args[end-1], args[end]
    if isnothing(ValType)
        ValType = :Val
    end
    return method_def_constants_expr(method_call, get_constant_method_name; ValType=ValType, map_expr=map_expr, _sourceinfo=__source__) |> esc
end
