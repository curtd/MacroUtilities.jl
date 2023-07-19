inner_param(::Type{Val{S}}) where {S} = S 
inner_param(::Type{MIME{S}}) where {S} = S 

get_constant_func((@nospecialize x)) = nothing
constant_key_type((@nospecialize x)) = Any 

function method_def_constants_expr( ref_method, get_constant_method, ValType::Union{Symbol, Expr}=:Val; transform_expr=nothing, concrete_type_matches_only::Bool=false, _sourceinfo::Union{Nothing, LineNumberNode}=nothing, ValueType=:Any)
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
        line=something(_sourceinfo, not_provided), 
        return_type=:(Vector{$ConstantType}))

    get_constant_method_def = __doc__macro(get_constant_method_def)

    output_expr = :($ConstantType[$inner_param(Base.fieldtype(m.sig,$(val_arg_index+1))) for m in $Tricks._methods(typeof($ref_method_name), T) if $(concrete_type_matches_only ? :(all(Base.fieldtype(m.sig, i) != Any for i in 1:nargs if i != $val_arg_index)) : true)])
    if !isnothing(transform_expr)
        output_expr = transform_expr(output_expr)
    end
    if ConstantType === :Symbol
        returnvals = :(QuoteNode.(output))
    else
        returnvals = :(output)
    end

    return quote 
        if isnothing($get_constant_func($ref_method_name))
            @inline @generated function $_constant_method(_T::Type{T}) where {T<:Tuple}
                $_sourceinfo
                output = $output_expr
                ci = $Tricks.create_codeinfo_with_returnvalue([Symbol("#self#"), :_T], [:T], (:T,), Expr(:vect, ($returnvals)...))
                ci.edges = $Tricks._method_table_all_edges_all_methods(typeof($ref_method_name), T)
                return ci
            end

            $(to_expr(get_constant_method_def))
            $MacroUtilities.get_constant_func(::typeof($ref_method_name)) = $_constant_method
            $MacroUtilities.constant_key_type(::typeof($ref_method_name)) = $ConstantType
        end
    end
end

"""
    @method_def_constant [ValType=Val] method_call get_constant_method

Given a `method_call` expression of the form `f(::Type{T1}, ::Type{T2}, ..., ::Type{Ti}, ::ValType{::S}, ::Type{Ti+1}, ..., ::Type{Tn})`, generates a method definition for `get_constant_method` which returns a `Vector{S}` of all of the compile-time constants contained in the `ValType` parameter of each definition of `f`. 

`get_constant_method` is a generated function and thus incurs no runtime overhead, but also contains backedges to each method instance of `f`, and so the output is recompiled when a new method definition of `f` is added. 

`ValType` must be a singleton type with a single parameter. Define `MacroUtilities.inner_param` to extract the innermost type parameter for your own custom types. 

*Note*: This macro is a no-op unless the current Julia version is < 1.10
"""
macro method_def_constant(args...)
    @static if VERSION < v"1.10"
        length(args) ≥ 1 || error("Must provide at least one argument")
        @parse_kwargs args[1] begin 
            ValType::Union{Symbol, Expr, Nothing} = nothing
        end
        if isnothing(ValType)
            ValType = :Val
            length(args) == 2 || error("Must provide exactly two arguments if `ValType` is not provided")
            method_call, get_constant_method_name = args
        else
            length(args) == 3 || error("Must provide exactly three arguments when `ValType` is provided")
            method_call, get_constant_method_name = args[2], args[3]
        end
        return method_def_constants_expr(method_call, get_constant_method_name, ValType) |> esc
    else 
        return :(nothing)
    end
end
