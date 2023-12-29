"""
    StructDefHeader(; typename, parameters, supertype)

Matches the header of a struct definition

# Fields 
- `typename::Symbol`
- `parameters::Vector{TypeVarExpr} = TypeVarExpr[]`
- `supertype::Union{Symbol, Expr, NotProvided} = not_provided`
"""
struct StructDefHeader <: AbstractExpr 
    typename::Symbol 
    parameters::Vector{SymbolTypeVar}
    supertype::Union{Symbol, Expr, NotProvided}
end

function StructDefHeader(; typename::Symbol, parameters::Vector{<:Any}=SymbolTypeVar[], supertype::Union{Symbol, Expr, NotProvided}=not_provided)
    if typeof(parameters) <: Vector{<:SymbolTypeVar}
        params = parameters
    else
        params = SymbolTypeVar[]
        for input in parameters 
            if input isa SymbolTypeVar 
                push!(params, input)
            else
                push!(params, from_expr(SymbolTypeVar, input; throw_error=true)) 
            end
        end
    end
    return StructDefHeader(typename, params, supertype)
end

function _from_expr(::Type{StructDefHeader}, expr)
    (rest, supertype) = @switch expr begin 
        @case Expr(:(<:), rest, supertype)
            (rest, supertype)
        @case _ 
            (expr, not_provided)
    end
    if rest isa Symbol 
        typename = rest
        parameters = TypeVarExpr[]
    else
        curly = _from_expr(CurlyExpr{Symbol, TypeVarExpr}, rest); 
        typename = first_arg(curly)
        parameters = curly.args
    end
    return StructDefHeader(; typename, parameters, supertype)
end

function to_expr(h::StructDefHeader)
    expr = h.typename
    if !isempty(h.parameters)
        expr = Expr(:curly, h.typename, map(to_expr_noquote, h.parameters)...)
    end
    if is_provided(h.supertype)
        expr = Expr(:(<:), expr, h.supertype)
    end
    return expr 
end

"""
    StructDefHeader(f::StructDefHeader; [typename, parameter, supertype])

Returns a new copy of `f`, with optional `typename`, `parameter`, or `supertype` overridden by the keyword arguments.

"""
function StructDefHeader(f::StructDefHeader; typename::Symbol=f.typename, parameters::Vector{<:Any}=[copy_value(p) for p in f.parameters], supertype::Union{Symbol, Expr, NotProvided}= copy_value(f.supertype))
    return StructDefHeader(typename, parameters, supertype)
end


function struct_data(::Type{NamedTuple{K, V}}) where {K, V}
    return (; (K[i] => Vector{Tuple{fieldtype(V, i), MaybeProvided{LineNumberNode}}}() for i in 1:length(K) )...)
end

struct_data_types(::Type{NamedTuple{K, V}}) where {K, V} = (; (K[i] => fieldtype(V, i) for i in 1:length(K))...)

struct_data_type(::Type{NamedTuple{K, V}}) where {K, V} = NamedTuple{K, Tuple{(Vector{Tuple{fieldtype(V, i), MaybeProvided{LineNumberNode}}} for i in 1:length(K))...}}

function copy_struct_data(::Type{C}, x) where {C}
    new_data = struct_data(C)
    for (key, vals) in pairs(x)
        new_vals = new_data[key]
        for val in vals 
            expr, lnn = val
            push!(new_vals, (copy_value(expr), lnn::MaybeProvided{LineNumberNode}))
        end
    end
    return new_data
end

function free_params(expr)
    @switch expr begin 
        @case ::Symbol 
            return Symbol[expr]
        @case Expr(:curly, outer, inner_args...)
            output = Symbol[]
            for arg in inner_args
                out_args = free_params(arg)
                !isnothing(out_args) && append!(output, out_args)
            end
        @case Expr(:(<:), lhs, rhs) || Expr(:(>:), lhs, rhs)
            return free_params(lhs)
        @case _ 
            return nothing 
    end
end

free_params(f::StructDefHeader) = Symbol[ param.typename for param in f.parameters ]

function typename_w_params(f::StructDefHeader)
    _params = free_params(f)
    isempty(_params) && return f.typename
    return Expr(:curly, f.typename, _params...)
end

whereparams(f::StructDefHeader) = isempty(f.parameters) ? not_provided : map(to_expr_noquote,f.parameters)

"""
    StructDef(; is_mutable, header, lnn, fields, constructors)

Matches a struct definition. The properties `.typename`, `.parameter`, and `.supertype` forward to the `header` field.

# Fields 
- `is_mutable::Bool`
- `header::StructDefHeader`
- `lnn::Union{LineNumberNode, NotProvided}` = not_provided
- `fields::Vector{Tuple{TypedVar, LineNumberNode}}`
- `constructors::Vector{Tuple{FuncDef, LineNumberNode}}`
"""
Base.@kwdef struct StructDef <: AbstractExpr 
    is_mutable::Bool 
    header::StructDefHeader 
    lnn::MaybeProvided{LineNumberNode} = not_provided
    fields::Vector{Tuple{TypedVar, LineNumberNode}}
    constructors::Vector{Tuple{FuncDef, LineNumberNode}}
end

function Base.show(io::IO, def::StructDef)
    print(io, "StructDef\n\n", to_expr(def))
end

Base.propertynames(::StructDef) = (:is_mutable, :header, :lnn, :fields, :constructors, :typename, :parameters, :supertype)

function Base.getproperty(s::StructDef, name::Symbol)
    if name in (:typename, :parameters, :supertype)
        return Base.getfield(Base.getfield(s, :header), name)
    else
        return Base.getfield(s, name)
    end
end

"""
    StructDef(f::StructDef; [is_mutable, header, fields, constructors])

Returns a new copy of `f`, with optional `is_mutable`, `header`, `fields`, or `constructors` overridden by the keyword arguments.

"""
function StructDef(f::StructDef; is_mutable::Bool=f.is_mutable, header::StructDefHeader=StructDefHeader(f.header), lnn::Union{LineNumberNode, NotProvided}=f.lnn, fields::Vector{Tuple{TypedVar, LineNumberNode}} = [(copy_value(_field), lnn) for (_field, lnn) in f.fields], constructors::Vector{Tuple{FuncDef, LineNumberNode}} = [(copy_value(_def), lnn) for (_def, lnn) in f.constructors])
    return StructDef(is_mutable, header, lnn, fields, constructors)
end

function _structdef_expr(expr)
    @switch expr begin 
        @case Expr(:block, arg1, arg2) && if arg1 isa LineNumberNode end && if arg2 isa Expr end 
            return (arg1, arg2)
        @case Expr(:struct, args...) 
            return (not_provided, expr)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid struct definition expression")
    end
end

function _parse_struct_expr(expr)
    @switch expr begin 
        @case Expr(:struct, is_mutable, header_expr, body_expr) && if is_mutable isa Bool end 
            return (is_mutable, header_expr, body_expr)
        @case _ 
            return @arg_error expr "is not a valid struct definition expression"
    end
end

function _parse_struct_body(body, ::Type{FieldType}, ::Type{AdditionalFieldTypes}) where {FieldType, AdditionalFieldTypes <: NamedTuple}
    num_args = length(body.args)
    fields = Tuple{FieldType, MaybeProvided{LineNumberNode}}[]
    additional_exprs = struct_data(AdditionalFieldTypes)
    additional_expr_types = struct_data_types(AdditionalFieldTypes)

    local first_lnn::LineNumberNode
    local lnn::MaybeProvided{LineNumberNode}
    i = 1 
    finished_parsing_fields = false
    while i ≤ num_args
        arg = body.args[i]
        if arg isa LineNumberNode 
            lnn = arg 
            if !(Base.@isdefined first_lnn)
                first_lnn = arg
            end
            if i < num_args 
                i += 1
                arg = body.args[i]
            else
                break 
            end
        else
            return @arg_error body "In struct body, expected a LineNumberNode but got $arg with type $(typeof(arg)) instead"
        end
        if !finished_parsing_fields
            _field = _from_expr(FieldType, arg)
            if !(_field isa Exception)
                push!(fields, (_field, lnn))
            else
                finished_parsing_fields = true 
            end
        end
        if finished_parsing_fields
            could_parse = false
            for (key, S) in pairs(additional_expr_types)
                _expr = _from_expr(S, arg)
                if !(_expr isa Exception)
                    could_parse = true
                    push!(additional_exprs[key], (_expr, lnn))
                    break
                end
            end
            !could_parse && return @arg_error arg "Could not parse expression as a field definition of type $FieldType nor as one of expression types $(additional_expr_types)"
        end
        i += 1
    end
    return first_lnn, fields, additional_exprs
end

function _from_expr(::Type{StructDef}, expr)
    local global_lnn
    @return_if_exception lnn, structdef_expr = _structdef_expr(expr)
    if is_provided(lnn)
        global_lnn = lnn
    end
    @return_if_exception is_mutable, header_expr, body_expr = _parse_struct_expr(structdef_expr)
    @return_if_exception header = _from_expr(StructDefHeader, header_expr)
    @return_if_exception body = _from_expr(BlockExpr, body_expr)
    @return_if_exception first_body_lnn, fields, additional_exprs = _parse_struct_body(body, TypedVar, NamedTuple{(:constructors,), Tuple{FuncDef}})
    if !(Base.@isdefined global_lnn)
        global_lnn = first_body_lnn
    end
    return StructDef(; is_mutable, lnn=global_lnn, header, fields, additional_exprs.constructors)
end

function to_expr(f::StructDef)
    body = Expr(:block)
    if !isempty(f.fields)
        for _field_def in f.fields
            _field, lnn = _field_def
            if is_provided(lnn)
                push!(body.args, lnn)
            end
            push!(body.args, to_expr(_field))
        end
        for _def in f.constructors
            _constructor, lnn = _def 
            if is_provided(lnn)
                push!(body.args, lnn)
            end
            push!(body.args, to_expr(_constructor))
        end
    elseif is_provided(f.lnn)
        push!(body.args, f.lnn)
    end
    result = Expr(:struct, f.is_mutable, to_expr(f.header), body)
    if is_provided(f.lnn)
        result = Expr(:block, f.lnn, result)
    end
    return result
end

for func in (:free_params, :typename_w_params, :whereparams)
    @eval $func(f::StructDef) = $func(f.header)
end

struct FieldView{F}
    fields::Vector{Tuple{F, MaybeProvided{LineNumberNode}}}
end

function Base.show(io::IO, f::FieldView{F}) where {F}
    print(io, "FieldView{$F}")
end

Base.length(f::FieldView) = Base.length(f.fields)

function Base.iterate(f::FieldView) 
    y = Base.iterate(f.fields)
    isnothing(y) && return y 
    result, state = y 
    return result[1], state
end

function Base.iterate(f::FieldView, old_state) 
    y = Base.iterate(f.fields, old_state)
    isnothing(y) && return y 
    result, state = y 
    return result[1], state
end
Base.getindex(f::FieldView, index::Int) = f.fields[index][1]

struct FieldNameEqual
    ref_field_name::Symbol
end
(f::FieldNameEqual)(vi) = struct_field_name(vi) == f.ref_field_name

for with_func in (false, true)
    @eval begin 
        function Base.findfirst($(with_func ? :(f::Function) : :f), v::FieldView)
            for (i,arg) in enumerate(v)
                f(arg)::Bool && return i 
            end
            return nothing
        end
    end
end
function Base.getindex(f::FieldView, key::Symbol)
    index = findfirst(FieldNameEqual(key), f)
    if !isnothing(index)
        return f[index]
    else
        error("FieldView does not have key `$key`")
    end
end

function Base.keys(f::FieldView)
    out_keys = Set{Symbol}()
    for field in f
        push!(out_keys, struct_field_name(field))
    end
    return out_keys
end

"""
    GeneralizedStructDef(; is_mutable, header, lnn, fields, constructors)

Matches a struct definition. The properties `.typename`, `.parameter`, and `.supertype` forward to the `header` field.

# Fields 
- `is_mutable::Bool`
- `header::StructDefHeader`
- `lnn::Union{LineNumberNode, NotProvided}` = not_provided
- `fields::Vector{Tuple{TypedVar, LineNumberNode}}`
- `constructors::Vector{Tuple{FuncDef, LineNumberNode}}`
"""
struct GeneralizedStructDef{StructDefFieldType, AdditionalFieldTypes, StructDataType} <: AbstractExpr 
    is_mutable::Bool 
    header::StructDefHeader 
    lnn::MaybeProvided{LineNumberNode}
    fields::Vector{Tuple{StructDefFieldType, MaybeProvided{LineNumberNode}}}
    additional_exprs::StructDataType
    function GeneralizedStructDef{A, B, C}(is_mutable::Bool, header::StructDefHeader, lnn::MaybeProvided{LineNumberNode}, fields::Vector{Tuple{A, MaybeProvided{LineNumberNode}}}, additional_exprs::C) where {A, B, C}
        return new{A, B, C}(is_mutable, header, lnn, fields, additional_exprs)
    end
    function GeneralizedStructDef{A, B}(; is_mutable::Bool, header::StructDefHeader, lnn::MaybeProvided{LineNumberNode} = not_provided, fields::Vector{Tuple{A, MaybeProvided{LineNumberNode}}} = Tuple{A, MaybeProvded{LineNumberNode}}[]) where {A, B}
        additional_exprs = construct_struct_data(B)
        return new{A, B, typeof(additional_exprs)}(is_mutable, header, lnn, fields, additional_exprs)
    end
end

function Base.show(io::IO, def::GeneralizedStructDef)
    print(io, "GeneralizedStructDef\n\n", to_expr(def))
end

Base.propertynames(f::GeneralizedStructDef) = (:is_mutable, :header, :lnn, :fields, :additional_exprs, :typename, :parameters, :supertype, propertynames(getfield(f, :additional_exprs))...)

function Base.getproperty(s::GeneralizedStructDef, name::Symbol)
    if name in (:typename, :parameters, :supertype)
        return Base.getfield(Base.getfield(s, :header), name)
    elseif name === :fields 
        return FieldView(Base.getfield(s, :fields))
    elseif name in propertynames(getfield(s, :additional_exprs))
        return Base.getproperty(Base.getfield(s, :additional_exprs), name)
    else
        return Base.getfield(s, name)
    end
end

"""
    GeneralizedStructDef(f::TypedVar; [is_mutable, header, fields, constructors])

Returns a new copy of `f`, with optional `is_mutable`, `header`, `fields`, or `constructors` overridden by the keyword arguments.

"""
function GeneralizedStructDef(f::GeneralizedStructDef{A,B,C}; is_mutable::Bool=f.is_mutable, header::StructDefHeader=StructDefHeader(f.header), lnn::MaybeProvided{LineNumberNode}=f.lnn, fields::Vector{Tuple{A, MaybeProvided{LineNumberNode}}} = [(copy_value(_field), lnn) for (_field, lnn) in getfield(f, :fields)], additional_exprs::C=copy_struct_data(B, f.additional_exprs)) where {A, B, C}
    return GeneralizedStructDef{A,B,C}(is_mutable, header, lnn, fields, additional_exprs)
end

_from_expr(::Type{GeneralizedStructDef{A, B}}, expr) where {A, B} = _from_expr(GeneralizedStructDef{A, B, struct_data_type(B)}, expr)

function _from_expr(::Type{GeneralizedStructDef{A, B, C}}, expr) where {A, B, C}
    local global_lnn
    @return_if_exception lnn, structdef_expr = _structdef_expr(expr)
    if is_provided(lnn)
        global_lnn = lnn
    end
    @return_if_exception is_mutable, header_expr, body_expr = _parse_struct_expr(structdef_expr)
    @return_if_exception header = _from_expr(StructDefHeader, header_expr)
    @return_if_exception body = _from_expr(BlockExpr, body_expr)
    @return_if_exception first_body_lnn, fields, additional_exprs = _parse_struct_body(body, A, B)
    if !(Base.@isdefined global_lnn)
        global_lnn = first_body_lnn
    end
    return GeneralizedStructDef{A,B,C}(is_mutable, header, global_lnn, fields, additional_exprs)
end

function to_expr(f::GeneralizedStructDef)
    body = Expr(:block)
    if !isempty(f.fields)
        for _field_def in getfield(f, :fields)
            _field, lnn = _field_def
            if is_provided(lnn)
                push!(body.args, lnn)
            end
            push!(body.args, to_expr(_field))
        end
        for component in f.additional_exprs
            for _def in component
                _output, lnn = _def 
                if is_provided(lnn)
                    push!(body.args, lnn)
                end
                push!(body.args, to_expr(_output))
            end
        end
    elseif is_provided(f.lnn)
        push!(body.args, f.lnn)
    end
    result = Expr(:struct, f.is_mutable, to_expr(f.header), body)
    if is_provided(f.lnn)
        result = Expr(:block, f.lnn, result)
    end
    return result
end

@inline StructDef(f::GeneralizedStructDef) = StructDef(; is_mutable=f.is_mutable, header=f.header, lnn=f.lnn, fields=[(TypedVar(struct_field_name(first(t)), struct_field_type(first(t))), last(t)) for t in getfield(f, :fields)], constructors= hasproperty(f, :constructors) ? f.constructors : Tuple{FuncDef, LineNumberNode}[])


struct_field_name(f::ExprWOptions{TypedVar}) = struct_field_name(f.lhs)
struct_field_type(f::ExprWOptions{TypedVar}) = struct_field_type(f.lhs)