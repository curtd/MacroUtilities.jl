"""
    StructDefHeader(; typename, parameter, supertype)

Matches the header of a struct definition

# Fields 
- `typename::Symbol`
- `parameter::Union{Symbol, Expr, NotProvided} = not_provided`
- `supertype::Union{Symbol, Expr, NotProvided} = not_provided`
"""
Base.@kwdef struct StructDefHeader <: AbstractExpr 
    typename::Symbol 
    parameter::Union{Symbol, Expr, NotProvided} = not_provided 
    supertype::Union{Symbol, Expr, NotProvided} = not_provided
end

function _from_expr(::Type{StructDefHeader}, expr)
    (rest, supertype) = @switch expr begin 
        @case Expr(:(<:), rest, supertype)
            (rest, supertype)
        @case _ 
            (expr, not_provided)
    end
    (typename, parameter) = @switch rest begin 
        @case Expr(:curly, typename, parameter)
            (typename, parameter)
        @case _
            (rest, not_provided)
    end
    if !(typename isa Symbol)
        return ArgumentError("Typename `$typename` derived from input expression `$expr` is not a Symbol")
    end
    return StructDefHeader(; typename, parameter, supertype)
end

function to_expr(h::StructDefHeader)
    expr = h.typename
    if is_provided(h.parameter)
        expr = Expr(:curly, h.typename, h.parameter)
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
function StructDefHeader(f::StructDefHeader; typename::Symbol=f.typename, parameter::Union{Symbol, Expr, NotProvided}=( f.parameter isa Expr ? deepcopy(f.parameter) : f.parameter), supertype::Union{Symbol, Expr, NotProvided}=( f.supertype isa Expr ? deepcopy(f.supertype) : f.supertype))
    return StructDefHeader(typename, parameter, supertype)
end

"""
    StructDefField(; name, type)

Matches a field definition in a struct definition

# Fields 
- `name::Symbol`
- `type::Union{Symbol, Expr, NotProvided}`
"""
Base.@kwdef struct StructDefField <: AbstractExpr 
    name::Symbol
    type::Union{Symbol, Expr, NotProvided} = not_provided
end

"""
    StructDefField(f::StructDefField; [name, type])

Returns a new copy of `f`, with optional `name` or `type` overridden by the keyword arguments.
"""
function StructDefField(f::StructDefField; name::Symbol=f.typename, type::Union{Symbol, Expr, NotProvided}=( f.type isa Expr ? deepcopy(f.type) : f.type))
    return StructDefField(name, type)
end

function _from_expr(::Type{StructDefField}, expr)
    @switch expr begin 
        @case Expr(:(::), name, type) && if name isa Symbol && (type isa Symbol || type isa Expr) end
            return StructDefField(; name, type)
        @case ::Symbol 
            return StructDefField(; name=expr)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid field definition")
    end
end
function to_expr(f::StructDefField)
    if is_not_provided(f.type)
        return f.name 
    else
        return Expr(:(::), f.name, f.type)
    end
end

"""
    StructDef(; is_mutable, header, lnn, fields, constructors)

Matches a struct definition. The properties `.typename`, `.parameter`, and `.supertype` forward to the `header` field.

# Fields 
- `is_mutable::Bool`
- `header::StructDefHeader`
- `lnn::Union{LineNumberNode, NotProvided}`
- `fields::Vector{Tuple{StructDefField, LineNumberNode}}`
- `constructors::Vector{Tuple{FuncDef, LineNumberNode}}`
"""
Base.@kwdef struct StructDef <: AbstractExpr 
    is_mutable::Bool 
    header::StructDefHeader 
    lnn::Union{LineNumberNode, NotProvided} = not_provided
    fields::Vector{Tuple{StructDefField, LineNumberNode}}
    constructors::Vector{Tuple{FuncDef, LineNumberNode}}
end

function Base.show(io::IO, def::StructDef)
    print(io, "StructDef\n\n", to_expr(def))
end

Base.propertynames(::StructDef) = (:is_mutable, :header, :lnn, :fields, :constructors, :typename, :parameter, :supertype)

function Base.getproperty(s::StructDef, name::Symbol)
    if name in (:typename, :parameter, :supertype)
        return Base.getfield(Base.getfield(s, :header), name)
    else
        return Base.getfield(s, name)
    end
end

"""
    StructDef(f::StructDefField; [is_mutable, header, fields, constructors])

Returns a new copy of `f`, with optional `is_mutable`, `header`, `fields`, or `constructors` overridden by the keyword arguments.

"""
function StructDef(f::StructDef; is_mutable::Bool=f.is_mutable, header::StructDefHeader=StructDefHeader(f.header), lnn::Union{LineNumberNode, NotProvided}=f.lnn, fields::Vector{Tuple{StructDefField, LineNumberNode}} = [(copy(_field), lnn) for (_field, lnn) in f.fields], constructors::Vector{Tuple{FuncDef, LineNumberNode}} = [(copy(_def), lnn) for (_def, lnn) in f.constructors])
    return StructDef(is_mutable, header, lnn, fields, constructors)
end

function _from_expr(::Type{StructDef}, expr)
    (is_mutable, header_expr, body_expr) = @switch expr begin 
        @case Expr(:struct, is_mutable, header_expr, body_expr) && if is_mutable isa Bool end 
            (is_mutable, header_expr, body_expr)
        @case _ 
            return ArgumentError("Input expression `$expr` is not a valid struct definition expression")
    end
    header = _from_expr(StructDefHeader, header_expr)
    header isa Exception && return header 
    body = _from_expr(BlockExpr, body_expr)
    body isa Exception && return body 
    num_args = length(body.args)
    fields = Vector{Union{Tuple{StructDefField, LineNumberNode}, Tuple{StructDefField, NotProvided}}}()
    constructors = Vector{Union{Tuple{FuncDef, LineNumberNode}, Tuple{FuncDef, NotProvided}}}()
    local lnn_global
    i = 1 
    finished_parsing_fields = false
    while i ≤ num_args
        arg = body.args[i]
        if arg isa LineNumberNode 
            lnn = arg 
            if !(Base.@isdefined lnn_global)
                lnn_global = arg
            end
            if i < num_args 
                i += 1
                arg = body.args[i]
            else
                break 
            end
        else
            return ArgumentError("In struct body, expected a LineNumberNode but got $arg with type $(typeof(arg)) instead")
        end
        if !finished_parsing_fields
            _field = _from_expr(StructDefField, arg)
            if !(_field isa Exception)
                push!(fields, (_field, lnn))
            else
                _constructor = _from_expr(FuncDef, arg)
                if !(_constructor isa Exception)
                    finished_parsing_fields = true 
                    push!(constructors, (_constructor, lnn))
                else 
                    return ArgumentError("Could not parse expression `$(arg)` as a field definition nor a constructor")
                end
            end
        else 
            _constructor = _from_expr(FuncDef, arg)
            if !(_constructor isa Exception)
                push!(constructors, (_constructor, lnn))
            else 
                return ArgumentError("Could not parse expression `$(arg)` as a constructor definition")
            end
        end
        i += 1
    end
    return StructDef(; is_mutable, lnn=lnn_global, header, fields, constructors)
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
    return Expr(:struct, f.is_mutable, to_expr(f.header), body)
end

"""
    map_fields(f, def::StructDef) -> StructDef

Apply the function `f(::StructDefField) -> StructDefField` to each field definition in `def`
"""
function map_fields(f, def::StructDef)
    _fields = map(f, first.(def.fields))::Vector{StructDefField}
    new_field_defs = Vector{Tuple{StructDefField, LineNumberNode}}()
    for ((_, lnn), new_field) in zip(def.fields, _fields)
        push!(new_field_defs, (new_field, lnn))
    end
    return StructDef(def; fields=new_field_defs)
end

const StructDefFieldOptionalDefault = ExprWOptionalRhs{StructDefField}