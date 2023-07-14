module MacroUtilities

    using MLStyle, OrderedCollections

    # General utilities 
    export @assert_type

    # Expression parsing
    export from_expr, to_expr

    export is_not_provided, is_provided, not_provided, NotProvided, MaybeProvided

    # Expression parsing types 
    export UnionExpr, TypedVar, AssignExpr, NamedTupleArg, NamedTupleExpr
    
    export BlockExpr, ExprWOptionalRhs, KVExpr, ExprWOptions, KeyWOptions, DestructuredAssigmentExpr

    export @arg_error, @return_if_exception, @unpack_option
    
    export NestedDotExpr

    export FuncArg, FuncCall, FuncDef, name_only

    export map_args, map_kwargs, names_only

    export StructDefHeader, StructDefField, StructDef, GeneralizedStructDef

    export map_fields

    # Macro parsing types
    export MacroCall

    export doc_macro, __doc__macro, assume_effects, assume_foldable

    # Keyword argument parsing 
    export parse_kvs!

    export @parse_kwargs

    include("utils.jl")

    include("parsing/_parsing.jl")

    include("transformations/_transformations.jl")

    include("parse_kwargs/_parse_kwargs.jl")
end
