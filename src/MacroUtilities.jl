module MacroUtilities

    using MLStyle, OrderedCollections

    # General utilities 
    export @assert_type

    # Expression parsing
    export from_expr, to_expr

    export is_not_provided, is_provided

    # Expression parsing types 
    export BlockExpr, AssignExpr

    export KVExpr, KeyWOptions

    export NestedDotExpr, NamedTupleArg, NamedTupleExpr

    export ExprWOptionalRhs, ExprWOptions

    export FuncArg, FuncCall, FuncDef, name_only

    export map_args, map_kwargs, names_only

    export StructDefHeader, StructDefField, StructDef 

    export map_fields

    # Macro parsing types
    export MacroCall

    export doc_macro, assume_effects, assume_foldable

    # Keyword argument parsing 
    export parse_kvs!

    export @parse_kwargs

    include("utils.jl")

    include("parsing/_parsing.jl")

    include("parse_kwargs/_parse_kwargs.jl")
end
