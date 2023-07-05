module MacroUtilities

    using MLStyle, OrderedCollections

    # Expression parsing
    export from_expr, to_expr

    # Expression parsing types 
    export KVExpr, BlockExpr, AssignExpr

    export FuncArg, FuncCall, FuncDef

    export map_args, map_kwargs

    # Macro parsing types
    export MacroCall

    export doc_macro, assume_effects, assume_foldable

    # Keyword argument parsing 
    export parse_kvs!

    export @parse_kwargs

    include("parsing/_parsing.jl")

    include("parse_kwargs/_parse_kwargs.jl")
end
