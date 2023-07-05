var documenterSearchIndex = {"docs":
[{"location":"","page":"API","title":"API","text":"CurrentModule = MacroUtilities","category":"page"},{"location":"#API","page":"API","title":"API","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"Modules = [MacroUtilities]\nOrder   = [:macro, :function, :type] \nPrivate = false","category":"page"},{"location":"#MacroUtilities.@parse_kwargs-Tuple","page":"API","title":"MacroUtilities.@parse_kwargs","text":"@parse_kwargs [args] kwarg_spec\n\nParses the set of keyword expressions given in args according to a series of kwarg_spec specification and sets the corresponding keys in the calling scope. \n\nkwarg_spec must be a block Expr, with each line consisting of an expression of the form \n\n    key = (expected_type = T)\n\nwhich specifies that for a key-value expression in args of the form key = value, value must have type T.\n\nAn alternative form is \n\n    key = (expected_types = (T1, T2, ..., Tn))\n\nwhich specifies that a key-value expression in args with key = value must have typeof(value) in (T1, T2, ..., Tn)\n\nIf the default key is provided, e.g., \n\n    key = (expected_types = (T1, T2, ..., Tn), default=default_value)\n\nthen key is set to default_value if args do not contain a key = value expression. \n\nIf the default key is not provided, then args must contain a key = value expression or an ArgumentError will be thrown. \n\n\n\n\n\n","category":"macro"},{"location":"#MacroUtilities.from_expr-Union{Tuple{T}, Tuple{Type{T}, Any}} where T","page":"API","title":"MacroUtilities.from_expr","text":"from_expr(::Type{T}, expr; throw_error::Bool=false, kwargs...)\n\nParses an expr into an object of type T\n\nThe provided kwargs are passed to the underlying parsing function. \n\nIf expr cannot be parsed into an object of type T     - if throw_error == true, throws an ArgumentError     - otherwise, returns nothing  \n\n====================\n\nfrom_expr(::Type{Vector{T}}, expr::Expr; throw_error::Bool=false)\n\nParses a tuple or vect expr as a Vector{T}. Each argument of expr must be of type T.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.to_expr","page":"API","title":"MacroUtilities.to_expr","text":"to_expr(x) -> Expr\n\nConverts x to an Expr representation\n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.FuncArg","page":"API","title":"MacroUtilities.FuncArg","text":"FuncArg(; name, type, value::Any, is_splat::Bool)\n\nMatches a function argument expression\n\nArguments\n\nname::Union{NotProvided, Symbol} = not_provided: Name of the argument\ntype::Union{NotProvided, Symbol, Expr} = not_provided: Annotated type of the argument. Only valid when value is not provided. \nvalue::Any = not_provided: Value of the argument. If name is provided, this corresponds to the default value of name in a method definition. Otherwise, this is the value passed as a function argument \nis_splat::Bool = false: true if this argument is splatted, false otherwise\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.FuncCall","page":"API","title":"MacroUtilities.FuncCall","text":"FuncCall(; funcname::Union{NotProvided, Symbol, Expr}, args::Vector{FuncArg}, kwargs::OrderedDict{Symbol, FuncArg})\n\nMatches a function call expression\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.FuncDef","page":"API","title":"MacroUtilities.FuncDef","text":"FuncDef(; header::FuncCall, head::Symbol, whereparams::Any, return_type::Any, body::Any, line::Union{LineNumberNode, NotProvided}, doc::Union{LineNumberNode, NotProvided})\n\nMatches a function definition expression\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.KVExpr","page":"API","title":"MacroUtilities.KVExpr","text":"KVExpr(; key, value)\n\nMatches expressions of the form key::Symbol = value\n\n\n\n\n\n","category":"type"},{"location":"","page":"API","title":"API","text":"is_not_provided\nNotProvided","category":"page"},{"location":"#MacroUtilities.is_not_provided","page":"API","title":"MacroUtilities.is_not_provided","text":"is_not_provided(x) -> Bool\n\nReturns true if the field corresponding to x is not provided, false otherwise. \n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.NotProvided","page":"API","title":"MacroUtilities.NotProvided","text":"NotProvided\n\nPlaceholder type to represent the absence of a field.\n\n\n\n\n\n","category":"type"}]
}
