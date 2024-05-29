var documenterSearchIndex = {"docs":
[{"location":"","page":"API","title":"API","text":"CurrentModule = MacroUtilities","category":"page"},{"location":"#API","page":"API","title":"API","text":"","category":"section"},{"location":"#General-Utilities","page":"API","title":"General Utilities","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"@assert_type \n@unpack_option\n@method_def_constant","category":"page"},{"location":"#MacroUtilities.@assert_type","page":"API","title":"MacroUtilities.@assert_type","text":"@assert_type var types\n\nEnsures that typeof(var) is in the list of provided types.\n\ntypes can be either a single type or a tuple or vect expression of types\n\n\n\n\n\n","category":"macro"},{"location":"#MacroUtilities.@unpack_option","page":"API","title":"MacroUtilities.@unpack_option","text":"@unpack_option [should_throw=false] options unpack_expr\n\nUnpacks a value from options depending on the form of unpack_expr. \n\nIf unpack_expr is of the form name, with name::Symbol, then this macro is  equivalent to writing \n\n$name = options[$name]\n\nIf should_throw == true, throws an ArgumentError if options does not contain key $name.\n\nOtherwise, returns an ArgumentError from the current function if options does not contain key $name.\n\nIf unpack_expr is of the form \n\nname::T, then options[$name] must be of type T or an ArgumentError will be returned\nname::Union{T1, T2, ..., Tn}, then options[$name] must be of type T1, T2, ..., or Tn\nname = default, then default_value will be used if $name is not present in options\nname => new_name, is the equivalent to $new_name = options[$name]\n\nAn unpack_expr of the form (name => new_name)::T, name::T = default or name::Union{T1, T2, ..., Tn} = default, etc., also behaves as expected.\n\n\n\n\n\n","category":"macro"},{"location":"#MacroUtilities.@method_def_constant","page":"API","title":"MacroUtilities.@method_def_constant","text":"@method_def_constant [ValType=Val] [map_expr] method_call get_constant_method\n\nGiven a method_call expression of the form f(::Type{T1}, ::Type{T2}, ..., ::Type{Ti}, ::ValType{::S}, ::Type{Ti+1}, ..., ::Type{Tn}), generates a method definition for get_constant_method which returns an iterable with element type S of all of the compile-time constants contained in the ValType parameter of each definition of f. \n\nget_constant_method is a generated function and thus incurs no runtime overhead, but also contains backedges to each method instance of f, and so the output is recompiled when a new method definition of f is added. \n\nValType must be a singleton type with a single parameter. Define MacroUtilities.inner_param to extract the innermost type parameter for your own custom types. \n\nIf map_expr is provided, it must resolve to a function mapping the collection of constants of type Vector{S} to a Expr. Defaults to MacroUtilities.default_extract_const_expr. (Requires at least Julia 1.7)\n\n\n\n\n\n","category":"macro"},{"location":"#Keyword-Arg-Parsing","page":"API","title":"Keyword Arg Parsing","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"@parse_kwargs ","category":"page"},{"location":"#MacroUtilities.@parse_kwargs","page":"API","title":"MacroUtilities.@parse_kwargs","text":"@parse_kwargs [args] kwarg_spec\n\nParses the set of keyword expressions given in args according to a series of kwarg_spec specification and sets the corresponding keys in the calling scope. Returns a Vector of the expressions that were not parsed as keyword arguments.\n\nkwarg_spec must be a block Expr, with each line consisting of an expression of the form \n\n    key = (expected_type = T)\n\nwhich specifies that for a key-value expression in args of the form key = value, value must have type T.\n\nAn alternative form is \n\n    key = (expected_types = (T1, T2, ..., Tn))\n\nwhich specifies that a key-value expression in args with key = value must have typeof(value) in (T1, T2, ..., Tn)\n\nIf the default key is provided, e.g., \n\n    key = (expected_types = (T1, T2, ..., Tn), default=default_value)\n\nthen key is set to default_value if args do not contain a key = value expression. \n\nIf the default key is not provided, then args must contain a key = value expression or an ArgumentError will be thrown. \n\nAn alternative, more compact, form to the above expressions is\n\n    key::Union{T1, T2, ..., Tn} = default_value\n\n\n\n\n\n","category":"macro"},{"location":"#Syntax-Parsing","page":"API","title":"Syntax Parsing","text":"","category":"section"},{"location":"#General","page":"API","title":"General","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"from_expr\nto_expr\nNotProvided\nis_not_provided\nis_provided\n@return_if_exception","category":"page"},{"location":"#MacroUtilities.from_expr","page":"API","title":"MacroUtilities.from_expr","text":"from_expr(::Type{T}, expr; throw_error::Bool=false, kwargs...)\n\nParses an expr into an object of type T\n\nThe provided kwargs are passed to the underlying parsing function. \n\nIf expr cannot be parsed into an object of type T     - if throw_error == true, throws an ArgumentError     - otherwise, returns nothing  \n\n====================\n\nfrom_expr(::Type{Vector{T}}, expr::Expr; throw_error::Bool=false)\n\nParses a tuple or vect expr as a Vector{T}. Each argument of expr must be of type T\n\n====================\n\nfrom_expr(::Type{Vector{T}}, input::T; throw_error::Bool=false)\n\nReturns a singleton Vector{T} containing input\n\n====================\n\nfrom_expr(::Type{FuncCall}, expr; normalize_kwargs::Bool=false)\n\nReturns a parsed FuncCall from expr\n\nIf normalize_kwargs = true, trailing equality expressions (e.g., f(a, b, c=1) will be parsed as keyword arguments. \n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.to_expr","page":"API","title":"MacroUtilities.to_expr","text":"to_expr(x) -> Expr\n\nConverts x to an Expr representation\n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.NotProvided","page":"API","title":"MacroUtilities.NotProvided","text":"NotProvided\n\nPlaceholder type to represent the absence of a field.\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.is_not_provided","page":"API","title":"MacroUtilities.is_not_provided","text":"is_not_provided(x) -> Bool\n\nReturns true if the field corresponding to x is not provided, false otherwise. \n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.is_provided","page":"API","title":"MacroUtilities.is_provided","text":"is_provided(d) -> Bool\n\nReturns true if the field corresponding to x is provided, false otherwise. \n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.@return_if_exception","page":"API","title":"MacroUtilities.@return_if_exception","text":"@return_if_exception(ex)\n\nEvalutes the destructured assignment expression ex of the form (; names...) = rhs or (names...) = rhs and returns from the current function if rhs returns an Exception\n\n\n\n\n\n","category":"macro"},{"location":"#Basic-Expression-Types","page":"API","title":"Basic Expression Types","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"NamedTupleArg\nNamedTupleArg(f::NamedTupleArg)\nNamedTupleExpr\nNamedTupleExpr(names::Vector{Symbol})\nTypeVarExpr\nCurlyExpr\nUnionExpr\nTypedVar\nTypedVar(d::TypedVar)\nTypedExpr\nTypedExpr(f::TypedExpr{E}) where {E}","category":"page"},{"location":"#MacroUtilities.NamedTupleArg","page":"API","title":"MacroUtilities.NamedTupleArg","text":"NamedTupleArg(; key::Symbol, value=not_provided, is_splat::Bool=false, kw_head::Bool)\n\nNamedTupleArg(key::Symbol; kw_head::Bool)\n\nMatches a key = value or a key argument inside a NamedTuple expression\n\nIf kw_head == true, the expression head is set to :kw, otherwise :(=)\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.NamedTupleArg-Tuple{NamedTupleArg}","page":"API","title":"MacroUtilities.NamedTupleArg","text":"NamedTupleArg(f::NamedTupleArg; [key::Symbol], [value], [is_splat::Bool], [kw_head])\n\nReturns a new copy of f, with optional name, type, value, or is_splat overridden by the keyword arguments. \n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.NamedTupleExpr","page":"API","title":"MacroUtilities.NamedTupleExpr","text":"NamedTupleExpr(args::Vector{NamedTupleArg})\nNamedTupleExpr(; args::Vector{NamedTupleArg})\n\nMatches a (named) tuple expression\n\nThe keys and values of this expression can be accessed, modified, and queried with Base.getindex, Base.setindex!, Base.iterate, Base.findfirst, Base.keys, Base.haskey, Base.length, Base.pop!, and Base.push!\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.NamedTupleExpr-Tuple{Vector{Symbol}}","page":"API","title":"MacroUtilities.NamedTupleExpr","text":"NamedTupleExpr(names::Vector{Symbol}; kw_head::Bool=false)\n\nReturns a NamedTupleExpr built from names. \n\nIf kw_head == true, this expression is of the form (; names...), and otherwise it is of the form (names...)\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.TypeVarExpr","page":"API","title":"MacroUtilities.TypeVarExpr","text":"TypeVarExpr <: AbstractExpr\n\nMatches expressions of the form\n\ntypename\ntypename <: UB \n<: UB\ntypename >: LB\n>: LB\nLB <: typename <: UB\n\nConstructors\n\n    TypeVarExpr{LB, UB}(; typename::Symbol, lb::Union{LB, NotProvided}=not_provided, ub::Union{UB, NotProvided}=not_provided)\n\n    TypeVarExpr(typename::Symbol, [lb], [ub])\n\n    TypeVarExpr{LB, UB}(t::TypeVarExpr)\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.CurlyExpr","page":"API","title":"MacroUtilities.CurlyExpr","text":"CurlyExpr{FirstArg, E} <: AbstractExpr\n\nMatches an expression of the form FirstArg{args...}, if FirstArg is a Symbol, or args[1]{args[2:end]...} otherwise.\n\nConstructors\n\n    CurlyExpr{FirstArg, E}(; args::Vector{E}) \n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.UnionExpr","page":"API","title":"MacroUtilities.UnionExpr","text":"UnionExpr(; args::Vector{Union{Symbol, Expr}})\n\nMatches an expression of the form Union{T1, T2, ...}\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.TypedVar","page":"API","title":"MacroUtilities.TypedVar","text":"TypedVar(; name, type)\n\nMatches an expression of the form name or name::type\n\nFields\n\nname::Symbol\ntype::Union{Symbol, Expr, NotProvided} = not_provided\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.TypedVar-Tuple{TypedVar}","page":"API","title":"MacroUtilities.TypedVar","text":"TypedVar(f::TypedVar; [name, type])\n\nReturns a new copy of f, with optional name or type overridden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.TypedExpr","page":"API","title":"MacroUtilities.TypedExpr","text":"TypedExpr{E}(; expr::E, type::Union{Symbol, Expr, NotProvided})\n\nMatches an expression of the form expr or expr::type\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.TypedExpr-Union{Tuple{TypedExpr{E}}, Tuple{E}} where E","page":"API","title":"MacroUtilities.TypedExpr","text":"TypedExpr(f::TypedExpr; [expr, type])\n\nReturns a new copy of f, with optional expr or type overridden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#Composite-Expression-Types","page":"API","title":"Composite Expression Types","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"PairExpr\nPairExpr(p::PairExpr)\nAssignExpr\nAssignExpr(f::AssignExpr)\nExprWOptionalRhs\nKVExpr\nExprWOptions\nExprWOptions(f::ExprWOptions)\nKeyWOptions\nDestructuredAssigmentExpr{E}\nDestructuredAssigmentExpr{E}(f::DestructuredAssigmentExpr)","category":"page"},{"location":"#MacroUtilities.PairExpr","page":"API","title":"MacroUtilities.PairExpr","text":"PairExpr{L,R}(; lhs::L, rhs::R)\n\nMatches an expression of the form lhs => rhs \n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.PairExpr-Tuple{PairExpr}","page":"API","title":"MacroUtilities.PairExpr","text":"PairExpr(f::PairExpr{L,R}; [lhs::L], [rhs::R])\n\nReturns a new copy of f, with optional lhs and rhs overriden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.AssignExpr","page":"API","title":"MacroUtilities.AssignExpr","text":"AssignExpr{L,R}(; lhs::L, rhs::R, allow_kw)\n\nMatches a lhs = rhs assignment expression. \n\nIf allow_key == true, also matches an assignment expression with head :kw\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.AssignExpr-Tuple{AssignExpr}","page":"API","title":"MacroUtilities.AssignExpr","text":"AssignExpr(f::AssignExpr{L,R}; [lhs::L], [rhs::R], [allow_kw::Bool])\n\nReturns a new copy of f, with optional lhs, rhs, and allow_kw overriden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.ExprWOptionalRhs","page":"API","title":"MacroUtilities.ExprWOptionalRhs","text":"ExprWOptionalRhs{E <: AbstractExpr}(; lhs::E, default=not_provided)\n\nMatches an expression of the form lhs = default, where lhs is matched by E, or an expression matched by E\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.KVExpr","page":"API","title":"MacroUtilities.KVExpr","text":"KVExpr(; lhs::Symbol, rhs)\n\nMatches an expression of the form lhs = rhs or lhs\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.ExprWOptions","page":"API","title":"MacroUtilities.ExprWOptions","text":"ExprWOptions{E}(; lhs::E, options::NamedTupleExpr)\nExprWOptions{E}(; inner_expr::AssignExpr{E, NamedTupleExpr})\nExprWOptions(lhs::E, options::NamedTupleExpr; allow_kw::Bool=false)\n\nMatches an expression of the form lhs = (key1=value1, ...) or lhs, where lhs matches E\n\nThe underlying options in the expression can be accessed/iterated over/modified via the Base.haskey, Base.keys, BAse.iterate, Base.getindex, and Base.setindex! functions.\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.ExprWOptions-Tuple{ExprWOptions}","page":"API","title":"MacroUtilities.ExprWOptions","text":"ExprWOptions(f::ExprWOptions{E}; [lhs::E], [rhs::NamedTupleExpr], [allow_kw::Bool])\n\nReturns a new copy of f, with optional lhs, rhs, and allow_kw overridden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.KeyWOptions","page":"API","title":"MacroUtilities.KeyWOptions","text":"KeyWOptions(; lhs::Symbol, options::NamedTupleExpr)\n\nMatches an expression of the form lhs = (key1=value1, ...) or lhs\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.DestructuredAssigmentExpr","page":"API","title":"MacroUtilities.DestructuredAssigmentExpr","text":"DestructuredAssigmentExpr{E}(; lhs_names::Vector{Symbol}, destructure_type::Symbol, rhs::E)\n\nMatches an expression of the form lhs = rhs, where \n\nif destructure_type == :tuple, lhs is of the form Expr(:tuple, lhs_names[1:end-1]..., Expr(:(=), lhs_names[end], rhs)\nif destructure_type == :eq_tuple, lhs is of the form (lhs_names...) = rhs\nif destructure_type == :eq_namedtuple, lhs is of the form (; lhs_names...) = rhs\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.DestructuredAssigmentExpr-Union{Tuple{DestructuredAssigmentExpr}, Tuple{E}} where E","page":"API","title":"MacroUtilities.DestructuredAssigmentExpr","text":"DestructuredAssigmentExpr(f::DestructuredAssigmentExpr{E}; lhs_names::Vector{Symbol}, destructure_type::Symbol, rhs::E)\n\nReturns a new copy of f, with optional lhs_names, destructure_type, and rhs overriden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#Block-Expression-Types","page":"API","title":"Block Expression Types","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"BlockExpr\nIfElseExpr","category":"page"},{"location":"#MacroUtilities.BlockExpr","page":"API","title":"MacroUtilities.BlockExpr","text":"BlockExpr(; args::Vector{Any})\n\nMatches a :block expression\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.IfElseExpr","page":"API","title":"MacroUtilities.IfElseExpr","text":"IfElseExpr(; [if_else_exprs], [else_expr])\n\nMatches an if/if-else/if-elseif-else block\n\nArguments\n\nif_else_exprs::Vector{Pair{Any,Any}} = Pair{Any, Any}[]: Set of condition => condition_expr blocks. The first pair corresponds to the if statement, the rest are elseif statements \nelse_expr::Any = not_provided: The else statement of the block\n\n\n\n\n\n","category":"type"},{"location":"#Syntactic-Sugar","page":"API","title":"Syntactic Sugar","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"NestedDotExpr","category":"page"},{"location":"#MacroUtilities.NestedDotExpr","page":"API","title":"MacroUtilities.NestedDotExpr","text":"NestedDotExpr(; keys::Vector{Symbol})\n\nMatches expressions of the form A.B.C...., with each symbol being stored in keys\n\n\n\n\n\n","category":"type"},{"location":"#Macro-Expressions","page":"API","title":"Macro Expressions","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"MacroCall\nDocExpr\ndoc_macro\n__doc__macro\nassume_effects\nassume_foldable","category":"page"},{"location":"#MacroUtilities.MacroCall","page":"API","title":"MacroUtilities.MacroCall","text":"MacroCall(; name::Union{Symbol, Expr, GlobalRef}, line::Union{LineNumberNode, NotProvided} = not_provided, args::Vector{Any} = [])\n\nMatches a macro call expression. A MacroCall object can be applied to one or more expressions, yielding another MacroCall. \n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.DocExpr","page":"API","title":"MacroUtilities.DocExpr","text":"DocExpr{E}(; line::LineNumberNode, docstr::Union{String, Expr}, expr::E)\n\nMatches a standard documented expression expr, e.g.,\n\n\"documentation\"\nexpr\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.doc_macro","page":"API","title":"MacroUtilities.doc_macro","text":"doc_macro\n\nA MacroCall corresponding to Core.@doc\n\n\n\n\n\n","category":"constant"},{"location":"#MacroUtilities.__doc__macro","page":"API","title":"MacroUtilities.__doc__macro","text":"__doc__macro\n\nA MacroCall corresponding to Core.@__doc__\n\n\n\n\n\n","category":"constant"},{"location":"#MacroUtilities.assume_effects","page":"API","title":"MacroUtilities.assume_effects","text":"assume_effects\n\nA MacroCall corresponding to Base.@assume_effects\n\n\n\n\n\n","category":"constant"},{"location":"#MacroUtilities.assume_foldable","page":"API","title":"MacroUtilities.assume_foldable","text":"assume_foldable\n\nA MacroCall corresponding to Base.@assume_effects :foldable\n\n\n\n\n\n","category":"constant"},{"location":"#Function-Expressions","page":"API","title":"Function Expressions","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"FuncDef\nFuncDef(f::FuncDef)\nFuncCall\nFuncCall(f::FuncCall)\nFuncArg\nFuncArg(f::FuncArg)\nmap_args\nmap_kwargs\nname_only\nnames_only","category":"page"},{"location":"#MacroUtilities.FuncDef","page":"API","title":"MacroUtilities.FuncDef","text":"FuncDef(; header, head, whereparams, return_type, body, line, doc)\n\nMatches a function definition expression\n\nFields\n\nheader::FuncCall\nhead::Symbol\nwhereparams::Any = not_provided\nreturn_type::Any = not_provided\nbody::Any\nline::Union{LineNumberNode, NotProvided} = not_provided\ndoc::Union{Expr, String, NotProvided} = not_provided\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.FuncDef-Tuple{FuncDef}","page":"API","title":"MacroUtilities.FuncDef","text":"FuncDef(f::FuncDef; [header, head, whereparams, return_type, body, line, doc)\n\nReturns a new copy of f, with optional header, head, whereparams, return_type, body, line, or doc values overridden by the keyword arguments. \n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.FuncCall","page":"API","title":"MacroUtilities.FuncCall","text":"FuncCall(; funcname, args, kwargs)\n\nMatches a function call expression\n\nFields\n\nfuncname::Union{NotProvided, Symbol, Expr}\nargs::Vector{FuncArg} = Vector{FuncArg}()\nkwargs::OrderedDict{Symbol, FuncArg} = OrderedDict{Symbol, FuncArg}()\n\nIn the FuncArgs keyword constructor, kwargs may also be a Vector{FuncArg} or a Vector{Pair{Symbol, FuncArg}}\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.FuncCall-Tuple{FuncCall}","page":"API","title":"MacroUtilities.FuncCall","text":"FuncCall(f::FuncCall; [funcname, args, kwargs])\n\nReturns a new copy of f, with optional funcname, args, or kwargs overridden by the keyword arguments. \n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.FuncArg","page":"API","title":"MacroUtilities.FuncArg","text":"FuncArg(; name, type, value::Any, is_splat::Bool)\n\nMatches a function argument expression\n\nFields\n\nname::Union{NotProvided, Symbol} = not_provided: Name of the argument\ntype::Union{NotProvided, Symbol, Expr} = not_provided: Annotated type of the argument. Only valid when value is not provided. \nvalue::Any = not_provided: Value of the argument. If name is provided, this corresponds to the default value of name in a method definition. Otherwise, this is the value passed as a function argument \nis_splat::Bool = false: true if this argument is splatted, false otherwise\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.FuncArg-Tuple{FuncArg}","page":"API","title":"MacroUtilities.FuncArg","text":"FuncArg(f::FuncArg; [name, type, value, is_splat])\n\nReturns a new copy of f, with optional name, type, value, or is_splat overridden by the keyword arguments. \n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.map_args","page":"API","title":"MacroUtilities.map_args","text":"map_args(f, expr::FuncCall) -> FuncCall\nmap_args(f, expr::FuncDef) -> FuncCall\n\nTransform the expr by applying f(FuncArg) -> FuncArg to each of its arguments\n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.map_kwargs","page":"API","title":"MacroUtilities.map_kwargs","text":"map_kwargs(f, expr::FuncCall) -> FuncCall\nmap_kwargs(f, expr::FuncDef) -> FuncCall\n\nTransform the expr by applying f(FuncArg) -> FuncArg to each of its keyword arguments\n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.name_only","page":"API","title":"MacroUtilities.name_only","text":"name_only(f::FuncArg; is_splat::Bool=f.is_splat) -> FuncArg\n\nReturns a new FuncArg with the type removed from f\n\nIf f.name is provided, also removes the value field from f\n\n\n\n\n\nname_only(f::NamedTupleArg) -> NamedTupleArg\n\nReturns a NamedTupleArg derived from f with its value removed\n\n\n\n\n\n","category":"function"},{"location":"#MacroUtilities.names_only","page":"API","title":"MacroUtilities.names_only","text":"names_only(f::NamedTupleExpr) -> NamedTupleExpr\n\nReturns a NamedTupleExpr derived from f with each arguments' value removed\n\n\n\n\n\nnames_only(f::FuncCall) -> FuncCall\n\nReturns a FuncCall derived from f with all of its types + values removed from its arguments/keyword arguments \n\n\n\n\n\n","category":"function"},{"location":"#Struct-Expressions","page":"API","title":"Struct Expressions","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"StructDefHeader\nStructDefHeader(h::StructDefHeader)\nStructDefField\nStructDef\nStructDef(d::StructDef)\nGeneralizedStructDef\nGeneralizedStructDef(f::GeneralizedStructDef)\nmap_fields(f, def::StructDef)\nmap_fields(f, def::GeneralizedStructDef{A,B,C}) where {A,B,C}","category":"page"},{"location":"#MacroUtilities.StructDefHeader","page":"API","title":"MacroUtilities.StructDefHeader","text":"StructDefHeader(; typename, parameters, supertype)\n\nMatches the header of a struct definition\n\nFields\n\ntypename::Symbol\nparameters::Vector{TypeVarExpr} = TypeVarExpr[]\nsupertype::Union{Symbol, Expr, NotProvided} = not_provided\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.StructDefHeader-Tuple{StructDefHeader}","page":"API","title":"MacroUtilities.StructDefHeader","text":"StructDefHeader(f::StructDefHeader; [typename, parameter, supertype])\n\nReturns a new copy of f, with optional typename, parameter, or supertype overridden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.StructDefField","page":"API","title":"MacroUtilities.StructDefField","text":"TypedVar(; name, type)\n\nMatches an expression of the form name or name::type\n\nFields\n\nname::Symbol\ntype::Union{Symbol, Expr, NotProvided} = not_provided\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.StructDef","page":"API","title":"MacroUtilities.StructDef","text":"StructDef(; is_mutable, header, lnn, fields, constructors)\n\nMatches a struct definition. The properties .typename, .parameter, and .supertype forward to the header field.\n\nFields\n\nis_mutable::Bool\nheader::StructDefHeader\nlnn::Union{LineNumberNode, NotProvided} = not_provided\nfields::Vector{Tuple{TypedVar, LineNumberNode}}\nconstructors::Vector{Tuple{FuncDef, LineNumberNode}}\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.StructDef-Tuple{StructDef}","page":"API","title":"MacroUtilities.StructDef","text":"StructDef(f::StructDef; [is_mutable, header, fields, constructors])\n\nReturns a new copy of f, with optional is_mutable, header, fields, or constructors overridden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.GeneralizedStructDef","page":"API","title":"MacroUtilities.GeneralizedStructDef","text":"GeneralizedStructDef(; is_mutable, header, lnn, fields, constructors)\n\nMatches a struct definition. The properties .typename, .parameter, and .supertype forward to the header field.\n\nFields\n\nis_mutable::Bool\nheader::StructDefHeader\nlnn::Union{LineNumberNode, NotProvided} = not_provided\nfields::Vector{Tuple{TypedVar, LineNumberNode}}\nconstructors::Vector{Tuple{FuncDef, LineNumberNode}}\n\n\n\n\n\n","category":"type"},{"location":"#MacroUtilities.GeneralizedStructDef-Tuple{GeneralizedStructDef}","page":"API","title":"MacroUtilities.GeneralizedStructDef","text":"GeneralizedStructDef(f::TypedVar; [is_mutable, header, fields, constructors])\n\nReturns a new copy of f, with optional is_mutable, header, fields, or constructors overridden by the keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.map_fields-Tuple{Any, StructDef}","page":"API","title":"MacroUtilities.map_fields","text":"map_fields(f, def::StructDef) -> StructDef\n\nApply the function f(::TypedVar) -> TypedVar to each field definition in def\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.map_fields-Union{Tuple{C}, Tuple{B}, Tuple{A}, Tuple{Any, GeneralizedStructDef{A, B, C}}} where {A, B, C}","page":"API","title":"MacroUtilities.map_fields","text":"map_fields(f, def::GeneralizedStructDef{A,B,C}) -> GeneralizedStructDef\n\nApply the function f(::A) -> A to each field definition in def\n\n\n\n\n\n","category":"method"},{"location":"#Expression-Generation","page":"API","title":"Expression Generation","text":"","category":"section"},{"location":"","page":"API","title":"API","text":"kwarg_constructor(typename, fields::Vector{TypedVar}, default_vals)\nkwarg_constructor(f::StructDef, default_vals)\ncopy_constructor(typename, fields::Vector{TypedVar})\ncopy_constructor(f::StructDef)","category":"page"},{"location":"#MacroUtilities.kwarg_constructor-Tuple{Any, Vector{TypedVar}, Any}","page":"API","title":"MacroUtilities.kwarg_constructor","text":"kwarg_constructor(typename, fields::Vector{TypedVar}, default_vals; [lnn::Union{LineNumberNode, Nothing)=nothing], [whereparams=not_provided])\n\nReturns a FuncDef keyword argument constructor for typename with fields and default_vals is any collection that implements Base.get(default_vals, fieldname, default)\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.kwarg_constructor-Tuple{StructDef, Any}","page":"API","title":"MacroUtilities.kwarg_constructor","text":"kwarg_constructor(f::StructDef, default_vals; [lnn::Union{LineNumberNode, Nothing)=nothing], [whereparams=not_provided]))\n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.copy_constructor-Tuple{Any, Vector{TypedVar}}","page":"API","title":"MacroUtilities.copy_constructor","text":"copy_constructor(typename, fields::Vector{TypedVar}; [input_var::Symbol], [lnn::Union{LineNumberNode, Nothing}], [whereparams=not_provided], [copy_expr=default_copy_expr])\n\nReturns a FuncDef copy constructor for typename with fields, i.e., a function of the form \n\n    $typename($input_var::$typename; field1::fieldtype1=Base.copy(getfield($input_var, :field1)), ...) = $typename(field1, ...)\n\nSupports an optionally provided lnn and whereparams\n\ncopy_expr(name, input_value) must return an Expr that determines how the field name will be copied from input_value \n\n\n\n\n\n","category":"method"},{"location":"#MacroUtilities.copy_constructor-Tuple{StructDef}","page":"API","title":"MacroUtilities.copy_constructor","text":"copy_constructor(f::StructDef; [input_var::Symbol], [lnn::Union{LineNumberNode, Nothing}], [whereparams=not_provided])\n\n\n\n\n\n","category":"method"}]
}