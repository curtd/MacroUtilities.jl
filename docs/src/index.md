```@meta
CurrentModule = MacroUtilities
```

# API 

## General Utilities 
```@docs 
@assert_type 
@unpack_option
```

## Keyword Arg Parsing  
```@docs 
@parse_kwargs 
```

## Syntax Parsing 
### General 
```@docs 
from_expr
to_expr
NotProvided
is_not_provided
is_provided
@return_if_exception
```

### Basic Expression Types
```@docs 
NamedTupleArg
NamedTupleArg(f::NamedTupleArg)
NamedTupleExpr
NamedTupleExpr(names::Vector{Symbol})
UnionExpr
TypedVar
TypedVar(d::TypedVar)
TypedExpr
TypedExpr(f::TypedExpr{E}) where {E}
```

### Composite Expression Types 
```@docs 
BlockExpr
PairExpr
PairExpr(p::PairExpr)
AssignExpr
AssignExpr(f::AssignExpr)
ExprWOptionalRhs
KVExpr
ExprWOptions
ExprWOptions(f::ExprWOptions)
KeyWOptions
DestructuredAssigmentExpr{E}
DestructuredAssigmentExpr{E}(f::DestructuredAssigmentExpr)
``` 

### Syntactic Sugar 
```@docs
NestedDotExpr
```

### Macro Expressions
```@docs 
MacroCall
doc_macro
__doc__macro
assume_effects
assume_foldable
```

### Function Expressions
```@docs 
FuncDef
FuncDef(f::FuncDef)
FuncCall
FuncCall(f::FuncCall)
FuncArg
FuncArg(f::FuncArg)
map_args
map_kwargs
name_only
names_only
```

### Struct Expressions 
```@docs 
StructDefHeader
StructDefHeader(h::StructDefHeader)
StructDefField
StructDef
StructDef(d::StructDef)
GeneralizedStructDef
GeneralizedStructDef(f::GeneralizedStructDef)
map_fields(f, def::StructDef)
map_fields(f, def::GeneralizedStructDef{A,B,C}) where {A,B,C}
```