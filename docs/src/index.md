```@meta
CurrentModule = MacroUtilities
```

# API 

## General Utilities 
```@docs 
@assert_type 
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
```

### Composite Expression Types 
```@docs 
BlockExpr
AssignExpr
ExprWOptionalRhs
KVExpr
ExprWOptions
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
StructDefField(d::StructDefField)
StructDef
StructDef(d::StructDef)
GeneralizedStructDef
map_fields
```