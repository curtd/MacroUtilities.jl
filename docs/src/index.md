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
```

### Expression Types
```@docs 
AssignExpr
BlockExpr
KVExpr
``` 

### Macro Expressions
```@docs 
MacroCall
doc_macro
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
```