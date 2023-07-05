# MacroUtilities

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://curtd.github.io/MacroUtilities.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://curtd.github.io/MacroUtilities.jl/dev/)
[![Build Status](https://github.com/curtd/MacroUtilities.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/curtd/MacroUtilities.jl/actions/workflows/CI.yml?query=branch%3Amain)

`MacroUtilities.jl` provides useful utilities for manipulating expressions and writing macros in Julia.

## Keyword Arguments From Expressions
Although macros operate at an expression level and don't have access to the types of their arguments, it is sometimes useful to access named values with expected literal types such as `String`, `Bool`, `Int`, or `Nothing`. 

This package provides the `@parse_kwargs` macro, which can be used in your own macro definitions to specify type / default value information for keyword arguments parsed from expressions. An `ArgumentError` is thrown if either the required keyword arguments (i.e., those without a default value) are missing or the provided argument does not have the expected type.

E.g., 
```julia
julia> using MacroUtilities 

julia> macro ex_macro(args...)
       @parse_kwargs args... begin 
           key1 = (expected_type = Int)
           key2 = (expected_types = (Bool, Symbol, Vector{Symbol}), default = false)
       end
       return quote 
           (key1 = $key1, key2 = $key2) 
       end |> esc
   end

julia> @ex_macro key2 = true

ERROR: ArgumentError: No value provided for key `key1`
...

julia> @ex_macro key1=false key2=a
ERROR: ArgumentError: In `key1 = rhs` expression, rhs (= false) has type Bool, which is not one of the expected types (Int64)
...

julia> @ex_macro key1=1 key2=(a,b,c)
(key1 = 1, key2 = [:a, :b, :c])
```

## Function Parsing 
Depending on the structure and purpose of your macro, you may also need to parse expressions involving function calls or function definitions, in order to transform or extract information from such expressions. This package provides the `FuncDef`, `FuncCall`, and `FuncArg` types, along with the `from_expr` and `to_expr` functions, for extracting information from function expressions. 

```julia
julia> ex = quote 
            """
                f(a, b; key1, kwargs...)
            """
            function f(a::T, b::Int; key1="abc", kwargs...) where {T}
                return nothing
            end
        end
julia> f = from_expr(FuncDef, ex)
       FuncDef - begin
           #= REPL[140]:2 =#
           #= REPL[140]:2 =# Core.@doc "    f(a, b; key1, kwargs...)\n" function f(a::T, b::Int; key1 = "abc", kwargs...) where T
                   #= REPL[140]:5 =#
                   #= REPL[140]:6 =#
                   return nothing
                end
       end

julia> f.args
       2-element Vector{FuncArg}:
       FuncArg - a::T
       FuncArg - b::Int

julia> f.kwargs
       OrderedCollections.OrderedDict{Symbol, FuncArg} with 2 entries:
       :key1   => FuncArg - key1 = abc
       :kwargs => FuncArg - kwargs...

julia> f.line
       :(#= REPL[140]:2 =#)

julia> f.whereparams
       1-element view(::Vector{Any}, 2:2) with eltype Any:
       :T

julia> to_expr(f)
       quote
       #= REPL[140]:2 =#
       #= REPL[140]:2 =# Core.@doc "    f(a, b; key1, kwargs...)\n" function f(a::T, b::Int; key1 = "abc", kwargs...) where T
            #= REPL[140]:5 =#
            #= REPL[140]:6 =#
            return nothing
       end
       end
```


## Similar Packages 
- [`MacroTools.jl`](https://github.com/FluxML/MacroTools.jl)
- [`Expronicon.jl`](https://github.com/Roger-luo/Expronicon.jl/tree/main)