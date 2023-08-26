@compile_workload begin 

    ex = :(if a b; elseif c d; elseif  e f; else g end)
    f = from_expr(IfElseExpr, ex)
    to_expr(f)

    ex = quote a end 
    expr = from_expr(BlockExpr, ex)
    to_expr(f)

    ex = quote 
        """
        some doc
        """
        f(x) = 1
    end
    f = from_expr(DocExpr{FuncDef}, ex.args[2])
    to_expr(f)

    ex = quote 
        """
        doc
        """
        @m f(x)
    end
    f = from_expr(DocExpr{MacroCall}, ex.args[2])
    to_expr(f)

    ex = quote 
        """
            f(a, b; key1, kwargs...)
        """
        function f(a::T, b::Int; key1="abc", kwargs...) where {T}
            return nothing
        end
    end
    f = from_expr(FuncDef, ex)
    to_expr(f)

    f = from_expr(FuncCall, :(A.f(1, 2, 3; k=1, l=2, m=3)))
    g = map_args(t->FuncArg(t; value=t.value+1), f)
    g = map_kwargs(t->FuncArg(t; value=t.value+1), f)
    names_only(f)

    ex = :(struct A 
            key1::String 
            key2::Int
            key3::Bool
        end)
    f = from_expr(StructDef, ex)
    to_expr(f)
    c = kwarg_constructor(f, (key1 = "abc", key2 = :(g())))
    to_expr(c)
    c = copy_constructor(f; input_var=:t)
    to_expr(c)

    ex = :(f(_))
    new_expr, was_replaced = replace_symbols(ex, [:_ => :x])

    f = ReplaceSymbols(:((g(_))::T), :_)
    f(:_ => :x) 
    
    args = Any[:(key1=1), :(key2="abc")]
    @parse_kwargs args... begin 
        key1::Int
        key2::String 
        key3::Union{Nothing, Bool} = nothing
    end
end