function first_lnn_in_block(expr::Expr)
    if Meta.isexpr(expr, :block)
        for arg in expr.args 
            if arg isa LineNumberNode
                return arg 
            end
        end
    end
    return nothing 
end
first_lnn_in_block(x) = nothing

macro arg_error(expr, args...)
    final_arg = args[end]
    final_arg isa String || Meta.isexpr(final_arg, :string) || throw(ArgumentError("Expected final argument $(final_arg) to be a String or String-expression"))

    arg_error_args = Any[:("In expression `$($expr)`")]
    for arg in args[1:end-1]
        if arg isa String || Meta.isexpr(arg, :string)
            push!(arg_error_args, arg)
        else
            push!(arg_error_args, :("`$($arg)`"))
        end
    end

    return quote 
        ArgumentError(join([$(arg_error_args...)], ", ")*" - "*$final_arg)
    end |> esc
end