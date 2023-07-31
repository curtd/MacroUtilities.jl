module TestMacroUtilities 
    using MacroUtilities, MacroUtilities.MLStyle
    using TestingUtilities, Test 

    if VERSION ≥ v"1.7"
        macro testthrows(msg, ex)
            return quote 
                Test.@test_throws $msg $ex
            end |> esc 
        end
    else
        macro testthrows(msg, ex)
            return quote
            end 
        end
    end

    include("expressions/_test_expressions.jl")
    include("macros/_test_macros.jl")   
       
end