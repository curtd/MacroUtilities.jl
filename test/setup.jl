using MacroUtilities, MacroUtilities.MLStyle
using TestingUtilities, Test 

if VERSION ≥ v"1.9"
    macro testthrows(msg, ex)
        return quote 
            $Test.@test_throws $msg $ex
        end |> esc 
    end
else
    macro testthrows(msg, ex)
        return quote
        end 
    end
end