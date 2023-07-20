using MacroUtilities
using Test 

if VERSION ≥ v"1.9"
    using Aqua
    Aqua.test_all(MacroUtilities)
end

@testset "MacroUtilities.jl" begin 
    include("TestMacroUtilities.jl")
end