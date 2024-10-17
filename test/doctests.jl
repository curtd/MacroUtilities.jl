@testitem "Doctests" begin 
    using Test, Documenter, MacroUtilities

    DocMeta.setdocmeta!(MacroUtilities, :DocTestSetup, :(using MacroUtilities); recursive=true)
end