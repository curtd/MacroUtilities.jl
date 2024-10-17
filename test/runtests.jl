using TestItemRunner, TestItems

@testsnippet TestSetup begin 
    using MacroUtilities
    include(joinpath(pkgdir(MacroUtilities), "test/setup.jl"))
end

@run_package_tests verbose=true