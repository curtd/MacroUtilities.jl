using Documenter

using Pkg
docs_dir = joinpath(@__DIR__, "..")
project_dir = isempty(ARGS) ? @__DIR__() : joinpath(pwd(), ARGS[1])
Pkg.activate(project_dir)

using MacroUtilities

DocMeta.setdocmeta!(MacroUtilities, :DocTestSetup, :(using MacroUtilities); recursive=true)

makedocs(;
    modules=[MacroUtilities],
    authors="Curt Da Silva",
    repo="https://github.com/curtd/MacroUtilities.jl/blob/{commit}{path}#{line}",
    sitename="MacroUtilities.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://curtd.github.io/MacroUtilities.jl",
        edit_link="main",
        assets=String[],
        ansicolor=true
    ),
    pages=[
        "API" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/curtd/MacroUtilities.jl.git",
    devbranch="main", push_preview=true
)
