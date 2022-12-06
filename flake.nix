{
  description = "Advent of Code 2022";

  inputs = {
    nixpkgs = { url = github:nixos/nixpkgs/nixpkgs-unstable; };
    flake-utils = { url = github:numtide/flake-utils; };
    nix-projects = { url = github:sutherlanda/nix-projects; };
  };

  outputs = { self, nixpkgs, flake-utils, nix-projects, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        rootPath = "$PWD";

        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        haskellPackages = p: [
          p.dotenv
          p.wai
          p.warp
          p.split
        ];

        projectLib = import nix-projects;

        # define haskell projects where each project has the following
        #   config consisting of src directory, build and artifact directories
        #   dependencies (other projects)
        # the created project will have commands for building, running, ghcide

        haskellProjectConfigs = [
          rec {
            name = "aoc";
            projectRoot = "${rootPath}/${name}";
            srcDir = "${projectRoot}/src";
            executables = {
              main = "Main.hs";
            };
          }
        ];

        haskellLib = projectLib.lib.projects.haskell {
          inherit pkgs haskellPackages;
        };

      in
      { devShell = haskellLib.mkShell haskellProjectConfigs; });
}
