{
  description = "A pascal compiler, named after the legend Niklaus Emil Wirth";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    crane,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      overlays = [(import rust-overlay)];
      pkgs = import nixpkgs {inherit system overlays;};
      # inherit (pkgs) lib;

      craneLib =
        (crane.mkLib pkgs)
        .overrideToolchain
        (pkgs.rust-bin.stable.latest.default.override
          {extensions = ["rust-src" "rust-analyzer"];});

      commonArgs = {
        src = craneLib.cleanCargoSource ./.;
        strictDeps = true;
        nativeBuildInputs = [];
        buildInputs = [];
      };

      emil-crate = craneLib.buildPackage (commonArgs
        // {
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        });
    in {
      checks = {emil-crate = emil-crate;};
      packages.default = emil-crate;
      apps.default = flake-utils.lib.mkApp {drv = emil-crate;};
      devShells.default = craneLib.devShell {
        checks = self.checks.${system};
        packages = [pkgs.taplo];
      };
    });
}
