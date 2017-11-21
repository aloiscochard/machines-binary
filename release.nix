let config = {
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: rec {

        machines-binary = self.callPackage ./default.nix {};

      };
    };
  };
};

pkgs = import <nixpkgs> {inherit config;};

in {
  machines-binary = pkgs.haskellPackages.callPackage ./default.nix {};
}

