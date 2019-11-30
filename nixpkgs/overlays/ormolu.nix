self: super:

let source = super.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "5b9861b57fd74d2cc65d0fb201329f5bd08fee9b"; # update as necessary
      sha256 = "0xazwm6ldbv0kk7fskb8q1f3s2iv6n0gzzzr7aq28q6zcijj01p3"; # as well
    };
    ormolu = import source { pkgs = self; };
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" = super.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
  };
}
