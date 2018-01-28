{
    allowUnfree = true;
    packageOverrides = super: let self = super.pkgs; in
    {
        myHaskellEnv = self.haskellPackages.ghcWithHoogle (hp: with hp; [
            criterion
	    cabal-install
	    mtl
	    QuickCheck
	    hspec
	    ghcid
	    alex
	    happy
	    brittany
	    hlint
	]);
	st = super.st.override {
	    patches = [ ./st/clipboard-colors.patch ./st/base16-brewer.patch ];
	};
	haskellPackages = super.haskellPackages.override {
	    overrides = self: super: {
	        myxmonad = self.callPackage ./xmonad {};
	    };
	};
    };
}
