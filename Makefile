boot:: sandbox
	cabal install --only-dependencies --enable-tests
	cabal configure -f llvm
	cabal build

sandbox::
	cabal sandbox init

clean:
	cabal sandbox delete
	cabal clean
