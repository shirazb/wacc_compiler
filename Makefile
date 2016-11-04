all:
        cabal update
	cabal configure
	cabal build

clean:
	cabal clean
