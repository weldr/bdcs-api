default: all

all: bdcs-api-server

sandbox:
	[ -d .cabal-sandbox ] || cabal sandbox init

bdcs-api-server: sandbox
	cabal sandbox add-source /bdcs/
	cabal update
	cabal install --dependencies-only
	cabal configure
	cabal build

clean:
	cabal clean

hlint: sandbox
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	[ -x .cabal-sandbox/bin/hlint ] || cabal install hlint
	cabal exec hlint .

tests: sandbox
	cabal sandbox add-source /bdcs/
	cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test

.PHONY: sandbox bdcs-api-server clean tests hlint
