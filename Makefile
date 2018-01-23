default: all

all: bdcs-api-server

sandbox:
	[ -d .cabal-sandbox ] || cabal sandbox init

bdcs-api-server: sandbox
	cabal update
	# happy appears to be required for warp(?) and won't be installed by --dependencies-only
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	cabal install --dependencies-only
	cabal configure
	cabal build

clean:
	cabal clean

hlint: sandbox
	cabal update
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	[ -x .cabal-sandbox/bin/hlint ] || cabal install hlint
	.cabal-sandbox/bin/hlint .

tests: sandbox
	cabal update
	# happy appears to be required for warp(?) and won't be installed by --dependencies-only
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test --show-details=always

ci:
	docker build -t welder/bdcs-api -f Dockerfile.build .

ci_after_success:
	[ -x ~/.cabal-sandbox/bin/hpc-coveralls ] || cabal update && cabal install hpc-coveralls
	~/.cabal-sandbox/bin/hpc-coveralls --display-report bdcs-api-server spec

.PHONY: sandbox bdcs-api-server clean tests hlint ci ci_after_success
