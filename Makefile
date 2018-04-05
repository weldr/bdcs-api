default: all

all: bdcs-api-server

sandbox:
	# Delete the sandbox if it already exists - The build-and-test target mounts
	# the source directory into the container.  If a sandbox already exists, it
	# can result in some pretty bizarre errors when trying to install deps.
	[ -d .cabal-sandbox ] && cabal sandbox delete && cabal clean
	cabal sandbox init

bdcs-api-server: sandbox
	cabal update
	cabal install --dependencies-only --reorder-goals --force-reinstalls
	cabal configure
	cabal build

clean:
	cabal clean

hlint:
	if [ -z "$$(which hlint)" ]; then \
		echo hlint not found in PATH - install it; \
		exit 1; \
	else \
		hlint .; \
	fi

tests: sandbox
	cabal update
	cabal install --dependencies-only --enable-tests --reorder-goals --force-reinstalls
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test --show-details=always

build-and-test: Dockerfile.build
	sudo docker build -t welder/bdcs-api-build-img -f $< .
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs-api/ welder/bdcs-api-build-img

bdcs-api-img: build-and-test
	sudo docker build -t welder/bdcs-api-img .

ci: bdcs-api-img

ci_after_success:
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs-api/ \
	    --env "TRAVIS=$$TRAVIS" --env "TRAVIS_JOB_ID=$$TRAVIS_JOB_ID" --entrypoint /usr/bin/make welder/bdcs-api coveralls

coveralls:
	if [ -z "$$(which hpc-coveralls)" ]; then \
		echo hpc-coveralls not found in PATH - install it; \
		exit 1; \
	else \
		hpc-coveralls --display-report spec; \
	fi

.PHONY: sandbox bdcs-api-server clean tests hlint ci ci_after_success coveralls
