# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

#PACKNAME=sparkle
#include ../Makefile.inc

SWIPL = swipl  -L0 -G0 -T0  -p library=prolog
all: test

check:
install:
clean:


test:
	echo "run 'make itest' for integration tests"
itest:
	$(SWIPL) -l tests/tests.pl -g run_tests,halt

t-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt

# --------------------
# rdf2s
# --------------------

chem := 21294996

scratch/propinst-%.tsv:
	pq-wikidata -f tsv  "$*(P),enlabel(P,N)" "x(N,P)" > $@

scratch/p2c.tsv:
	pq-wikidata -d sparqlprog -f tsv -l -L enlabel  "properties_for_this_type(C,P)"  > $@


# --------------------
# Docker
# --------------------

# Get version from pack
VERSION = v$(shell swipl -l pack.pl -g "version(V),writeln(V),halt.")

show-version:
	echo $(VERSION)

TN = wikidata_ontomatcher
IM = cmungall/$(TN)

docker-all: docker-clean docker-build docker-run

docker-clean:
	docker kill $(IM) || echo not running ;
	docker kill $(IM) || echo not running ;
	docker rm $(IM) || echo not made 

docker-build:
	@docker build -t $(IM):$(VERSION) . \
	&& docker tag $(IM):$(VERSION) $(IM):latest

docker-run:
	docker rm $(IM) || echo ok
	docker run --name $(TN) $(IM)

docker-publish: docker-build
	@docker push $(IM):$(VERSION) \
	&& docker push $(IM):latest
