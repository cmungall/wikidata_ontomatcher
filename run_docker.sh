#!/bin/sh
docker run -v $PWD:/work -w /work --rm -ti cmungall/wikidata_ontomatcher pl2sparql -u sparqlprog_wikidata -u wikidata_ontomatcher -e -s wd "$@"
