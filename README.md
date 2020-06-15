[![DOI](https://zenodo.org/badge/13996//sparqlprog_wikidata.svg)](https://zenodo.org/badge/latestdoi/13996//sparqlprog_wikidata)

# Match an ontology to Wikidata

This application will scan an ontology (given in some RDF serialization) and match classes and/or individuals with entities in wikidata.

Command line usage:

    wd-ontomatch -d ontomatcher -i envo.owl match_classes

This will produce a file `CACHE.ttl` that contains `skos:closeMatch` triples, as well as rdfs:label triples for all matching entities.

This can then be used in combination with rdf_matcher:

    rdfmatch -f tsv -l -i envo.owl -i CACHE.ttl exact


```
wd-ontomatch  -d ontomatcher -i $* -a wikidata_ontomatcher:save_frequency=0.05 -a wikidata_ontomatcher:cached_db_file=matches/wd-$*-cache.ttl match_all
```


