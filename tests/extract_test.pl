:- use_module(library(wikidata_ontomatcher)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).


:- begin_tests(extract,
               []).

:- debug(sparqlprog).
:- debug(ontomatcher).


test(extract) :-
        G=river,
        extract_ontology(['http://www.wikidata.org/entity/Q55659107',
                          'http://www.wikidata.org/entity/Q4022'], Triples,[graph(G)]), % intermittent natural watercourse
        %extract_ontology(['http://www.wikidata.org/entity/Q55659107'], Triples,[graph(G)]), % intermittent natural watercourse
        %extract_ontology(['http://www.wikidata.org/entity/Q4022'], Triples,[graph(G)]), % river
        maplist(writeln, Triples),
        rdf_save_turtle('tests/extract_river.ttl',[graph(G)]),
        nl.

:- rdf_meta triples_contain(r,r,r,o).
triples_contain(S,P,O,L) :-
        assertion( member(rdf(S,P,O),L) ).

        

:- end_tests(extract).
    
