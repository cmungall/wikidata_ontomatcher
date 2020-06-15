:- use_module(library(wikidata_ontomatcher)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).

:- begin_tests(ontomatcher).

%:- debug(sparqlprog).
:- debug(ontomatcher).

test(search) :-
        T=river,
        search_wd(x,T,Matches,[cached_db_file('tests/test_cache.ttl')]),
        maplist(writeln, Matches),
        nl.


:- end_tests(ontomatcher).
    
