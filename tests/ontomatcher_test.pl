:- use_module(library(wikidata_ontomatcher)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).

:- begin_tests(ontomatcher).

%:- debug(sparqlprog).
:- debug(ontomatcher).

test(foo) :-
        T=river,
        search_wd(x,T,Matches,[]),
        maplist(writeln, Matches),
        nl,
        writeln('searching again... (should use cache)'),
        search_wd(x,T,Matches2,[]),
        maplist(writeln, Matches2),
        nl.

:- end_tests(ontomatcher).
    
