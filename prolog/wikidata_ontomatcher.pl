/** <module>

  matches all classes in an ontology to wikidata

*/

:- module(wikidata_ontomatcher,
          [match_classes/0,
           search_wd/4]).


:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).

:- setting(target_graph, any, target_graph, 'Where rdf results are caches').


search_wd(C, Term, Triples, Opts) :-
        debug(ontomatcher,' Searching on: ~w',[Term]),
        option(search_limit(Limit),Opts,25),
        findall([
                 rdf(C,skos:exactMatch, X),
                 rdf(X,rdfs:label,XN),
                 rdf(T,rdfs:label,TN),
                 rdf(X,rdfs:subClassOf,T)
                 ],
                (   wd ?? (
                           entity_search(Term,X,Limit),
                           subclass_of(X,T),
                           enlabel(X,XN),
                           enlabel(T,TN)
                          )),
                Results),
        flatten(Results, TriplesAll),
        sort(TriplesAll, Triples),
        setting(target_graph,G),
        % include self-match to know that we have searched
        forall(member(rdf(S,P,O),[rdf(C,skos:exactMatch,C)|Triples]),
               rdf_assert(S,P,O,G)),
        option(db_file(File),Opts,'CACHE.ttl'),
        rdf_save_turtle(File,[graph(G)]).


/*

  search_wd(Term, Matches, Opts) :-
        debug(ontomatch,' Searching on: ~w',[Term]),
        option(search_limit(Limit),Opts,25),
        findall(match(X,XN,T,TN), (wd ??
                   (   entity_search(Term,X,Limit),
                       subclass_of(X,T),
                       enlabel(X,XN),
                       enlabel(T,TN)
                   )), Tuples),
        
        group_match_tuples(Tuples, [], Matches).

group_match_tuples([], Matches, Matches).
group_match_tuples([Tuple|Rest], Matches, Acc) :-
        Tuple=match(X,_,T,TN),
        Matches=[M|MatchesRest],
        M = m(X,_)-TL,
        !,
        group_match_tuples(Rest, [M-[t(T,TN)|TL] | MatchesRest], Acc).
group_match_tuples([Tuple|Rest], Matches, Acc) :-
        Tuple=match(X,XN,T,TN),
        !,
        group_match_tuples(Rest, [m(X,XN)-[t(T,TN)] | Matches], Acc).

  */
        

match_class(C, Matches, _Opts):-
        setting(target_graph,G),
        debug(ontomatcher,'CHECKING: ~w',[G]),
        setof(rdf(C,skos:exactMatch,M),
              rdf(C,skos:exactMatch,M,G),
              Matches),
        debug(ontomatcher,'~w CACHED MATCHES: ~w',[C,Matches]),
        !.


match_class(C, Matches, Opts):-
        debug(ontomatcher,'Matching: ~w',[C]),
        rdf(C,rdfs:label,Label),
        ensure_atom(Label, CN),
        search_wd(C, CN, Matches, Opts),
        !,
        debug(ontomatcher,'~w ~w MATCHES: ~w',[C,CN,Matches]).
match_class(_, [], _).

match_classes :-
        match_classes([]).

match_classes(Opts) :-
        forall(rdf(C,rdf:type,owl:'Class'),
               match_class(C,_,Opts)).

        


