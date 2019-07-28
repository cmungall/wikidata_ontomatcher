/** <module>

  matches all classes in an ontology to wikidata

*/

:- module(wikidata_ontomatcher,
          [match_classes/0,
           match_all/0,
           match_entity/3,
           search_wd/4]).


:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).

:- setting(target_graph, any, target_graph, 'Where rdf results are cached in memory').
:- setting(cached_db_file, any, 'CACHE.ttl', 'Where rdf results are cached on disk').
:- setting(save_frequency, float, 1.0, 'How frequently results are saved >0, =<1.').


:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').

try_n_times(Goal,N) :-
        try_n_times(Goal,N, 1).
try_n_times(Goal,N,Pause) :-
        N > 0,
        !,
        Nm1 is N-1,
        Pause2 is Pause * 10,
        catch(Goal,
              E,
              (   print_message(error, E),
                  sleep(Pause),
                  try_n_times(Goal,Nm1,Pause2))).
try_n_times(Goal,_,_) :-
        throw(error(tried_and_failed(Goal))).


        

search_wd(C, Term, Triples, Opts) :-
        debug(ontomatcher,' Searching on: ~w',[Term]),
        option(search_limit(Limit),Opts,25),
        (   rdf(C,rdf:type,owl:'NamedIndivdual')
        ;   TP=rdf:type,
            WG=instance_of(X,T)
        ;   TP=rdfs:subClassOf,
            WG=subclass_of(X,T)),
        findall([
                 rdf(C,skos:closeMatch, X),
                 rdf(X,rdfs:label,XN),
                 rdf(T,rdfs:label,TN),
                 rdf(X,TP,T)
                 ],
                (   wd ?? (
                           entity_search(Term,X,Limit),
                           WG,
                           enlabel(X,XN),
                           enlabel(T,TN)
                          )),
                Results),
        flatten(Results, TriplesAll),
        sort(TriplesAll, Triples),
        setting(target_graph,G),
        % include self-match to know that we have searched
        forall(member(rdf(S,P,O),[rdf(C,skos:closeMatch,C)|Triples]),
               rdf_assert(S,P,O,G)),
        setting(save_frequency,Freq),
        random(Random),
        (   Random < Freq
        ->  safe_rdf_save(Opts)
        ;   debug(ontomatcher,'Not saving this iteration ~w > ~w',[Random,Freq])).


safe_rdf_save(Opts) :-
        setting(target_graph,G),
        get_cached_db_file(File, Opts),
        debug(ontomatcher,' Saving ~w to: ~w',[G,File]),
        atom_concat(File,'.tmp',FileTmp),
        rdf_save_turtle(FileTmp,[graph(G)]),
        copy_file(FileTmp,File).



match_entity(C, Matches, _Opts):-
        setting(target_graph,G),
        debug(ontomatcher,'CHECKING: ~w',[G]),
        % if we already have matches (even to self) in target graph
        % then we have already matched this class
        setof(rdf(C,skos:closeMatch,M),
              rdf(C,skos:closeMatch,M,G),
              Matches),
        debug(ontomatcher,'~w CACHED MATCHES: ~w',[C,Matches]),
        !.


match_entity(C, Matches, Opts):-
        setting(target_graph,TG),
        debug(ontomatcher,'Matching: ~w',[C]),
        rdf(C,rdfs:label,Label,SG),
        % don't include classes that come from wikidata itself
        SG\=TG,
        \+ deprecated(C),
        ensure_atom(Label, CN),
        atom_length(CN, Len),
        Len < 30,
        try_n_times(search_wd(C, CN, Matches, Opts),
                    4),
        !,
        debug(ontomatcher,'~w ~w MATCHES: ~w',[C,CN,Matches]).
match_entity(_, [], _).


        



match_all :-
        match_all([]).
match_classes :-
        match_classes([]).
        
match_classes(Opts) :-
        load_cached_db(Opts),
        forall(rdf(C,rdf:type,owl:'Class'),
               match_entity(C,_,Opts)).

match_all(Opts) :-
        load_cached_db(Opts),
        forall(rdf(C,rdf:type,_),
               match_entity(C,_,Opts)).

        

load_cached_db(Opts) :-
        get_cached_db_file(File, Opts),
        setting(target_graph,G),
        (   exists_file(File)
        ->  debug(ontomatcher,'Using cached: ~w',[File]),
            rdf_load(File,[graph(G)])
        ;   debug(ontomatcher,'No cached db file yet: ~w',[File])).



get_cached_db_file(File, Opts) :-
        setting(cached_db_file, DefaultFile),
        option(cached_db_file(File), Opts, DefaultFile).


