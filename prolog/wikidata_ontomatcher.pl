/** <module>

  matches all classes in an ontology to wikidata

*/

:- module(wikidata_ontomatcher,
          [match_classes/0,
           match_all/0,
           match_entity/3,
           search_wd/4,

           extract_and_save/1,
           extract_ontology/2,
           extract_ontology/3]).


:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).

:- setting(target_graph, any, target_graph, 'Where rdf results are cached in memory').
:- setting(cached_db_file, any, 'CACHE.ttl', 'Where rdf results are cached on disk').
:- setting(save_frequency, float, 1.0, 'How frequently results are saved >0, =<1.').

:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(schema_org, 'http://schema.org/').
:- rdf_register_prefix(obo, 'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(oio, 'http://www.geneontology.org/formats/oboInOwl#').

% match all classes
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

%! match_entity(+Object:iri, ?MatchingTriples:list, +Opts:list) is det.
%
%  wraps search_wd/3 to create a sub-ontology around all matched entities
%
%  MatchingTriples=[rdf(S1,P1,O1), ...]
%
%  Will not attempt a match if a skos closeMatch predicate already exists
%
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



%! search_wd(+Object:iri, +LabelTerm:atom, ?Triples:list, +Opts:list) :-
%
%  remote search of wikidata for entity with Object iri and label LabelTerm
%
%  For each match X, enrich using enrich_matches/3
%
%  side effects:
%   - save enriched Triples via rdf_assert
%   - potentially persist current graph to disk
search_wd(Entity, Term, Triples, Opts) :-
        option(search_limit(Limit),Opts,25),
        debug(ontomatcher,' Searching on: ~w  // limit=~w',[Term, Limit]),
        findall(X, (wd ?? entity_search(Term,X,Limit)), Xs),
        enrich_matches(Entity, Xs, Triples),
        setting(target_graph,G),
        % include self-match to know that we have searched
        forall(member(rdf(S,P,O),[rdf(Entity,skos:closeMatch,Entity)|Triples]),
               rdf_assert(S,P,O,G)),
        setting(save_frequency,Freq),
        random(Random),
        (   Random < Freq
        ->  safe_rdf_save(Opts)
        ;   debug(ontomatcher,'Not saving this iteration ~w > ~w',[Random,Freq])).

%  for each match X, Triples is unified with:
%
%   - Entity skos:closeMatch X .
%   - X rdfs:label <label> .  # english label
%   - X Pred Parent .         # where Parent is a named entity
%   - Parent rdfs:label <label> 
% TODO: make language customizable
enrich_matches(_, [], []) :- !.
enrich_matches(Entity, Xs, Triples) :-
        findall([
                 rdf(Entity,skos:closeMatch, X),
                 rdf(X,rdfs:label,XN),
                 rdf(T,rdfs:label,TN),
                 rdf(X,TP,T)],
                (   wd ?? (member(X,Xs),
                           rdf(X,TP,T),
                           enlabel(X,XN),
                           enlabel(T,TN)
                          )),
                Results),
        flatten(Results, TriplesAll),
        sort(TriplesAll, Triples1),
        maplist(fix_prop,Triples1,Triples2),
        findall(rdf(X,P,Y),
                (   wd ?? (member(X,Xs),
                           member(P,[skos:altLabel]),
                           rdf(X,P,Y),
                           lang(Y)="en"
                          )),
                AnnTriples),
        append(Triples2,AnnTriples,Triples).
        
fix_prop(rdf(S,'http://www.wikidata.org/prop/direct/P279',O), rdf(S,rdfs:subClassOf,O)) :- !.
fix_prop(rdf(S,'http://www.wikidata.org/prop/direct/P31',O), rdf(S,rdf:type,O)) :- !.
fix_prop(rdf(S,'http://www.wikidata.org/prop/direct/P910',O), rdf(S,skos:inScheme,O)) :- !.
fix_prop(T,T).
foo_('0').

map_literal_prop(skos:altLabel, oio:hasRelatedSynonym).
map_literal_prop(schema_org:description, obo:'IAO_0000115').
map_literal_prop(rdfs:label, rdfs:label).

extract_and_save(Outfile) :-
        findall(X,seed(X),Xs),
        extract_ontology(Xs,_Triples,[graph(extract)]),
        rdf_save_turtle(Outfile,[graph(extract)]).


%! extract_ontology(+SeedEntities:list, ?Triples:list) :-
%
%  extract an ontology module from wikidata.
%
%  1. Expand seed entities with all superclasses, and subclasses (for non-general nodes)
%  2. enrich with all labels, descriptions, and altLabels
%  3. link all entities
extract_ontology(Xs,Triples) :-
        extract_ontology(Xs,Triples,[]).
extract_ontology(Xs,Triples,Opts) :-
        length(Xs,NumX),
        debug(ontomatcher,'expanding ontology: ~w entities',[NumX]),        
        setof(Y,expand_ontology(Xs,Y),Ys),
        length(Ys,NumY),
        debug(ontomatcher,'expanded ontology ~w -> ~w entities',[NumX,NumY]),
        append(Xs,Ys,Cs),
        findall(T,chunked_enrich_ontology_with(Cs,T,Cs),Triples),
        (   member(graph(G),Opts)
        ->  forall(member(rdf(S,P,O),Triples),
                   rdf_assert(S,P,O,G))
        ;   true).

chunked_enrich_ontology_with(Cs,T,All) :-
        select_first_n(Cs,10,Cs1,Tail),
        Cs\=[],
        !,
        debug(ontomatcher,'chunk: ~w',[Cs]),        
        (   enrich_ontology_with(Cs1,T,All)
        ;   chunked_enrich_ontology_with(Tail,T,All)).
        
enrich_ontology_with(Xs,rdf(X,P2,V),_) :-
        map_literal_prop(P1,P2),
        wd_query( (member(X,Xs),rdf(X,P1,V),lang(V)="en") ).

enrich_ontology_with(Xs,rdf(X,rdfs:subClassOf,Y),All) :-
        ??(wd, (member(X,Xs),rdf(X,'http://www.wikidata.org/prop/direct/P279',Y))),
        % remove dangling
        member(Y,All).
enrich_ontology_with(Xs,rdf(X,rdf;type,Y),All) :-        
        ??(wd, (member(X,Xs),rdf(X,'http://www.wikidata.org/prop/direct/P31',Y))),
        % remove dangling
        member(Y,All).

expand_ontology(Xs,Y) :-
        select_chunk(Xs,10,Chunk),
        debug(ontomatcher,'Querying chunk for instance-of',[]),
        ??(wd, (member(X,Chunk), rdf_path(X,zeroOrMore('http://www.wikidata.org/prop/direct/P31'),Y))).

expand_ontology(Xs,Y) :-
        select_chunk(Xs,10,Chunk),
        debug(ontomatcher,'Querying chunk for subClassOf',[]),
        ??(wd, (member(X,Chunk), rdf_path(X,zeroOrMore('http://www.wikidata.org/prop/direct/P279'),Y))).

expand_ontology(Xs,D) :-
        member(X,Xs),
        % do not do this for high-level nodes
        ??(wd,
           aggregate(count(D),
                     rdf_path(D,oneOrMore('http://www.wikidata.org/prop/direct/P279'),X),
                     NumD),
           NumDLit),
        NumDLit=literal(type(_,NumDA)),
        atom_number(NumDA,NumD),
        debug(ontomatcher,'entity ~q has ~w subclasses',[X,NumD]),
        % semi-arbitrary cut-off
        NumD < 100,
        !,
        ??(wd,rdf_path(D,oneOrMore('http://www.wikidata.org/prop/direct/P279'),X)).

wd_query(Q) :-
        % for some reason altLabel queries frequently fail..
        catch(try_n_times( ??(wd,Q), 2 ),
              _E,
              fail).

% ----------------------------------------
% util predicates
% ----------------------------------------

safe_rdf_save(Opts) :-
        setting(target_graph,G),
        get_cached_db_file(File, Opts),
        debug(ontomatcher,' Saving ~w to: ~w',[G,File]),
        atom_concat(File,'.tmp',FileTmp),
        rdf_save_turtle(FileTmp,[graph(G)]),
        copy_file(FileTmp,File).




        

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


% occasionally match queries will fail;
% we iterate increasing the pause duration a certain amount of times.
try_n_times(Goal) :-
        try_n_times(Goal,3).
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

select_chunk(L,N,Chunk) :-
        select_first_n(L,N,H,T),
        (   Chunk=H
        ;   select_chunk(T,N,Chunk)),
        Chunk\=[].

select_first_n(L,0,[],L) :- !.
select_first_n([],_,[],[]) :- !.
select_first_n([X|L],N,[X|H],T) :-
        N>0,
        !,
        Nm1 is N-1,
        select_first_n(L,Nm1,H,T).

