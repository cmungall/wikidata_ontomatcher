:- use_module(library(wikidata_ontomatcher)).
:- use_module(library(sparqlprog_wikidata)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl), [label/2]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).


:- begin_tests(envo,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        % load into a test-specific graph due to cache issue
        rdf_load('tests/river.ttl',[cache(false),
                                         graph(obo_util)]).



%:- debug(sparqlprog).
:- debug(ontomatcher).

test(match_class) :-
        CACHE='tests/envo_test_cached_triples.ttl',
        setting(wikidata_ontomatcher:target_graph,G),
        T=river,
        label_of(T,C),
        match_entity(C,Matches,[cached_db_file(CACHE)]),
        maplist(writeln, Matches),
        assertion( member(rdf('http://www.wikidata.org/entity/Q4022',_,literal(lang(en,river))), Matches)),
        assertion( rdf('http://purl.obolibrary.org/obo/ENVO_00000022',skos:closeMatch,'http://www.wikidata.org/entity/Q4022',G) ),
        nl,
        writeln('searching again... (should use cache)'),
        %set_setting(wikidata_ontomatcher:cached_db_file, 'tests/envo_test_cached_triples.ttl'),
        match_entity(C,Matches2,[cached_db_file(CACHE)]),
        maplist(writeln, Matches2),
        assertion( rdf('http://purl.obolibrary.org/obo/ENVO_00000022',skos:closeMatch,'http://www.wikidata.org/entity/Q4022',G) ),
        nl.

test(punning) :-
        writeln('Should have type and subclassof...'),
        search_wd(x,sericulture,Matches,[]),
        maplist(writeln, Matches),
        triples_contain('http://www.wikidata.org/entity/Q864650',rdfs:subClassOf,'http://www.wikidata.org/entity/Q80962',Matches),
        triples_contain('http://www.wikidata.org/entity/Q864650',rdf:type,'http://www.wikidata.org/entity/Q29028649',Matches),
        nl.

:- rdf_meta triples_contain(r,r,r,o).
triples_contain(S,P,O,L) :-
        assertion( member(rdf(S,P,O),L) ).

        

:- end_tests(envo).
    
