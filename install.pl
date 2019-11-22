
install_requirements :-
        ensure_loaded(pack),
        % https://swi-prolog.discourse.group/t/www-swi-prolog-org-goes-https/811/3
        ensure_loaded(library(prolog_pack)),
        set_setting(prolog_pack:server, 'https://www.swi-prolog.org/pack/'),
        Opts=[interactive(false)],
        requires(X),
        pack_install(X, Opts),
        fail.
install_requirements.
    
