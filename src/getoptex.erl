%% GetoptEx project
%% Erlang GetOpt Extended services
%% @ 2012, 2014 Angel J. Alvarez
%% 2012 Initial design Catedromol project
%% 2014 Cosmetic Fixes
%%
%%

-module(getoptex).
-author("angeljalvarezmiguel@gmail.com").
-compile(export_all).

-import(lists, [foldl/3]).


% Parse cmdline args with getopt and then checkout additional conditions on requested options
parse_args(SpecList, ArgList, FunList) ->
    case getopt:parse(SpecList, ArgList) of
    {ok, {Options, NonOptArgs}} -> 
                                    check_args(Options, NonOptArgs, FunList);           %% Successfull getopt
    {error, {Reason, Data}} ->
                                    {error, {Reason, Data}}                             %% getopt returned some error
    end.

check_args(Options, NonOptArgs, FunList) ->
    case foldl(fun do_check/2, Options, FunList) of
    COpts when is_list(COpts) -> 
                                {ok,{COpts, NonOptArgs}};                               %% make a ok result out of an Opts list
    Other                     ->
                                Other                                                   %% Oh boy! this is a error tuple...
    end.

%% apply a checking fun 
do_check(Fun,Acc) when is_function(Fun,1), is_list(Acc) -> Fun(Acc);
do_check(_,Acc) -> Acc.


%% Fold over a module list accumulating optspecs and funs exported
collect_option_providers(Modules) when is_list(Modules) ->
    {Opts, Funs} = foldl(fun(Mod,{OptList,FunList}) ->
                                            {ModOpts,ModFuns} = Mod:option_specs(),
                                            {ModOpts ++ OptList, ModFuns ++ FunList} end
                        ,{[],[]}
                        ,Modules),
    {lists:reverse(Opts), lists:reverse(Funs)}.

% get modules composing a given application
-spec list_app_modules(atom(),atom(),atom()) -> [atom() | tuple()].
list_app_modules(App,Class,Key) when is_atom(App), is_atom(Class), is_atom(Key) ->
    {ok,ModuleList}  = application:get_key(App,modules),
    list_modules(Class,Key,ModuleList).

% get modules exporting a value
-spec list_modules(atom(),atom(),[atom() | tuple()]) -> [atom() | tuple()].
list_modules(Class,Key,AppModules) when is_atom(Class),is_atom(Key) ->
    % Qué modulos exportan una valor de la clase pedida?
    Modules = [ X || X <- AppModules, lists:keymember(Key,1,X:module_info(Class)) ],
    Modules.
