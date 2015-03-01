
GetOptEX
========

GetOptEX is an auxiliary OTP library application designed to help other apps on parsing cmd-line arguments.

Common parsing scenarios involve using the excellent Juanjo Comellas' getopt by calling it directly with 
a suitable opts list, and then dealing with getopt leftovers. 

GetOptEx uses also an extra user supplied list of funs to check on parsed command-line options and do things
like file or TCP ports checking prior to application start.

GetOptEX also adds automatic scanning and collection of opts terms lists and funs lists from any modules listed 
in the calling application manifesto (app file). The consumer does not need to provide any of these items as 
there are gatherer automagically...

Originally getoptEx evolved from a bunch of modules used in my own applications but I decided to move them into a 
separate application and also bump getopt deps to the latest version.

AUTHORS
=======

@ 2012, 2014 2015 Angel J. Alvarez <nageljalvarezmiguel@gmail.com>
	2012 Initial design (Catedromol project)
	2014 Cosmetic Fixes OTP package
	2015 Fixes and improvements


INSTALL
=======

Add GetOptEx as a dependency into your rebar.config like this:

{deps,
        [
             {getopt,   ".*", {git, "https://github.com/jcomellas/getopt.git",   {branch, "master"}}}
            ,{getoptex, ".*", {git, "https://github.com/angelitoj/getoptex.git", {branch, "master"}}}
        ]}.


Note that getOptEx depends on Juanjo Comellas's excelent getopt library. Also have in main when producing escript bundles
to include getopt and getoptex in the generated archive like this:

{escript_incl_apps,[getoptex, getopt, ... ]}.



Usage
=====

Some types:

	opt_spec       :: {Name, Short, Long, ArgSpec, Help}   %% Same format as Getopt library
	error          :: {error, ErrorData}                   %% error tuple
	opt_spec_state :: opt_spec | error                     %% Either a opt_spec tuple or an error tuple
	check_fun      :: opt_spec_state -> opt_spec_state     %% Checking Fun chaining opt_spec tuples or error tuples
	option_specs   :: () -> {[opt_spec],[check_fun]}       %% tuple of opt_specs list and opt_spec checking funs



Modules that need to get its commandline options must export an option_specs function. Let see a simple escript bundle comprising 
a gen_server key-store, 'db_srv.erl':


	-module(db_srv).
	-export([option_specs/0]).

	%% opt_specs for a simple DB gen_server reading its data from a command line provided filename
	option_specs() ->
	    {    
	        [
	             {dbformat,$f ,"format", string, "DB format <terms|csv>"}   %% Use erlang term or a CSV as file format 
	            ,{dbfile  ,$D ,"dbfile", string, "DB Data file."}           %% options spec to request especified DB file
	        ]
	        ,[
	             fun check_db_format/1                                      %% check data format is one among "terms" or "csv"
	            ,fun check_db_file/1                                        %% check file and access rights.
	            ]
	    }.

Now 'keystore.erl' the escript main file, comprising most command line options bits and OTP startup pre-flight checks

	-module(keystore).
	-export([option_specs/0])

	option_specs() ->
	    Procesos = erlang:system_info(schedulers_online) * 2,
	    {    
			%%List of getopt descriptors {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg} a.l.a GetOpt
	        [
	             {help,        $?,        "help",        undefined,                    "Show this help."}
	            ,{debug,       $d,        "debug",       {integer, 0},                 "Show debug info."}
	            ,{verbose,     $v,        "verbose",     undefined,                    "Show all actions performed."}
	            ,{version,     $V,        "version",     undefined,                    "Show software version."}
	            ,{procs,       $P,        "cores",       {integer, Procesos },         "Number of workers (default 2*core)."}
	            ,{timeout,     $T,        "timeout",     {integer, 300},               "Default app timeout in seconds."}
	        ]
	        %% args processing funs required for some options
	        ,[
	             fun check_help/1          % check for help requests at the command line.
	            ,fun check_version/1       % check for software version.
	            ,fun check_debug/1         % check for debug level among posible values.
	            ,fun check_timeout/1       % Application Timeout
	        ]
	    }.


Once you have got all your exported option descriptions and checking functions you can proceed to check command line options 
at any time in your app, i.e lets see how to check in the escript main file just before starting the main OTP tree:

	AppName = ?MODULE,        

	%% Load application and retrieve some properties
    ok                       = application:load(AppName),
    {ok,Version}             = application:get_key(AppName,vsn),
    {ok,Description}         = application:get_key(AppName,description),
    AppFilename              = escript:script_name(),


No use GetOptEx to scan app modules requiring options processing (those exporting option_specs function).

    OptionProviders          = getoptex:list_app_modules(AppName,exports,option_specs),

And collect all these option descriptors and checking funs into a single list.

    {OptSpecList,OptFunList} = getoptex:collect_option_providers(OptionProviders), 


Proceed to parse user supplied agrs, check them and start your app (or bang in front of the user :P)

    io:format("~s  Version: ~s\n\n",[AppFilename, Version]),

    %% Parse args using option descriptors and then check args values using provided funs.
    case getoptex:parse_args(OptSpecList, Args, OptFunList) of

    {ok, {AppOpts, _OtherArgs}} ->                      %% Everything went Ok.
        app_main(AppName,AppOpts);     %% Start the application, AppOpts is a property list.

    {help} ->                                           %% Help request detected..
        getopt:usage(OptSpecList, AppFilename);         %% Provide info about usage.

    {version} ->                                        %% Software version requested..
        io:format("~s (v ~s)\n~s\n", 
        	[AppFilename, Version, Description]);       %% Show the info.

    {error, {invalid_option_arg, Data}} ->              %% Some argument was wrong..
        io:format("Error:\n\t Invalid option: ~p~n~n",  
        	[Data]),                                    %% Show the offending bits.
        getopt:usage(OptSpecList, AppFilename);         %% Provide info about usage.

    {error, {Reason, Data}} ->                          %% Something else went wrong.. 
        io:format("Error:\n\t~s ~p~n~n",
        	[Reason, Data]),                            %% Show some error diagnostics
        getopt:usage(OptSpecList, AppFilename)          %% Provide info about usage.

    end.





