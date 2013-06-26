-module(rce_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start_dev/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = rce_sup:start_link(),
    ok = rce_board_vnode:start(),
    {ok, Pid}.

stop(_State) ->
    ok.

%% ===================================================================
%% Start all deps
%% ===================================================================

start_dev() ->
    [ ok = application:start(App) || App <- [
            sasl,
            syntax_tools,
            compiler,
            crypto,
            lager,
            inets,
            mochiweb,
            webmachine,
            riak_sysmon,
            os_mon,
            riak_core,
            rce
        ]],
    ok.
