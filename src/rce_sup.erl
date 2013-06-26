-module(rce_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

init(_Args) ->
    BoardVnodeMaster = { rce_board_vnode_master,
        {riak_core_vnode_master, start_link, [ rce_board_vnode ]},
        permanent, 5000, worker, [ riak_core_vnode_master ]
    },

    {ok, {{one_for_one, 5, 10}, [
                BoardVnodeMaster
            ]}}.

