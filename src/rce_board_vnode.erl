-module(rce_board_vnode).
-behaviour(riak_core_vnode).

-export([
        new/2,
        move/3,
        result/1
    ]).

-export([ start/0, start_vnode/1 ]).
-export([
        delete/1,
        encode_handoff_item/2,
        handle_command/3,
        handle_coverage/4,
        handle_exit/3,
        handle_handoff_command/3,
        handle_handoff_data/2,
        handoff_cancelled/1,
        handoff_finished/2,
        handoff_starting/2,
        init/1,
        is_empty/1,
        terminate/2
    ]).


%% ===================================================================
%% api
%% ===================================================================

new(BoardId, Callback) ->
    command(BoardId, {new, BoardId, Callback}).

move(BoardId, Position, Value) ->
    command(BoardId, {move, BoardId, Position, Value}).

result(BoardId) ->
    command(BoardId, {result, BoardId}).

command(Key, Command) ->
    Hash = riak_core_util:chash_key({?MODULE, Key}),
    [{IndexNode, _}] = riak_core_apl:get_primary_apl(Hash, 1, ?MODULE),
    riak_core_vnode_master:sync_command(IndexNode, Command, rce_board_vnode_master).

%% ===================================================================
%%
%% ===================================================================

start() ->
    ok = riak_core:register([{vnode_module, ?MODULE}]),
    ok = riak_core_node_watcher:service_up(?MODULE, self()).

start_vnode(Partition) ->
    riak_core_vnode_master:get_vnode_pid(Partition, ?MODULE).

%% ===================================================================
%% riak_core_vnode behaviour callbacks
%% ===================================================================

%% -------------------------------------------------------------------
%% vnode process management
%% -------------------------------------------------------------------

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% command handling
%% -------------------------------------------------------------------

handle_command({new, BoardId, Extra}, _, State) ->
    Board = rce_board_model:new(Extra),
    NewState = lists:keystore(BoardId, 1, State, {BoardId, Board}),
    {reply, ok, NewState};

handle_command({move, BoardId, Position, Player}, _, State) ->
    case lists:keyfind(BoardId, 1, State) of
        {_, Board} ->
            case rce_board_model:move(Board, Position, Player) of
                {ok, NewBoard} ->
                    NewState = lists:keyreplace(BoardId, 1, State, {BoardId, NewBoard}),
                    Result = rce_board_model:result(NewBoard),
                    {reply, {ok, Result}, NewState};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_command({result, BoardId}, _, State) ->
    case lists:keyfind(BoardId, 1, State) of
        {_, Board} ->
            Result = rce_board_model:result(Board),
            {reply, {ok, Result}, State};
        _ ->
            {reply, {error, not_found}, State}
    end.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

%% -------------------------------------------------------------------
%% handoff
%% -------------------------------------------------------------------

delete(State) ->
    {ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

is_empty(State) ->
    {true, State}.

