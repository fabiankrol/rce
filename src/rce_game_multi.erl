-module(rce_game_multi).

-export([
        new/0,
        move/4
    ]).

-export([
        init_field/0,
        init_state/0,
        move/3,
        value/1,
        is_over/1
    ]).

%% ===================================================================
%% api
%% ===================================================================

new() ->
    new(?MODULE).

new(Callback) ->
    Id = make_ref(),
    ok = rce_board_vnode:new(Id, Callback),
    Id.

move(Id, PosMulti, PosSingle, Value) ->
    rce_board_vnode:move(Id, PosMulti, {PosSingle, Value}).

%% ===================================================================
%% callbacks
%% ===================================================================

init_field() ->
    new(rce_game_single).

init_state() ->
    [{count, 0}, {last_turn, undefined}].

move(Id, {Pos, Value}, State) ->
    case proplists:get_value(last_turn, State) of
        Value -> {error, forbidden};
        _ -> move_(Id, {Pos, Value}, State)
    end.

move_(Id, {Pos, Value}, State) ->
    case rce_board_vnode:move(Id, Pos, Value) of
        {ok, _} ->
            Count = proplists:get_value(count, State),
            State1 = lists:keystore(count, 1, State, {count, Count+1}),
            State2 = lists:keystore(last_turn, 1, State1, {last_turn, Value}),
            {ok, Id, State2};
        _ ->
            {error, forbidden}
    end.

value(Id) ->
    case rce_board_vnode:result(Id) of
        {ok, {win, Value}} -> Value;
        {ok, Value} -> Value;
        Error -> Error
    end.

is_over(State) ->
    case proplists:get_value(count, State) of
        9 -> true;
        _ -> false
    end.

