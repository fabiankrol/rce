-module(rce_game_single).

-export([
        init_field/0,
        init_state/0,
        move/3,
        value/1,
        is_over/1
    ]).

%% ===================================================================
%% callbacks
%% ===================================================================

init_field() ->
    undefined.

init_state() ->
    [{count, 0}].

move(undefined, Value, State) ->
    Count = proplists:get_value(count, State),
    NewState = lists:keystore(count, 1, State, {count, Count+1}),
    {ok, Value, NewState};
move(_, _, _) ->
    {error, forbidden}.

value(Value) -> Value.

is_over(State) ->
    case proplists:get_value(count, State) of
        9 -> true;
        _ -> false
    end.

