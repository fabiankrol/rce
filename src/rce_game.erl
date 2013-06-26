-module(rce_game).

-export([new/0, move/4]).

new() ->
    rce_game_multi:new().

move(Id, Position, SecondaryPosition, XO) ->
    case rce_game_multi:move(Id, Position, SecondaryPosition, XO) of
        {ok, undefined} -> ok;
        Other -> Other
    end.



