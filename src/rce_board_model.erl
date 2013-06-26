-module(rce_board_model).

-export([
        new/1,
        move/3,
        result/1
    ]).

-record(model, {
        callback,
        moves = [],
        result,
        state
    }).

%% ===================================================================
%% api
%% ===================================================================

new(Callback) ->
    #model{
        callback = Callback,
        moves = [ {{X,Y}, Callback:init_field()} || X <- [1,2,3], Y <- [1,2,3] ],
        state = Callback:init_state()
    }.

move(Model = #model{ result = undefined, moves = Moves, state = State }, Position, Extra) ->
    Callback = Model#model.callback,
    OldValue = proplists:get_value(Position, Moves),
    case Callback:move(OldValue, Extra, State) of
        {ok, Value, NewState} ->
            NewMoves = lists:keystore(Position, 1, Moves, {Position, Value}),
            NewModel = Model#model{ moves = NewMoves, state = NewState },
            {ok, update(NewModel, Position)};
        Error ->
            Error
    end;
move(_, _, _) ->
    {error, ended}.

result(#model{ result = Result }) ->
    Result.

%% ===================================================================
%% helpers
%% ===================================================================

update(Model, {X, Y}) ->
    Lines = [
        [ {X, N} || N <- lists:seq(1,3) ],
        [ {N, Y} || N <- lists:seq(1,3) ],
        [ {N, N} || N <- lists:seq(1,3) ],
        [ {1,3}, {2,2}, {3,1} ]
    ],
    check_lines(Model, Lines).

check_lines(Model = #model{ callback = Callback, state = State }, []) ->
    case Callback:is_over(State) of
        true -> Model#model{ result = draw };
        false -> Model
    end;
check_lines(Model = #model{ callback = Callback, moves = Moves }, [ Line | Rest ]) ->
    case [ Callback:value( proplists:get_value(Pos, Moves) ) || Pos <- Line ] of
        [ XO, XO, XO ] when XO /= undefined ->
            Model#model{ result = {win, XO} };
        _ ->
            check_lines(Model, Rest)
    end.

