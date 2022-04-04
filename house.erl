-module(house).
-export([start/1]).

-export([loop/1]).


start(MaxPower) -> 
    % TODO: Decide whether to spawn separate process for house
    spawn(?MODULE, loop, [{MaxPower, 0, []}]).


% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    receive
        {exit} -> 
            erlang:display("Ending");
        {Other} ->
            io:format("Received: ~s~n", [Other]),
            loop(CurrentState)
    end.

