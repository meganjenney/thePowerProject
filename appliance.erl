-module(appliance).
-export([start_appliance/4]).

-export([loop/1]).

% Client function to start a new appliance
start_appliance(Name, ParentPID, Power, Clock) ->
    spawn_monitor(?MODULE, loop, [{Name, ParentPID, Power, 1, Clock}]).

% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    {Name, ParentPID, Power, Status, Clock} = CurrentState,
    receive
        {exit} -> 
            io:format("Ending: ~p~n", [self()]);
        Other ->
            io:format("Received: ~s~n", [Other]),
            loop(CurrentState)
    end.