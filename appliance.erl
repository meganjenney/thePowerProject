-module(appliance).
-export([start_appliance/3]).

-export([loop/1]).

% Client function to start a new appliance
start_appliance(Name, Power, Clock) ->
    spawn_monitor(?MODULE, loop, [{Name, self(), Power, 1, Clock}]).

% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    {_Name, _ParentPID, _Power, _Status, _Clock} = CurrentState,
    receive
        {exit} -> 
            io:format("Ending: ~p~n", [self()]);
        Other ->
            io:format("Received: ~s~n", [Other]),
            loop(CurrentState)
    end.