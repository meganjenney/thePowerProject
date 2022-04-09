-module(breaker).
-export([start/2]).

-export([loop/1]).

% Client function to start a new breaker.
start(Name, MaxPower) -> 
    spawn_monitor(?MODULE, loop, [{Name, self(), MaxPower, 0, []}]).

% Helper function to tell all children processes to exit
exit_children([]) -> success;
exit_children([{Pid, _Ref} | Rest]) ->
    Pid ! {exit},
    exit_children(Rest).

% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    {Name, ParentPid, MaxPower, CurrentUsage, Children} = CurrentState,
    receive
        {createApp, ChildName, Power, Clock} -> 
            Pid = appliance:start_appliance(ChildName, Power, Clock),
            io:format("Created Pid: ~p~n", [Pid]),
            loop({Name, ParentPid, MaxPower, CurrentUsage, [Pid | Children]});
        {exit} -> 
            io:format("Ending breaker ~p and killing all children~n", [self()]),
            exit_children(Children);
        {'DOWN', _Ref, process, Pid, normal} -> 
            io:format("Process ~p died~n", [Pid]);
        Other ->
            io:format("Received: ~p~n", [Other]),
            loop(CurrentState)
    end.
