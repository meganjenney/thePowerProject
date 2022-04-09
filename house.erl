-module(house).
-export([start/1]).

-export([loop/1]).

% Spawn house process
start(MaxPower) -> 
    % TODO: Decide whether to spawn separate process for house
    spawn(?MODULE, loop, [{MaxPower, 0, []}]).

% Helper function to tell all children processes to exit
exit_children([]) -> success;
exit_children([{Pid, _Ref} | Rest]) ->
    Pid ! {exit},
    exit_children(Rest).

% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    {MaxPower, CurrentUsage, Children} = CurrentState,
    receive
        {createApp, Name, Power, Clock} -> 
            Pid = appliance:start_appliance(Name, Power, Clock),
            io:format("Created Pid: ~p~n", [Pid]),
            loop({MaxPower, CurrentUsage, [Pid | Children]});
        {createBreaker, Name, MaxBreakerPower} -> 
            % TODO: Add ability to create appliances at breaker
            Pid = breaker:start(Name, MaxBreakerPower),
            io:format("Created Breaker: ~p~n", [Pid]),
            loop({MaxPower, CurrentUsage, [Pid | Children]});            
        {exit} -> 
            io:format("Ending house and killing all children~n", []),
            exit_children(Children);
        {'DOWN', _Ref, process, Pid, normal} -> 
            io:format("Process ~p died~n", [Pid]);
        Other ->
            io:format("Received: ~s~n", [Other]),
            loop(CurrentState)
    end.

