%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module is the basis for Appliance processes, which consume
%%%     power at a specified rate and receivee external requests to
%%%     turn power consumption on and off.
%%% Processes of this module can be created by House and Breaker
%%%     processes.
%%% 
%%% Last Edited 10 April 2022 by M. Jenney
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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