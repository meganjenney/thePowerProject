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
%%% Last Edited 11 April 2022 by M. Jenney
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
    {Name, _ParentPID, _Power, _Status, _Clock} = CurrentState,
    receive
	{info, From} ->
        % info for UI
	    From ! {self(), {appliance, CurrentState}},
	    loop(CurrentState);

        %% structure
        % trying to make new clone appliance
        {createApp, _Breaker, ChildName, _Power, _Clock} ->
            io:format("Appliance ~p ignoring creation of ~p~n", [Name, ChildName]),
	    loop(CurrentState);

        %% power status
        % turn on
        {turnOn, _BreakerName, Name} ->
            io:format("Turning appliance ~p with PID ~p ON~n", [Name, self()]),
            ParentPID ! {powerUpdate, Name, Power, on},
            loop({Name, ParentPID, Power, on, Clock});
        % turn off
        {turnOff, _BreakerName, Name} ->
            io:format("Turning appliance ~p with PID ~p OFF~n", [Name, self()]),
            ParentPID ! {powerUpdate, Name, Power, off},
            loop({Name, ParentPID, Power, off, Clock});

        {exit} ->
            io:format("Ending appliance: ~p~n", [Name]);
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
