%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module serves as the framework for Breaker processes which
%%%     supervise child appliances, manage power so its collection of
%%%     appliances doesn't exceed a breaker's limit, and handle
%%%     requests to create, turn on, and turn off individual appliances.
%%% Processes of this module can only be created by House processes.
%%% Able to create processes of Appliance modles.
%%% 
%%% Last Edited 10 April 2022 by M. Jenney
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
