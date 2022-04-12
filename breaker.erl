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
%%% Last Edited 11 April 2022 by S. Bentley
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

rpc(Pid, Request) ->
    Pid ! { Request, self() },
    receive
	{Pid, Response} -> Response
    end.

% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    {Name, ParentPid, MaxPower, CurrentUsage, Children} = CurrentState,
    receive
	{info, From} ->
	    RequestInfo = fun({ChildPid, _Ref}) ->
				  rpc(ChildPid, info)
			  end,
	    ChildInfo = lists:map(RequestInfo, Children),
	    From ! {self(), {breaker, Name, MaxPower, CurrentUsage, ChildInfo}},
	    loop(CurrentState);
        {createApp, Name, ChildName, Power, Clock} -> 
            Pid = appliance:start_appliance(ChildName, Power, Clock),
            io:format("Created Pid: ~p~n", [Pid]),
            loop({Name, ParentPid, MaxPower, CurrentUsage, [Pid | Children]});
        {createApp, OtherBreaker, ChildName, Power, Clock} ->
            % TODO: If multiple levels of breakers, add ability to forward
            io:format("Breaker ~p ignoring creation of ~p~n", [Name, ChildName]);
        {exit} -> 
            io:format("Ending breaker ~p and killing all children~n", [Name]),
            exit_children(Children);
        {'DOWN', _Ref, process, Pid, normal} -> 
            io:format("Process ~p died~n", [Pid]);
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
