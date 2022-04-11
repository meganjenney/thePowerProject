%#!/usr/bin/env escript
%%! -name house
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module is the basis for a house process which manages the
%%%     entire simulation, acting as head of a supervisor tree in the
%%%     creation, turning on, and turning off of appliances either as
%%%     requested by a user or as needed according to power consumption.
%%% Integrates with user interface module to provide user visibility
%%%     and control.
%%% Able to create processes of Breaker and Appliance modules.
%%% 
%%% Last Edited 11 April 2022 by S. Bentley
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(house).
-export([start/1, get_info/1]).

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

forward_message(_Message, []) -> success;
forward_message(Message, [{Pid, _Ref} | Rest]) ->
    Pid ! Message,
    forward_message(Message, Rest).

rpc(Pid, Request) ->
    Pid ! { Request, self() },
    receive
	{Pid, Response} -> Response
    end.

get_info(Pid) -> rpc(Pid, info).

% Message receiving loop with debug code
loop(CurrentState) -> 
    erlang:display(CurrentState),
    {MaxPower, CurrentUsage, Children} = CurrentState,
    receive
	{info, From} ->
	    RequestInfo = fun({ChildPid, _Ref}) ->
				  rpc(ChildPid, info)
			  end,
	    ChildInfo = lists:map(RequestInfo, Children),
	    From ! {self(), {house, CurrentState, ChildInfo}},
	    loop(CurrentState);
        {createApp, house, Name, Power, Clock} -> 
            Pid = appliance:start_appliance(Name, Power, Clock),
            io:format("Created Pid: ~p~n", [Pid]),
            loop({MaxPower, CurrentUsage, [Pid | Children]});        
        {createApp, BreakerName, Name, Power, Clock} -> 
            io:format("Forwarding appliance creation: ~p~n", [Name]),
            forward_message({createApp, BreakerName, Name, Power, Clock}, Children),
            loop(CurrentState);
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
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.

