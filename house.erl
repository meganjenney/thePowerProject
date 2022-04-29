%!/usr/bin/env escript
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(house).
-export([start/1, get_info/1]).

-export([loop/1]).

% Spawn house process
start(MaxPower) ->
    spawn(?MODULE, loop, [{MaxPower, 0, on, [], "none"}]).

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

check_capacity({MaxPower, CurrentUsage, Status, Children, TripApp}, {AppName, AppPower}) ->
    io:format("Name of appliance now on is ~p~n", [AppName]),
    case MaxPower >= (CurrentUsage+AppPower) of
        true -> loop({MaxPower, CurrentUsage+AppPower, Status, Children, TripApp});
        false -> forward_message({turnOff, all}, Children),
                loop({MaxPower, 0, tripped, Children, AppName})
    end.

% Message receiving loop with debug code
loop(CurrentState) ->
    {MaxPower, CurrentUsage, Status, Children, TripApp} = CurrentState,
    receive
        %% info for UI
        {info, From} ->
            RequestInfo = fun({ChildPid, _Ref}) ->
                    rpc(ChildPid, info)
                end,
            ChildInfo = lists:map(RequestInfo, Children),
            From ! {self(), {house, MaxPower, CurrentUsage, Status, ChildInfo, TripApp}},
            loop(CurrentState);
        {createApp, "house", Name, Power, Clock} -> 
            Pid = appliance:start_appliance(Name, Power, Clock),
            io:format("Created Pid: ~p~n", [Pid]),
            loop({MaxPower, CurrentUsage, Status, [Pid | Children], TripApp});
        % add appliance on breaker
        {createApp, BreakerName, Name, Power, Clock} ->
            io:format("Forwarding appliance creation: ~p~n", [Name]),
            forward_message({createApp, BreakerName, Name, Power, Clock}, Children),
            loop(CurrentState);
        % remove appliance
        {removeNode, NodeName} ->
            io:format("House forwarding remove node: ~p~n", [NodeName]),
            forward_message({removeNode, NodeName}, Children),
            loop(CurrentState);
        % add breaker
        {createBreaker, Name, MaxBreakerPower} ->
            Pid = breaker:start(Name, MaxBreakerPower),
            io:format("Created Breaker: ~s (~p)~n", [Name, Pid]),
            loop({MaxPower, CurrentUsage, Status, [Pid | Children], TripApp});
        
        %% power status
        % turn on appliance
        {turnOn, AppName} ->
            forward_message({turnOn, AppName}, Children),
            loop(CurrentState);
        % turn off applicance
        {turnOff, AppName} ->
            forward_message({turnOff, AppName}, Children),
            loop(CurrentState);
        % power usage update
        {powerUpdate, on, AppInfo} ->
            check_capacity(CurrentState, AppInfo);
        {powerUpdate, off, {_AppName, AppPower}} ->
            loop({MaxPower, CurrentUsage-AppPower, Status, Children, TripApp});
        {powerUpdate, removal, on, {AppName, AppPower, AppPid}} ->
            io:format("house registers removal of appliance ~p~n", [AppName]),
            case Status == on of
                true -> loop({MaxPower, CurrentUsage-AppPower, Status, proplists:delete(AppPid, Children), TripApp});
                false -> loop({MaxPower, CurrentUsage, Status, proplists:delete(AppPid, Children), TripApp})
            end;
        {powerUpdate, removal, off, {AppName, _AppPower, AppPid}} ->
            io:format("house registers removal of appliance ~p~n", [AppName]),
            loop({MaxPower, CurrentUsage, Status, proplists:delete(AppPid, Children), TripApp});
        % breaker trip
        {trip, BreakerName, BreakerUsage, AppName} ->
            io:format("~p on breaker ~p has caused a trip~n", [AppName, BreakerName]),
            loop({MaxPower, CurrentUsage-BreakerUsage, Status, Children, TripApp});
        % trip resolution
        {tripResolve, Decision, RemoveApp} ->
            io:format("TRIP DECISION IS ~p~n", [Decision]),
            io:format("REMOVING ~p~n", [RemoveApp]),
            case Decision == "yes" of
                true -> forward_message({tripResolve, removeNode, RemoveApp}, Children);
                false -> ok
            end,
            io:format("after decision check"),
            loop({MaxPower, CurrentUsage, on, Children, "none"});

        {exit} ->
            io:format("Ending house and killing all children~n", []),
            exit_children(Children),
            exit(self(), normal);
        {'DOWN', _Ref, process, Pid, normal} ->
            io:format("Process ~p died~n", [Pid]),
            loop({MaxPower, CurrentUsage, Status, proplists:delete(Pid, Children), TripApp});
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
