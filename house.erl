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
%% Client functions for house
-export([start/1, get_info/1]).
%% Internal appliance process function
-export([loop/1]).

%%====================================================================
%% Client functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the house process
%% Inputs: MaxPower (float) - Maximum power available to the house in Amps
%% Returns: Pid - Identifier for house process
%%--------------------------------------------------------------------
start(MaxPower) ->
    spawn(?MODULE, loop, [{MaxPower, 0, on, [], "none"}]).

%%--------------------------------------------------------------------
%% Function: get_info/1
%% Description: Gets house and subtree information
%% Inputs: Pid (pid) - Process identifier for house
%% Returns: Info (tuple) - Nested tuple of house and subprocess state info
%%--------------------------------------------------------------------
get_info(Pid) -> rpc(Pid, info).


%%====================================================================
%% Internal helper functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: exit_children/1
%% Description: Sends exit messages to all children processes
%% Inputs: List of {ListenerPid, Ref} for children processes
%% Returns: success
%%--------------------------------------------------------------------
exit_children([]) -> success;
exit_children([{Pid, _Ref} | Rest]) ->
    Pid ! {exit},
    exit_children(Rest).

%%--------------------------------------------------------------------
%% Function: forward_message/2
%% Description: Sends messages to all children processes without response
%% Inputs: Message to forward to children
%%         List of {ListenerPid, Ref} for children processes
%% Returns: success
%%--------------------------------------------------------------------
forward_message(_Message, []) -> success;
forward_message(Message, [{Pid, _Ref} | Rest]) ->
    Pid ! Message,
    forward_message(Message, Rest).

%%--------------------------------------------------------------------
%% Function: rpc/2
%% Description: Sends messages to process and waits for response
%% Inputs: Pid of process to send message to
%%         Request to forward to process
%% Returns: success
%%--------------------------------------------------------------------
rpc(Pid, Request) ->
    Pid ! { Request, self() },
    receive
	    {Pid, Response} -> Response
    end.

%%--------------------------------------------------------------------
%% Function: check_capacity/2
%% Description: Checks whether child power update is within available power 
%%              and updates parent
%% Inputs: CurrentState (tuple) - Current state variables of house
%%         {AppName, AppPower} - New power consumption from child
%%--------------------------------------------------------------------
check_capacity({MaxPower, CurrentUsage, Status, Children, TripApp},
                {AppName, AppPower}) ->
    case MaxPower >= (CurrentUsage+AppPower) of
        true ->
            loop({MaxPower, CurrentUsage+AppPower, Status, Children, TripApp});
        false -> forward_message({turnOff, all}, Children),
                loop({MaxPower, 0, tripped, Children, AppName})
    end.

%%====================================================================
%% Internal process function
%%====================================================================
%%--------------------------------------------------------------------
%% Function: loop/1
%% Description: Receives and handles messages for house process
%% Inputs: {MaxPower, CurrentUsage, Status, Children, TripApp}
%%         MaxPower (float) - Maximum available power for house in Amps
%%         CurrentUsage (float) - Current house power in Amps
%%         Status (atom) - House state, either on or tripped
%%         Children (List of {ListenerPid, Ref}) - Monitors for child processes
%%         TripApp (string) - name of appliance that caused a trip
%%--------------------------------------------------------------------
loop(CurrentState) ->
    {MaxPower, CurrentUsage, Status, Children, TripApp} = CurrentState,
    receive
        % Send node information for user interface
        {info, From} ->
            RequestInfo = fun({ChildPid, _Ref}) ->
                    rpc(ChildPid, info)
                end,
            ChildInfo = lists:map(RequestInfo, Children),
            NewS = {house, MaxPower, CurrentUsage, Status, ChildInfo, TripApp},
            From ! {self(), NewS},
            loop(CurrentState);

        %% Structural behavior
        % Create direct child appliance
        {createApp, "house", Name, Power, Clock} -> 
            Pid = appliance:start_appliance(Name, Power, Clock),
            io:format("Created Pid: ~p~n", [Pid]),
            loop({MaxPower, CurrentUsage, Status, [Pid | Children], TripApp});
        % Create appliance on breaker child
        {createApp, BreakerName, Name, Power, Clock} ->
            io:format("Forwarding appliance creation: ~p~n", [Name]),
            forward_message({createApp, BreakerName, Name, Power, Clock},
                             Children),
            loop(CurrentState);
        % Forward message to remove child node
        {removeNode, NodeName} ->
            io:format("House forwarding remove node: ~p~n", [NodeName]),
            forward_message({removeNode, NodeName}, Children),
            loop(CurrentState);
        % Create breaker child
        {createBreaker, Name, MaxBreakerPower} ->
            Pid = breaker:start(Name, MaxBreakerPower),
            io:format("Created Breaker: ~s (~p)~n", [Name, Pid]),
            loop({MaxPower, CurrentUsage, Status, [Pid | Children], TripApp});
        
        %% Power status update behavior
        % Turn on appliance
        {turnOn, AppName} ->
            forward_message({turnOn, AppName}, Children),
            loop(CurrentState);
        % Turn off appliance
        {turnOff, AppName} ->
            forward_message({turnOff, AppName}, Children),
            loop(CurrentState);
        % Respond to child on power update
        {powerUpdate, on, AppInfo} ->
            check_capacity(CurrentState, AppInfo);
        % Respond to child off power update
        {powerUpdate, off, {_AppName, AppPower}} ->
            loop({MaxPower, CurrentUsage-AppPower, Status, Children, TripApp});
        % Respond to child removal power update when child on
        {powerUpdate, removal, on, {AppName, AppPower, AppPid}} ->
            io:format("house registers removal of appliance ~p~n", [AppName]),
            case Status of
                on -> NewUsage = CurrentUsage - AppPower,
                    loop({MaxPower, NewUsage, Status,
                          proplists:delete(AppPid, Children), TripApp});
                _Other -> 
                    loop({MaxPower, CurrentUsage, Status,
                          proplists:delete(AppPid, Children),TripApp})
            end;
        % Respond to child removal power update when child off
        {powerUpdate, removal, off, {AppName, _AppPower, AppPid}} ->
            io:format("house registers removal of appliance ~p~n", [AppName]),
            loop({MaxPower, CurrentUsage, Status,
                  proplists:delete(AppPid, Children), TripApp});
        % Respond to breaker trip
        {trip, BreakerName, BreakerUsage, AppName} ->
            io:format("~p on breaker ~p has caused a trip~n",
                [AppName, BreakerName]),
            loop({MaxPower,CurrentUsage-BreakerUsage,Status,Children,TripApp});
        % Respond to breaker trip resolution
        {tripResolve, Decision, TripApp} ->
            case Decision of
                "yes" ->
                    forward_message({tripResolve,removeNode,TripApp},Children);
                _Other -> ok
            end,
            io:format("after decision check"),
            loop({MaxPower, CurrentUsage, on, Children, "none"});

        % Exit all children and then self
        {exit} ->
            io:format("Ending house and killing all children~n", []),
            exit_children(Children),
            exit(self(), normal);
        % Remove child monitor if process dies
        {'DOWN', _Ref, process, Pid, normal} ->
            io:format("Process ~p died~n", [Pid]),
            loop({MaxPower, CurrentUsage, Status,
                proplists:delete(Pid, Children), TripApp});
        % Process unknown message
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
