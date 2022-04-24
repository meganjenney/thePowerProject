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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(breaker).
-export([start/2]).

-export([loop/1]).

% Client function to start a new breaker.
start(Name, MaxPower) -> 
    spawn_monitor(?MODULE, loop, [{Name, self(), MaxPower, 0, on, []}]).

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

checkCapacity(Name, ParentPid, MaxPower, CurrentUsage, ChildPower, Status, Children) ->
    case MaxPower > (CurrentUsage+ChildPower) of
        true  -> ParentPid ! {powerUpdate, Name, ChildPower, on},
                 loop({Name, ParentPid, MaxPower, CurrentUsage+ChildPower, Status, Children});
        false -> forward_message({turnOff, all}, Children),
                 ParentPid ! {trip, Name, CurrentUsage},
                 loop({Name, ParentPid, MaxPower, 0, tripped, Children})
    end.

% Message receiving loop with debug code
loop(CurrentState) ->
    erlang:display(CurrentState),
    {Name, ParentPid, MaxPower, CurrentUsage, Status, Children} = CurrentState,
    receive
        %% info for UI
        {info, From} ->
            RequestInfo = fun({ChildPid, _Ref}) ->
                    rpc(ChildPid, info)
                end,
            ChildInfo = lists:map(RequestInfo, Children),
            From ! {self(), {breaker, Name, MaxPower, CurrentUsage, Status, ChildInfo}},
            loop(CurrentState);
        
        %% structure
        % create appliance on current breaker
        {createApp, Name, ChildName, Power, Clock} ->
            case Status == on of
                false ->
                    io:format("Cannot add appliance ~p on tripped breaker ~p~n", [ChildName, Name]),
                    loop(CurrentState);
                true ->
                    Pid = appliance:start_appliance(ChildName, Power, Clock),
                    io:format("Created Pid: ~p~n", [Pid]),
                    loop({Name, ParentPid, MaxPower, CurrentUsage, Status, [Pid | Children]})
            end;
        % trying to create appliance on another breaker
        {createApp, _OtherBreaker, ChildName, _Power, _Clock} ->
            % TODO: If multiple levels of breakers, add ability to forward
            io:format("Breaker ~p ignoring creation of ~p~n", [Name, ChildName]),
            loop(CurrentState);
        % trying to remove appliance
        {removeNode, NodeName} ->
            io:format("Breaker ~p forwarding remove node: ~p~n", [Name, NodeName]),
            forward_message({removeNode, NodeName}, Children),
            loop(CurrentState);
        
        %% power status
        % turn on appliance
        {turnOn, BreakerName, AppName} ->
            case Status == on of
                false ->
                    io:format("Attempting to turn on appliance on tripped breaker ~p~n", [Name]);
                true ->
                    forward_message({turnOn, BreakerName, AppName}, Children)
            end,
            loop(CurrentState);
        % turn off appliance
        {turnOff, BreakerName, AppName} ->
            forward_message({turnOff, BreakerName, AppName}, Children),
            loop(CurrentState);
        % appliance power usage updates
        {powerUpdate, _AppName, ChildPower, on} ->
            % check status of breaker capacity
            checkCapacity(Name, ParentPid, MaxPower, CurrentUsage, ChildPower, Status, Children);
        {powerUpdate, _AppName, ChildPower, off} ->
            ParentPid ! {powerUpdate, Name, -ChildPower},
            loop({Name, ParentPid, MaxPower, CurrentUsage-ChildPower, Status, Children});
        {turnOff, all} ->
            io:format("House trip registered on breaker ~p~n", [Name]),
            forward_message({turnOff, all}, Children),
            loop({Name, ParentPid, MaxPower, 0, shutdown, Children});
        % trip from child breaker
        {breakerTrip, _OtherBreaker} ->
            % TODO: If multiple levels of breakers, add ability to see child trip
            loop(CurrentState);

        {exit} ->
            io:format("Ending breaker ~p and killing all children~n", [Name]),
            exit_children(Children);
        {'DOWN', _Ref, process, Pid, normal} ->
            io:format("Process ~p died~n", [Pid]),
            loop({Name, ParentPid, MaxPower, CurrentUsage, Status, proplists:delete(Pid, Children)});  
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
