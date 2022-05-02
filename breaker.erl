%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module serves as the framework for Breaker processes which
%%%     supervise child nodes, handles requests to create, turn on,
%%%     and turn off individual appliances, as well as requests
%%%     to remove appliances in event of a trip.
%%% Processes of this module can only be created by House processes.
%%% Able to create processes of Appliance modules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(breaker).
%% Client function to start breaker
-export([start/2]).
%% Internal appliance process functions
-export([loop/1]).

%%====================================================================
%% Client function
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/2
%% Description: Starts the breaker process
%% Inputs: Name (string) - Breaker name
%%         MaxPower (float) - Maximum power available to the breaker in Amps
%% Returns: {ListenerPid, Ref} - Identifier and reference for process monitor
%%--------------------------------------------------------------------
start(Name, MaxPower) -> 
    spawn_monitor(?MODULE, loop, [{Name, self(), MaxPower, 0, on, []}]).


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
%% Inputs: CurrentState (tuple) - Current state variables of breaker
%%         {AppName, AppPower} - New power consumption from child
%%--------------------------------------------------------------------
check_capacity({Name, ParentPid, MaxPower, CurrentUsage, Status, Children},
               {AppName, AppPower}) ->
    case Status of
        on ->
            case MaxPower >= (CurrentUsage + AppPower) of
                true  -> ParentPid ! {powerUpdate, on, {AppName, AppPower}},
                        loop({Name, ParentPid, MaxPower, 
                              CurrentUsage + AppPower, Status, Children});
                false -> forward_message({turnOff, all}, Children),
                        ParentPid ! {trip, Name, CurrentUsage, AppName},
                        loop({Name, ParentPid, MaxPower, 0, tripped, Children})
            end;
        _Other ->
            io:format("Tried to turn on appliance ~s on tripped breaker ~s~n",
                      [AppName, Name]),
            loop({Name, ParentPid, MaxPower, CurrentUsage, Status, Children})
    end.

%%====================================================================
%% Internal process function
%%====================================================================
%%--------------------------------------------------------------------
%% Function: loop/1
%% Description: Receives and handles messages for breaker process
%% Inputs: {Name, ParentPid, MaxPower, CurrentUsage, Status, Children}
%%         Name (string) - Breaker name
%%         ParentPid (pid) - Process identifier for house parent
%%         MaxPower (float) - Maximum available power for breaker in Amps
%%         CurrentUsage (float) - Current breaker power in Amps
%%         Status (atom) - Breaker state, either on, tripped, or shutdown
%%         Children (List of {ListenerPid, Ref}) - Monitors for child processes
%%--------------------------------------------------------------------
loop(CurrentState) ->
    {Name, ParentPid, MaxPower, CurrentUsage, Status, Children} = CurrentState,
    receive
        % Send node information for user interface
        {info, From} ->
            RequestInfo = fun({ChildPid, _Ref}) ->
                    rpc(ChildPid, info)
                end,
            ChildInfo = lists:map(RequestInfo, Children),
            From ! {self(), 
                   {breaker, Name, MaxPower, CurrentUsage, Status, ChildInfo}},
            loop(CurrentState);
        
        %% Structural behavior
        % Create child appliance on current breaker
        {createApp, Name, ChildName, Power, Clock} ->
            case Status of
                on ->
                    Pid = appliance:start_appliance(ChildName, Power, Clock),
                    io:format("Created Pid: ~p~n", [Pid]),
                    loop({Name, ParentPid, MaxPower, CurrentUsage, Status, 
                         [Pid | Children]});
                _Other ->
                    io:format("Can't add appliance ~p on tripped breaker ~p~n",
                              [ChildName, Name]),
                    loop(CurrentState)

            end;
        % Remove current breaker and update parent
        {removeNode, Name} ->
            io:format("Remove breaker: ~p~n", [Name]),
            exit_children(Children),
            case Status of
                on -> ParentPid ! {powerUpdate, off, {Name, CurrentUsage}};
                _Other -> none
            end;
        % Forward message to remove child node
        {removeNode, NodeName} ->
            io:format("Breaker ~p forwarding remove node: ~p~n", 
                      [Name, NodeName]),
            forward_message({removeNode, NodeName}, Children),
            loop(CurrentState);
        
        %% Power status update behavior
        % Turn off all children due to house trip
        {turnOff, all} ->
            io:format("House trip registered on breaker ~p~n", [Name]),
            forward_message({turnOff, all}, Children),
            loop({Name, ParentPid, MaxPower, 0, shutdown, Children});
        % Turn on child appliance
        {turnOn, AppName} ->
            case Status of
                on ->
                    forward_message({turnOn, AppName}, Children);
                _Other ->
                    io:format("Can't turn on appliance with breaker ~p trip~n",
                              [Name])
            end,
            loop(CurrentState);
        % Turn off child appliance
        {turnOff, AppName} ->
            forward_message({turnOff, AppName}, Children),
            loop(CurrentState);
        % Respond to child on power update
        {powerUpdate, on, AppInfo} ->
            % check status of breaker capacity
            check_capacity(CurrentState, AppInfo);
        % Respond to child turn off power update
        {powerUpdate, off, {AppName, AppPower}} ->
            ParentPid ! {powerUpdate, off, {AppName, AppPower}},
            NewPower = CurrentUsage-AppPower,
            loop({Name, ParentPid, MaxPower, NewPower, Status, Children});
        % Respond to child removal power update when child on
        {powerUpdate, removal, on, {AppName, AppPower, AppPid}} ->
            case Status of
                on -> ParentPid ! {powerUpdate, removal, on, 
                                  {AppName, AppPower, AppPid}},
                    loop({Name, ParentPid, MaxPower, CurrentUsage-AppPower, 
                          Status, proplists:delete(AppPid, Children)});
                _Other ->
                    loop({Name, ParentPid, MaxPower, CurrentUsage, Status,
                                proplists:delete(AppPid, Children)})
            end;
        % Respond to child removal power update when child off
        {powerUpdate, removal, off, {AppName, _AppPower, AppPid}} ->
            io:format("house registers removal of ~p~n", [AppName]),
            loop({Name, ParentPid, MaxPower, CurrentUsage, Status, 
                  proplists:delete(AppPid, Children)});
        % Resolve trip by removing child node
        {tripResolve, removeNode, NodeName} ->
            io:format("Breaker ~p forwarding remove node: ~p~n", 
                      [Name, NodeName]),
            forward_message({removeNode, NodeName}, Children),
            loop({Name, ParentPid, MaxPower, CurrentUsage, on, Children});
        
        % Exit all children and then self
        {exit} ->
            io:format("Ending breaker ~p and killing all children~n", [Name]),
            exit_children(Children);
        % Remove child monitor if process dies
        {'DOWN', _Ref, process, Pid, normal} ->
            io:format("Process ~p died~n", [Pid]),
            loop({Name, ParentPid, MaxPower, CurrentUsage, Status, 
                  proplists:delete(Pid, Children)});  
        % Process unknown message
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
