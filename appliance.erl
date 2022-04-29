%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module is the basis for Appliance processes, which consume
%%%     power at a specified rate with noise and receive external requests to
%%%     turn power consumption on and off.
%%% Processes of this module can be created by House and Breaker
%%%     processes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(appliance).

%% Client function to start appliance
-export([start_appliance/3]).

%% Internal appliance process functions
-export([loop/1, power_loop/3]).

%%====================================================================
%% Client function
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_appliance/3
%% Description: Starts the appliance listener and power consumption processes
%% Inputs: Name (string) - Appliance name
%%         Power (float) - Mean appliance power consumption in Amps
%%         Clock (float) - Maximum power consumption interval in decaseconds
%% Returns: {ListenerPid, Ref} - Identifier and reference for process monitor
%%--------------------------------------------------------------------
start_appliance(Name, Power, Clock) ->
    PowerPid = spawn(?MODULE, power_loop, [Power, Clock, self()]),
    {ListenerPid, Ref} = spawn_monitor(?MODULE, loop, 
                                       [{Name, self(), Power, off, PowerPid}]),
    PowerPid ! {ListenerPid},
    {ListenerPid, Ref}.


%%====================================================================
%% Internal process functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: power_loop/3
%% Description: Determines power consumption centered on given mean power
%% Inputs: Power (float) - Mean appliance power consumption in Amps
%%         Clock (float) - Maximum power consumption interval in decaseconds
%%         ListenerPid (pid) - Process identifier for appliance to send power 
%%                             consumption to
%%--------------------------------------------------------------------
power_loop(Power, Clock, ListenerPid) ->
    receive 
        {exit} -> ok;
        {Pid} when is_pid(Pid) -> power_loop(Power, Clock, Pid)
    after round(Clock * 1000 * rand:uniform(10)) ->
        NewPower = rand:normal(Power, Power / 5),
        case NewPower =< 0 of
            true -> ListenerPid ! {power, self(), 0};
            false -> ListenerPid ! {power, self(), NewPower}
        end,
        power_loop(Power, Clock, ListenerPid)
    end.

%%--------------------------------------------------------------------
%% Function: loop/1
%% Description: Receives and handles messages for appliance behavior
%% Inputs: {Name, ParentPid, Power, Status, PowerPid} = CurrentState
%%         Name (string) - Appliance name
%%         ParentPid (pid) - Process identifier for breaker or house parent
%%         Power (float) - Current appliance power consumption in Amps
%%         Status (atom) - Appliance state, either on or off
%%         PowerPid (pid) - Process identifier for power consuming process
%%--------------------------------------------------------------------
loop(CurrentState) -> 
    {Name, ParentPid, Power, Status, PowerPid} = CurrentState,
    receive
        % Send node information for user interface
        {info, From} ->
            From ! {self(), {appliance, Name, Power, Status}},
            loop(CurrentState);

        %% Structural behavior
        % Ignore creation message not directed to node 
        {createApp, _Breaker, ChildName, _Power, _PowerPid} ->
            io:format("Appliance ~p ignoring creation of ~p~n", 
                      [Name, ChildName]),
	        loop(CurrentState);
        % Notify parent of removal and stop appliance process
        {removeNode, Name} ->
            io:format("Removing appliance: ~p~n", [Name]),
            ParentPid ! {powerUpdate, removal, Status, {Name, Power, self()}};
        % Ignore removal message not directed to node
        {removeNode, OtherName} ->
            io:format("Appliance ~p ignoring removal of ~p~n", 
                      [Name, OtherName]),
            loop(CurrentState);
        
        %% Power status update behavior
        % Receive new power consumption from consumer
        {power, PowerPid, NewPower} -> 
            io:format("Changing power of ~p from ~p to ~p~n", 
                      [Name, Power, NewPower]),
            case Status of
            % Determines change in power
                on -> 
                    ParentPid ! {powerUpdate, on, {Name, NewPower - Power}},
                    loop({Name, ParentPid, NewPower, Status, PowerPid}); 
                off -> loop({Name, ParentPid, Power, Status, PowerPid})
            end;
        % Turn off appliance due to breaker trip
        {turnOff, all} ->
            io:format("A parent breaker of appliance ~p has tripped~n", [Name]),
            loop({Name, ParentPid, Power, off, PowerPid});
        % Turn on appliance 
        {turnOn, Name} ->
            io:format("Turning appliance ~p with PID ~p ON~n", [Name, self()]),
            case Status of
                on -> io:format("Appliance ~p already ON~n", [Name]);
                off -> ParentPid ! {powerUpdate, on, {Name, Power}}
            end,           
            loop({Name, ParentPid, Power, on, PowerPid});
        % Turn off appliance
        {turnOff, Name} ->
            io:format("Turning appliance ~p with PID ~p OFF~n", [Name, self()]),
            case Status of
                off -> io:format("Appliance ~p already OFF~n", [Name]);
                on -> ParentPid ! {powerUpdate, off, {Name, Power}}
            end,
            loop({Name, ParentPid, Power, off, PowerPid});
        % Remove appliance as a result of parent trip
        {tripResolve, removeNode, Name} ->
            io:format("Removing appliance as resolution to trip: ~p~n", [Name]),
            ParentPid ! {powerUpdate, removal, Status, {Name, Power, self()}};
        
        % Update parent and exit appliance and power consumer
        {exit} -> 
            io:format("Ending appliance: ~p~n", [Name]),
            PowerPid ! {exit},
            case Status of
                on -> ParentPid ! {powerUpdate, off, {Name, Power}};
                off -> none
            end;
        % Process unknown message
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
