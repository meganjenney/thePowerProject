%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module is the basis for Appliance processes, which consume
%%%     power at a specified rate and receive external requests to
%%%     turn power consumption on and off.
%%% Processes of this module can be created by House and Breaker
%%%     processes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(appliance).
-export([start_appliance/3]).

-export([loop/1, power_loop/3]).

% Client function to start a new appliance
start_appliance(Name, Power, Clock) ->
    PowerPid = spawn(?MODULE, power_loop, [Power, Clock, self()]),
    {ListenerPid, Ref} = spawn_monitor(?MODULE, loop, [{Name, self(), Power, off, PowerPid}]),
    PowerPid ! {ListenerPid},
    {ListenerPid, Ref}.

% Independent process to consume power
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

% Message receiving loop with debug code
loop(CurrentState) -> 
    {Name, ParentPID, Power, Status, PowerPid} = CurrentState,
    receive
        %% info for UI
        {info, From} ->
            From ! {self(), {appliance, Name, Power, Status}},
            loop(CurrentState);

        %% structure
        % trying to make new clone appliance
        {createApp, _Breaker, ChildName, _Power, _PowerPid} ->
            io:format("Appliance ~p ignoring creation of ~p~n", [Name, ChildName]),
	        loop(CurrentState);
        % shutdown current appliance
        {removeNode, Name} ->
            io:format("Removing appliance: ~p~n", [Name]),
            ParentPID ! {powerUpdate, removal, Status, {Name, Power, self()}};
        % trying to shutdown other appliance
        {removeNode, OtherName} ->
            io:format("Appliance ~p ignoring removal of ~p~n", [Name, OtherName]),
            loop(CurrentState);
        
        %% power status
        {power, PowerPid, NewPower} -> 
            io:format("Changing power of ~p from ~p to ~p~n", [Name, Power, NewPower]),
            case Status of
            % Trying difference
                on -> 
                    ParentPID ! {powerUpdate, on, {Name, NewPower - Power}},
                    loop({Name, ParentPID, NewPower, Status, PowerPid}); 
                off -> loop({Name, ParentPID, Power, Status, PowerPid})
            end;
        % turn on
        % breaker trip
        {turnOff, all} ->
            io:format("A parent breaker of appliance ~p has tripped~n", [Name]),
            loop({Name, ParentPID, Power, off, PowerPid});
        {turnOn, Name} ->
            io:format("Turning appliance ~p with PID ~p ON~n", [Name, self()]),
            case Status of
                on -> io:format("Appliance ~p already ON~n", [Name]);
                off -> ParentPID ! {powerUpdate, on, {Name, Power}}
            end,           
            loop({Name, ParentPID, Power, on, PowerPid});
        % turn off
        {turnOff, Name} ->
            io:format("Turning appliance ~p with PID ~p OFF~n", [Name, self()]),
            case Status of
                off -> io:format("Appliance ~p already OFF~n", [Name]);
                on -> ParentPID ! {powerUpdate, off, {Name, Power}}
            end,
            loop({Name, ParentPID, Power, off, PowerPid});
        
        {exit} -> 
            io:format("Ending appliance: ~p~n", [Name]),
            PowerPid ! {exit},
            case Status of
                on -> ParentPID ! {powerUpdate, off, {Name, Power}};
                off -> none
            end;
        Other ->
            io:format("Received: ~w~n", [Other]),
            loop(CurrentState)
    end.
