%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module provides administrative tools, including compiling
%%%     all project files and starting and stopping the server.
%%% Calls functions in the House and HTTPServer modules which spawn
%%%     their respective processes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(admin).
-export([compile/0, start/2, stop/0]).

%%--------------------------------------------------------------------
%% Function: compile/0
%% Description: Compiles all project Erlang files
%% Returns: ok
%%--------------------------------------------------------------------
compile() ->
    compile:file(house),
    compile:file(httpserver),
    compile:file(breaker),
    compile:file(appliance),
    ok.

%%--------------------------------------------------------------------
%% Function: start/2
%% Description: Starts the house and http server
%% Inputs: MaxPower (float) - Maximum power available to house
%%         Port (int) - Port at which to start the server
%% Returns: ok
%%--------------------------------------------------------------------
start(MaxPower, Port) ->
    H = house:start(MaxPower),
    register(house, H),
    S = httpserver:start(Port, H),
    register(houseserver, S),
    ok.

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Shuts down house and server
%% Returns: ok
%%--------------------------------------------------------------------
% Stops house and associated server
stop() ->
    HousePid = whereis(house),
    ServerPid = whereis(houseserver),
    unregister(house),
    unregister(houseserver),
    HousePid ! {exit},
    ServerPid ! {exit},
    ok.
