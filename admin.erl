%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%% 
%%% This module serves as a tool to streamline the startup and
%%%     shutdown of server processes.
%%% Calls functions in the House and HTTPServer modules which spawn
%%%     their respective processes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(admin).
-export([compile/0, start/2, stop/0]).

% Compiles all project files
compile() ->
    compile:file(house),
    compile:file(httpserver),
    compile:file(breaker),
    compile:file(appliance),
    complete.

% Starts a house and associated server
start(MaxPower, Port) ->
    H = house:start(MaxPower),
    register(house, H),
    S = httpserver:start(Port, H),
    register(houseserver, S).

% Stops house and associated server
stop() ->
    HousePid = whereis(house),
    ServerPid = whereis(houseserver),
    unregister(house),
    unregister(houseserver),
    HousePid ! {exit},
    ServerPid ! {exit},
    true.
