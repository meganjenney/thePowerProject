
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
    house ! {exit},
    exit(houseserver),
    true.