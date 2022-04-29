# thePowerProject

## Running the Simulator
Start a session of the Erlang shell.
```erlang
> c(admin).                   % compile the admin module once per shell
{ok, admin}
> admin:compile()             % compile all project modules
complete

> admin:start(10.0, 8080).    % create a new house with 10.0 amps of available
                              % current and start the server running on port
                              % 8080
Starting on port 8080
ok
```
Now, modifications to the `house`, will be reflected on
the webpage hosted by the `httpserver`.

You can see the webpage by navigating to `localhost` at port 8080 
(`0.0.0.0:8080`) in your browser. Each appliance, breaker, and the house are 
represented in a tree on the user interface, followed by the current power 
usage and the maximum power usage if applicable. When you hover over nodes in 
the tree, options will appear as appropriate for that node type, as well 
highlighting that node's power consumption in the graph at the right, which 
displays the history of power consumption over time.


To shutdown the web server, type:
``` erlang
> admin:stop().
ok
```

## File Contents
All files are under the main directory `thePowerProject.`

- [admin.erl](admin.erl): Erlang module providing administrative tools,
    including compiling all project files and starting and stopping the server.
- [appliance.erl](appliance.erl): Erlang module for appliance processes, which
    consume power and receive external requests to switch on and off.
- [breaker.erl](breaker.erl): Erlang module for breaker processes, which head 
    subtrees of the house, supervising child nodes and managing power 
    consumption.
- [house.erl](house.erl): Erlang module for house process managing entire 
    simulation, acting as head of supervisor tree to create and switch on/off
    appliances as requested by user or dictated by power consumption.
- [httpserver.erl](httpserver.erl): Erlang module defining interface between 
    house process and server. 
- [index.html](index.html): HTML file to structure user interface style and 
    contents for house simulation.
