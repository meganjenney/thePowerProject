# thePowerProject

## Running the Simulator
```erlang
> make:all().                   % compile all modules
Recompile: appliance
Recompile: breaker
Recompile: house
Recompile: httpserver
Recompile: make
up_to_date

> H = house:start(10.0).      % create a new house with 10.0 amps of available current
...
> S = httpserver:start(8080, H).  % start the server running on port 8080
                              % and make pass in the house PID.
Starting on port 8080
<0.88.0>
>
```
Now, modifications to the house object `H`, will be reflected on
the webpage hosted by the `httpserver`.

You can see the webpage by navigating to `localhost` at port 8080 
(`0.0.0.0:8080`) in your browser.

To shutdown the web server, type:
``` erlang
> exit(S, exit)
true
>
```
