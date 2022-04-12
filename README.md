# thePowerProject

## Running the Simulator
```erlang
> c(house).                   % compile the house module
{ok, house}
> c(breaker).                 % compile the breaker module
{ok, house}
> c(appliance).               % compile the appliance module
{ok, house}
> c(httpserver).              % compile the httpserver
{ok, httpserver}
> H = house:start(10.0).      % create a new house with 10.0 amps of available current
...
> httpserver:start(8080, H).  % start the server running on port 8080
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
