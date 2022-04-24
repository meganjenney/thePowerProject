# thePowerProject

## Running the Simulator
```erlang
> c(admin).                   % compile the admin module once per shell
{ok, admin}
> admin:compile()             % compile all project modules
complete

> admin:start(10.0, 8080).    % create a new house with 10.0 amps of available current
                              % and start the server running on port 8080
Starting on port 8080
true
...

```
Now, modifications to the `house`, will be reflected on
the webpage hosted by the `httpserver`.

You can see the webpage by navigating to `localhost` at port 8080 
(`0.0.0.0:8080`) in your browser.

To shutdown the web server, type:
``` erlang
> admin:stop().
true
>
```
