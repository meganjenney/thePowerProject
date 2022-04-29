%!/usr/bin/env escript
%%! -name httpserver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Concurrent Programming (CS21)
%%% Tufts University Spring 2022
%%% S. Bentley, S. Cohen, M. Jenney
%%%
%%% This module defines ways for a server to handle different types
%%%     of messages from a user and send them to an associated House
%%%     process.
%%% Stores a copy of the current state of the house power consumption
%%%     and existing breakers and appliances.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(httpserver).

%% Client functions for server
-export([start/2, info_to_json/1]).

%%====================================================================
%% Client functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/2
%% Description: Starts the http server
%% Inputs: Port (int) - Port on which to start server
%%		   HousePid (pid) - Process identifier for associated house
%% Returns: Pid - Identifier for http process
%% Note: based on https://stackoverflow.com/a/2219330/13492694
%%--------------------------------------------------------------------
start(Port, HousePid) ->
    {ok, IndexBinary} = file:read_file("./index.html"),
    State = {IndexBinary, HousePid},
    io:format("Starting on port ~p\n", [Port]),
    spawn_link(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false},
							    {reuseaddr, true}]),
			 loop(Sock, State) end).

%%--------------------------------------------------------------------
%% Function: info_to_json
%% Description: converts the state of a house, breaker, or appliance
%%              into a JSON string
%% Inputs: the state of a simulation object as produced by the info message
%% Returns: a JSON string representing the object passed into the function
%%--------------------------------------------------------------------
info_to_json({house, MaxPower, CurrentUsage, Status, ChildInfo, TripApp}) ->
    OwnInfo = io_lib:format("{ \"type\": \"house\", "
		++ "\"max_power\": ~f, "
		++ "\"current_usage\": ~f, "
		++ "\"status\": \"~s\", "
		++ "\"trip_app\": \"~s\", "
		++ "\"children\": [",
		[float(MaxPower), float(CurrentUsage), atom_to_list(Status), TripApp]),
    ChildrenJson = lists:map(fun (Child) -> info_to_json(Child) end,ChildInfo),
    OwnInfo ++ concat_with_delim(ChildrenJson, ", ") ++ "] }";
info_to_json({breaker, Name, MaxPower, CurrentUsage, Status, ChildInfo}) ->
    OwnInfo = io_lib:format("{ \"type\": \"breaker\", "
			    ++ "\"name\": \"~s\", "
			    ++ "\"max_power\": ~f, "
			    ++ "\"current_usage\": ~f, "
			    ++ "\"status\": \"~s\", "
			    ++ "\"children\": [",
			    [Name, float(MaxPower), float(CurrentUsage),
			     atom_to_list(Status)]),
    ChildrenJson = lists:map(fun (Child) -> info_to_json(Child) end,ChildInfo),
    OwnInfo ++ concat_with_delim(ChildrenJson, ", ") ++ "] }";
info_to_json({appliance, Name, Power, Status}) ->
    io_lib:format("{ \"type\": \"appliance\", "
		  ++ "\"name\": \"~s\", "
		  ++ "\"current_usage\": ~f, "
		  ++ "\"status\": \"~s\" }",
		  [Name, float(Power), atom_to_list(Status)]);
info_to_json({tripInfo, TripApp}) ->
	io_lib:format("{ \"trip_appliance\": \"~s\"}", [TripApp]);
info_to_json(V) ->
    io:format("Unknown info shape: ~w", [V]),
    io_lib:format("{ \"type\": \"error\" }").


%%====================================================================
%% Internal helper functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: loop/2
%% Description: Message receiving loop for the http server interface
%% Inputs: Sock (Socket) - Web socket for server
%%		   State (Tuple) - Representation of UI's HTML source and the process
%%                                 identifier for the house node
%%--------------------------------------------------------------------
loop(Sock, State) ->
    receive
	{exit} ->
	    gen_tcp:close(Sock),
	    exit(self(), normal);
	_ -> ok
    after 0 -> ok
    end,
    {Atom, ConnReason} = gen_tcp:accept(Sock, 1000),
    case Atom of
	ok -> Handler = spawn(fun () -> handle(ConnReason, State) end),
	      gen_tcp:controlling_process(ConnReason, Handler);
	error -> ok
    end,
    loop(Sock, State).

%%--------------------------------------------------------------------
%% Function: parse_query_string/1
%% Description: Parse query string into Key Value tuples
%% Inputs: Str (string) - String to parse
%% Returns: List of {Key, Value} tuples
%%--------------------------------------------------------------------
parse_query_string("") -> [];
parse_query_string(Str) ->
    lists:map(fun (Pair) ->
		      [ Key, Value ] = string:split(Pair, "="),
		      { Key, Value } end,
	      string:split(Str, "&", all)).

%%--------------------------------------------------------------------
%% Function: handle/2
%% Description: Handle a user's HTTP request
%% Inputs: Conn (Socket) -- representing the connected client's socket
%%                          connection
%%         State (Tuple) -- Representation of UI's HTML source and process
%%                          identifier for the house node
%% Returns: does not return
%%--------------------------------------------------------------------
handle(Conn, State) ->
    {_IndexBinary, HousePid} = State,
    {ok, Bin} = gen_tcp:recv(Conn, 0),
    [ FirstLine | _ ] = string:split(Bin, "\r\n"),
    [ Method, URL, _ ] = string:split(FirstLine, " ", all),
    [ Endpoint | QueryString ] = string:split(URL, "?"),
    KeyValuePairs = parse_query_string(QueryString),

    case { Method, Endpoint } of
		{"GET", "/"} ->
			{ok, IndexBinary} = file:read_file("./index.html"),
			gen_tcp:send(Conn, html_response(IndexBinary));
		{"GET", "/info"} ->
				InfoText = info_to_json(house:get_info(HousePid)),
				gen_tcp:send(Conn, json_response(list_to_binary(InfoText)));

		{"POST", "/new_appliance"} ->
			Parent = proplists:get_value("parent", KeyValuePairs),
			Name = proplists:get_value("name", KeyValuePairs),
			{Power,[]} = string:to_float(
							proplists:get_value("power", KeyValuePairs)),
			{Clock,[]} = string:to_float(
							proplists:get_value("clock", KeyValuePairs)),
			HousePid ! {createApp, Parent, Name, Power, Clock},
			gen_tcp:send(Conn, "HTTP/1.0 200 OK");
		{"POST", "/new_breaker"} ->
			Name = proplists:get_value("name", KeyValuePairs),
			{Power,[]} = string:to_float(
							proplists:get_value("power", KeyValuePairs)),
			HousePid ! {createBreaker, Name, Power};
		{"POST", "/delete"} ->
			Name = proplists:get_value("name", KeyValuePairs),
			HousePid ! {removeNode, Name};
		{"POST", "/turn_on"} ->
			Name = proplists:get_value("name", KeyValuePairs),
			HousePid ! {turnOn, Name};
		{"POST", "/turn_off"} ->
			Name = proplists:get_value("name", KeyValuePairs),
			HousePid ! {turnOff, Name};
		{"POST", "/resolve"} ->
			Decision = proplists:get_value("handle", KeyValuePairs),
			RemoveApp = proplists:get_value("app", KeyValuePairs),
			io:format("KVP: ~p~n", [KeyValuePairs]),
			HousePid ! {tripResolve, Decision, RemoveApp};
		_ ->
			io:format("unknown endpoint: Method ->~s<- Ept->~s<-~n",
				[Method,Endpoint]),
			lists:foreach(fun (Elem) ->
					{Key, Value} = Elem,
					io:format("query parameter: ~s <- ~s~n", [Key, Value])
				end, KeyValuePairs),
			gen_tcp:send(Conn, "HTTP/1.0 404 Not Found")
    end,
    gen_tcp:close(Conn).

%%--------------------------------------------------------------------
%% Function: json_response/1
%% Description: Creates a JSON response from the input binary string
%% Inputs: B (Binary String) - the content of the HTTP message
%% Returns: A binary string conforming to the HTTP protocol
%%--------------------------------------------------------------------
json_response(B) ->
    iolist_to_binary(
      io_lib:fwrite(
	"HTTP/1.0 200 Ok\nContent-Type:application/json\nContent-Length: ~p\n\n~s",
	[size(B), B])).

%%--------------------------------------------------------------------
%% Function: html_response/1
%% Description: Creates a HTML response from the input binary string
%% Inputs: B (Binary String) - the content of the HTTP message
%% Returns: A binary string conforming to the HTTP protocol
%%--------------------------------------------------------------------
html_response(B) ->
    iolist_to_binary(
      io_lib:fwrite(
	"HTTP/1.0 200 Ok\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
	[size(B), B])).

%%--------------------------------------------------------------------
%% Function: concat_with_delim/1
%% Description: Concat a list of strings placing a delimeter between
%%              each element, but not after the last element
%%              Example:
%%              concat_with_delim(["a", "b", "c"], ", ")
%%                 returns "a, b, c"
%% Inputs: List of strings
%% Returns: String
%%--------------------------------------------------------------------
concat_with_delim([],     _D) -> "";
concat_with_delim([A],    _D) -> A;
concat_with_delim([A, B],  D) -> A ++ D ++ B;
concat_with_delim([A | B], D) -> A ++ D ++ concat_with_delim(B, D).


