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
%%% 
%%% Last Edited 10 April 2022 by M. Jenney
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(httpserver).
-export([start/2, info_to_json/1]).

% based on https://stackoverflow.com/a/2219330/13492694
start(Port, HousePid) ->
    {ok, IndexBinary} = file:read_file("./index.html"),
    State = {IndexBinary, HousePid},
    io:format("Starting on port ~p\n", [Port]),
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
		    loop(Sock, State) end).

loop(Sock, State) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn, State) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock, State).

handle(Conn, State) ->
    {_IndexBinary, HousePid} = State,
    {ok, Bin} = gen_tcp:recv(Conn, 0),
    {Method, Endpoint} =
	case re:run(Bin, "(?<METHOD>GET|POST) /(?<ENDPOINT>[[:alnum:]]*)", 
		    [{capture,['METHOD','ENDPOINT'],list}]) of
	    {match, [M, E]} -> {M, E};
	    _ -> io:format("no match~n")
	end,
    case {Method, Endpoint} of
	{"GET", []} ->
	    {ok, IndexBinary} = file:read_file("./index.html"),
	    gen_tcp:send(Conn, html_response(IndexBinary));
	{"GET", "info"} ->
	    JsonText = info_to_json(house:get_info(HousePid)),
	    gen_tcp:send(Conn, json_response(list_to_binary(JsonText)));
	_ ->
	    gen_tcp:send(Conn, "HTTP/1.0 404 Not Found")
    end,
    gen_tcp:close(Conn).

json_response(B) ->
    iolist_to_binary(
      io_lib:fwrite(
	"HTTP/1.0 200 Ok\nContent-Type: application/json\nContent-Length: ~p\n\n~s",
	[size(B), B])).

html_response(B) ->
    iolist_to_binary(
      io_lib:fwrite(
	"HTTP/1.0 200 Ok\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
	[size(B), B])).

concat_with_delim([],     _D) -> "";
concat_with_delim([A],    _D) -> A;
concat_with_delim([A, B],  D) -> A ++ D ++ B;
concat_with_delim([A | B], D) -> A ++ D ++ concat_with_delim(B, D).

info_to_json({house, MaxPower, CurrentUsage, ChildInfo}) ->
    OwnInfo = io_lib:format("{ \"type\": \"house\", "
			    ++ "\"max_power\": ~f, "
			    ++ "\"current_usage\": ~f, "
			    ++ "\"children\": [", 
			    [float(MaxPower), float(CurrentUsage)]),
    ChildrenJson = lists:map(fun (Child) -> info_to_json(Child) end, ChildInfo),
    OwnInfo ++ concat_with_delim(ChildrenJson, ", ") ++ "] }";
info_to_json({breaker, Name, MaxPower, CurrentUsage, ChildInfo}) ->
    OwnInfo = io_lib:format("{ \"type\": \"breaker\", "
			    ++ "\"name\": \"~s\", "
			    ++ "\"max_power\": ~f, "
			    ++ "\"current_usage\": ~f, "
			    ++ "\"children\": [", 
			    [Name, float(MaxPower), float(CurrentUsage)]),
    ChildrenJson = lists:map(fun (Child) -> info_to_json(Child) end, ChildInfo),
    OwnInfo ++ concat_with_delim(ChildrenJson, ", ") ++ "] }";
info_to_json({appliance, Name, Power}) ->
    io_lib:format("{ \"type\": \"appliance\", "
		  ++ "\"name\": \"~s\", "
		  ++ "\"power\": ~f }", 
		  [Name, float(Power)]);
info_to_json(V) -> 
    io:format("Unknown info shape: ~w", [V]),
    io_lib:format("{ \"type\": \"error\" }").
