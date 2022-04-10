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
-export([start/1]).

% based on https://stackoverflow.com/a/2219330/13492694
start(Port) ->
    {ok, IndexBinary} = file:read_file("./index.html"),
    io:format("Starting on port ~p\n", [Port]),
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, true}]),
		    loop(Sock, IndexBinary) end).

loop(Sock, IndexBinary) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn, IndexBinary) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock, IndexBinary).

handle(Conn, IndexBinary) ->
    gen_tcp:send(Conn, response(IndexBinary)),
    gen_tcp:close(Conn).

response(B) ->
    iolist_to_binary(
      io_lib:fwrite(
	"HTTP/1.0 200 Ok\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
	[size(B), B])).
