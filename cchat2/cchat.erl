% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

%% Sends a job to the server
send_job(ServerString, Function, InputList) ->
	spawn( fun() ->
		genserver:request(list_to_atom(ServerString), {task, Function, InputList})
	end),
	ok.
