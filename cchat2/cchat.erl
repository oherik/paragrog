% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3, send_to_client/1]).
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
%   What it does, in order:
%   Gets the connected clients from the server. 
%   Creates a list of a combination of the function and all its inputs, and assigns them to a client.
%   Creates a list of a client, a unique reference and its task, as well as a list of just the references.
%   Starts the send_to_client method for each client
%   Collects the results
send_job(ServerString, Function, InputList) ->
    Clients = genserver:request(list_to_atom(ServerString), get_clients),
    Tasks = [{Function, Argument} || Argument <- InputList],
    TaskList = assign_tasks(Clients, Tasks),
    ClientsAndRefs = [{Client, make_ref(), Task} || {Client, Task} <- TaskList],
    Refs = [Ref || {_,Ref,_} <- ClientsAndRefs],
    lists:foreach(fun send_to_client/1, ClientsAndRefs),
    handleref([],Refs).

% Collects all resuls by waiting for replies from the send_to_client function
% Refs are usid instead of the client pids to ensure that each task is put in the list in the correct order.
handleref(Result, []) ->
    Result;
handleref(Result, [Ref|Tail]) ->
    receive {Ref, Response} ->
        handleref(Result++[Response], Tail)
    end.

%   Creates a new function which is made to send a callback using the client/task combination's unique reference 
%   number. A request is made to the client to compute said task
send_to_client({Client, Ref, Task}) ->    
    CchatPid = self(),
    spawn (fun () ->
        CchatPid!{Ref, 
         genserver:request(Client, {task, Task})}
    end).

% From the course webpage
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].

