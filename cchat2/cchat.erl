% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3, send_task/2]).
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
%   Uses the native Erlang function rpc:pmap to accomplish this. pmap calls the function send_task/2 in this module
%   for all {Client, Input} tuples in TaskList, and also adds the function as an extra parameter. The pmap function
%   returns a list of the results from these function calls, and guarantees that they are in the same order as they
%   were in the input list.  
send_job(ServerString, Function, InputList) ->
    Clients = genserver:request(list_to_atom(ServerString), get_clients),
    TaskList = assign_tasks(Clients, InputList),
    rpc:pmap({cchat, send_task}, [Function], TaskList).

%   Creates a genserver request for the client and returns the result from the client computation.
send_task({Client, Input}, Function) -> 
    genserver:request(Client, {task, Function, Input}).

% From the course webpage
% Assigns a task to the users sent in. Creates a tuble with {User, Task} for each task sent in.
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
