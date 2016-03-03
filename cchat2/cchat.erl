% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3, sendTask/2]).
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
    Clients = genserver:request(list_to_atom(ServerString), get_clients),
    TaskList = assign_tasks(Clients, InputList),
    rpc:pmap({cchat, sendTask}, [Function], TaskList).

sendTask({Client, Input}, Function) -> 
    genserver:request(Client, {task, Function, Input}).

% From the course webpage
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].




%% Sends a job to the server
%send_job(ServerString, Function, InputList) ->
    %Clients = genserver:request(list_to_atom(ServerString), get_clients),
    %Tasks = [{Function, Argument} || Argument <- InputList],
    %TaskList = assign_tasks(Clients, Tasks),
    %rpc:pmap({cchat, sendTask}, [], TaskList).

%sendTask({Client, Task}) -> 
%    genserver:request(Client, {task, Task}).
