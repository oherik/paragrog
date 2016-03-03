% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3, sendTask/1]).
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
    Tasks = [{Function, Argument} || Argument <- InputList],
    TaskList = assign_tasks(Clients, Tasks),
    
    Self = self(),
    Send_pids = [spawn(fun() -> 
        Self!{self(), genserver:request(Client, {task, Task})} 
    end) || {Client ,Task} <- TaskList],
 

   
lists:flatten([receive {Send_pid, Response} -> 
        Response 
    end|| Send_pid <- Send_pids])

    .

% From the course webpage
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].


  sendTask({Client, Task}) -> 
    genserver:request(Client, {task, Task}).

