-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{serverName = ServerName}.



%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, User, Pid}) ->
    io:fwrite("Server received: ~p~n", [User]),
    CurrentUser = lists:keyfind(User, 1, St#server_st.connectedUsers),
    if CurrentUser == false -> Updated_St = St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [{User, Pid}])},
    				{reply, ok, Updated_St};
    			true -> {reply, user_already_connected, St}
	end;
handle(St, {disconnect, User}) ->
	io:fwrite("Server received: ~p~n", [User]),
	case existsInChannels(User, St#server_st.channelList) of
		true -> {reply, leave_channels_first, St};
		false -> Updated_St = St#server_st{connectedUsers = lists:keydelete(User,1, St#server_st.connectedUsers)},
			{reply, ok, Updated_St}
	end;
handle(St, {join, User, Channel}) ->
	io:fwrite("Server received: ~p~n", [Channel]), 
	case lists:keyfind(Channel,1,St#server_st.channelList) of
		false ->
		  {reply, ok, St#server_st{channelList = lists:append(St#server_st.channelList, [{Channel, [User]}])}};
		 {_,Users} ->
			case lists:member(User, Users) of
				true -> {reply, user_already_joined, St};
				false ->  

					{reply, ok, St#server_st{channelList = lists:keyreplace(Channel, 1, St#server_st.channelList, {Channel, lists:append(Users, [User])})}}
		end                       
    end;
handle(St, {leave, User, Channel}) ->
	io:fwrite("Server received: ~p~n", [Channel]),
	case  lists:keyfind(Channel,1,St#server_st.channelList) of
		false ->
			{reply, channel_not_found, St};
	 	{_,Users} ->
			case lists:member(User, Users) of
				true -> {reply, ok, St#server_st{channelList = lists:keyreplace(Channel, 1, St#server_st.channelList, {Channel, lists:delete(User,Users)})}};
				false -> {reply, user_not_joined, St}
	
			end                       
    end;
handle(St, {msg_from_GUI, User, Channel, Msg}) ->
	case lists:keyfind(list_to_atom(Channel),1,St#server_st.channelList) of
	 	false -> {reply, channel_not_found, St};
		{_, Users} ->
			case lists:member(User, Users) of
				true -> 
			%	io:fwrite("Server in send message: Message:  ~p~n", [Msg]),
			%	io:fwrite("Server in send message: User:  ~p~n", [User]),
			%	io:fwrite("Server in send message: Channel:  ~p~n", [Channel]),
			%	io:fwrite("Server in send message: Users:  ~p~n", [Users]),

				MessageArguments = [{ClientPID, Channel, User, Msg} || {Nick, ClientPID} <- St#server_st.connectedUsers, Nick /= User, lists:member(Nick, Users)],
				pmap(fun sendMessage/1, MessageArguments),
				{reply, ok, St};
			false ->	
				{reply, user_not_joined, St}
			end
		end.
   
existsInChannels( _, []) ->
	false;

existsInChannels( Element, [Item | ListTail]) ->
	{_,X} = Item,
	case lists:member(Element, X)  of
		true 	-> true;
		false	-> existsInChannels(Element, ListTail)
	end.

sendMessage({PID, Channel, User, Message}) ->
	genserver:request(PID, {incoming_msg, Channel, User, Message}).

% Map code (basically the one that's on the course webpage)

-record(pmap_st, {tasks, aworkers, pworkers, get, combine}).

pmap(Function, Tasks) ->	% TODO: 4 cores = 4 workers?
   W1 = worker(Function),
   W2 = worker(Function), 
   W3 = worker(Function), 
   W4 = worker(Function), 
   start(Tasks, [W1, W2, W3, W4], fun get_pmap/1, fun combine_pmap/2, []).

get_pmap([Task|Tasks]) -> 	% TODO: is this method necessary? 
	{Task, Tasks}.

combine_pmap(Result,Results) -> 
	[Result|Results].	


% Worker code (basically the one that's on the course webpage)

worker(Function) ->
	spawn(fun() -> 
		worker_body(Function) 		% Spawns a worker, which then will loop
			end).

worker_body(Function) ->
	receive{BossPID, Argument} ->	%The worker will return its result to the boss
		Result = Function(Argument),
		BossPID!{self(), Result},
		worker_body(Function)		% An infinite loop
	end.

start(Tasks, Workers, Get, Combine, InitialResult) ->
 	St = #pmap_st{tasks = Tasks,
             pworkers = Workers,
             aworkers = [],
             get = Get, combine = Combine},
    work_load(St, InitialResult).

work_load(#pmap_st{tasks=[], aworkers=[]}, Results) ->		% All done
	Results;

work_load(St = #pmap_st{tasks = [NextTask|TasksLeft], pworkers = [PWorker|PWorkers], aworkers = AWorkers, get = Get}, Results) ->			% Tasks and passive workers left
	%{NextTask, TasksLeft} = Get([NextTaskI|TasksLeftI]),  
	PWorker!{self(), NextTask},
	work_load(St#pmap_st{tasks = TasksLeft, pworkers = PWorkers, aworkers = [PWorker|AWorkers]}, Results);

work_load(St = #pmap_st{pworkers = PWorkers, aworkers = AWorkers, combine = Combine}, Results) ->
	receive{WorkerPID, Result} ->
		work_load(St#pmap_st{pworkers = [WorkerPID|PWorkers], aworkers = lists:delete(WorkerPID, AWorkers)},
			Combine(Result, Results))
	end.	

