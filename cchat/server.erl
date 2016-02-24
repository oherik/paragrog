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

handle(St, {connect, UserState}) ->
    io:fwrite("Server received: ~p~n", [UserState]),
    CurrentUser = lists:member(UserState, St#server_st.connectedUsers),
    if CurrentUser == false -> Updated_St = St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [UserState])},
    				{reply, ok, Updated_St};
    			true -> {reply, user_already_connected, St}
	end;

handle(St, {disconnect, UserState}) ->
	io:fwrite("Server received: ~p~n", [UserState]),
	case existsInChannels(UserState, St#server_st.channelList) of
		true -> {reply, leave_channels_first, St};
		false -> Updated_St = St#server_st{connectedUsers = lists:delete(UserState, St#server_st.connectedUsers)},
			{reply, ok, Updated_St}
	end;

handle(St, {join, User, Channel}) ->
	ChannelPID = whereis(Channel),
	if ChannelPID == undefined ->
		io:fwrite(Channel),
			genserver:start(Channel, channel:initial_state(Channel), fun channel:handle/2),
			NewState = St#server_st{channelList = lists:append(St#server_st.channelList, Channel)};
			% Registers a new channel process if the channel name is not already registered  
		true -> already_registered,
			NewState = St
	end,

	Data = {join, User},
	Response = genserver:request(Channel, Data),
    {reply, Response, NewState};

handle(St, {leave, User, Channel}) ->
	io:fwrite("Server received: ~p~n", [Channel]),	% TODO debug
	case  lists:keyfind(Channel,1,St#server_st.channelList) of
		false ->
			{reply, channel_not_found, St};
	 	{_,Users} ->
			case lists:member(User, Users) of
				true -> {reply, ok, St#server_st{channelList = lists:keyreplace(Channel, 1, St#server_st.channelList, 
					{Channel, lists:delete(User,Users)})}};
				false -> {reply, user_not_joined, St}
	
			end                       
    end;
handle(St, {msg_from_GUI, User, Channel, Msg}) ->
	case lists:keyfind(list_to_atom(Channel),1,St#server_st.channelList) of
	 	false -> {reply, channel_not_found, St};
		{_, Users} ->
			case lists:member(User, Users) of
				true -> 
				io:fwrite("Server in send message: Message:  ~p~n", [Msg]),	% TODO debug
				io:fwrite("Server in send message: User:  ~p~n", [User]),	% TODO debug
				io:fwrite("Server in send message: Channel:  ~p~n", [Channel]),	% TODO debug
				io:fwrite("Server in send message: Users:  ~p~n", [Users]),	% TODO debug

				MessageArguments = [{ClientPID, Channel, User, Msg} || {Nick, ClientPID} <- St#server_st.connectedUsers, 
					Nick /= User, lists:member(Nick, Users)],
				list_to_atom(Channel)!{send, MessageArguments},	% Send a message to the channel
				{reply, ok, St};
			false ->	
				{reply, user_not_joined, St}	% The user hasn't joined a channel
			end
		end.
   
existsInChannels( _, []) ->
	false;

existsInChannels( Element, [Item | ListTail]) ->
	{_,X} = Item,
	case lists:member(Element, X#channel_st.connectedUsers)  of
		true 	-> true;
		false	-> existsInChannels(Element, ListTail)
	end.

sendMessage({PID, Channel, User, Message}) ->
	genserver:request(PID, {incoming_msg, Channel, User, Message}).

% Channel code
channel() ->
	spawn(fun() -> 
		channel_body() 		
			end).

channel_body() ->
	receive{send, MessageArguments} ->	
		pmap(fun sendMessage/1, MessageArguments)	% Starts a process map based on the PIDs of the clients connected to the channel
	end,
	channel_body()		% An infinite loop
	.

% Map code (basically the one that's on the course webpage, but doesn't use the get/combine functions)
	% TODO does it need to use thos functions?

-record(pmap_st, {tasks, aworkers, pworkers}).

pmap(Function, Tasks) ->	% TODO: 4 cores = 4 workers?
   W1 = worker(Function),
   W2 = worker(Function), 
   W3 = worker(Function), 
   W4 = worker(Function), 
   start(Tasks, [W1, W2, W3, W4], []).

% Worker code (basically the one that's on the course webpage, but doesn't use the get/combine functions)

worker(Function) ->
	spawn(fun() -> 
		worker_body(Function) 		% Spawns a worker, which then will loop
			end).

worker_body(Function) ->
	receive{BossPID, Argument} ->	%The worker will return its result to the boss
		Result = Function(Argument),		% TODO strunta i resultatet vid utskickning?
		BossPID!{self(), Result},
		worker_body(Function)		% An infinite loop			
	end.

start(Tasks, Workers, InitialResult) ->
 	St = #pmap_st{tasks = Tasks,
             pworkers = Workers,
             aworkers = []},
    work_load(St, InitialResult).

work_load(#pmap_st{tasks=[], aworkers=[]}, Results) ->		% All done
	Results;

work_load(St = #pmap_st{tasks = [NextTask|TasksLeft], pworkers = [PWorker|PWorkers], aworkers = AWorkers}, Results) ->		% Tasks and passive workers left
	PWorker!{self(), NextTask},
	work_load(St#pmap_st{tasks = TasksLeft, pworkers = PWorkers, aworkers = [PWorker|AWorkers]}, Results);

work_load(St = #pmap_st{pworkers = PWorkers, aworkers = AWorkers}, Results) ->
	receive{WorkerPID, Result} ->
		work_load(St#pmap_st{pworkers = [WorkerPID|PWorkers], aworkers = lists:delete(WorkerPID, AWorkers)},
			[Result|Results])
	end.	

