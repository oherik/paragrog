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
				io:fwrite("Server in send message: Message:  ~p~n", [Msg]),
				io:fwrite("Server in send message: User:  ~p~n", [User]),
				io:fwrite("Server in send message: Channel:  ~p~n", [Channel]),
				io:fwrite("Server in send message: Users:  ~p~n", [Users]),

				PIDs = [ClientPID || {Nick, ClientPID} <- St#server_st.connectedUsers, Nick /= User, lists:member(Nick, Users)],
				sendMessage(PIDs, Channel, User, Msg),
				{reply, ok, St};
				false -> {reply, user_not_joined, St}
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

sendMessage([PID | Tail], Channel, User, Msg) ->
	genserver:request(PID, {incoming_msg, Channel, User, Msg}),
	sendMessage(Tail, Channel, User, Msg);
	

sendMessage([],_,_,_) ->
	true.

