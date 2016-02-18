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

handle(St, {connect, User}) ->
    io:fwrite("Server received: ~p~n", [User]),
    case lists:member(User, St#server_st.connectedUsers) of
    			true -> {reply, user_already_connected, St};
    			false -> Updated_St = St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [User])},
    				{reply, ok, Updated_St}
	end;
handle(St, {disconnect, User}) ->
	io:fwrite("Server received: ~p~n", [User]),
	case existsInChannels(User, St#server_st.channelList) of
		true -> {reply, leave_channels_first, St};
		false -> Updated_St = St#server_st{connectedUsers = lists:delete(User, St#server_st.connectedUsers)},
			{reply, ok, Updated_St}
	end;
handle(St, {join, User, Channel}) ->
	io:fwrite("Server received: ~p~n", [Channel]),
	CurrentChannel = lists:keyfind(Channel,1,St#server_st.channelList),
	if  CurrentChannel == false ->
		  {reply, ok, St#server_st{channelList = lists:append(St#server_st.channelList, [{Channel, [User]}])}};
		 true->
		 	{_,Users} = CurrentChannel,
			case lists:member(User, Users) of
			true -> {reply, user_already_joined, St};
			false ->  
			{reply, ok, St#server_st{channelList = lists:keyreplace(Channel, 1, St#server_st.channelList, {Channel, lists:append(Users, User)})}}
		end                       
    end.
   
	
    
existsInChannels( Element, []) ->
	false;

existsInChannels( Element, [Item | ListTail]) ->
	{_,X} = Item,
	case lists:member(Element, X)  of
		true 	-> true;
		false	-> existsInChannels(Element, ListTail)
	end.

