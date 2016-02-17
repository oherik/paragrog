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
	ChannelList = St#server_st.channelList,
	case CurrentChannel = lists:keyfind(Channel,#channel_st.channelName,ChannelList) of
		false ->
		  {reply, ok, St#server_st{channelList = lists:append(St#server_st.channelList, #channel_st{channelName = Channel, channelUsers = [User]})}};
		 true->
			io:fwrite(CurrentChannel),
			case lists:member(User, CurrentChannel#channel_st.channelUsers) of
			true -> {reply, user_already_joined, St};
			false ->  
			Channel_update = CurrentChannel#channel_st{channelUsers = lists:append(CurrentChannel#channel_st.channelUsers, User)},
			{reply, ok, St#server_st{channelList = lists:append(Channel_update, lists:delete(CurrentChannel, St#server_st.channelList))}}
		end                       
    end.
   
	
    
existsInChannels( Element, []) ->
	false;

existsInChannels( Element, [Item | ListTail]) ->
	case lists:member(Element, Item)  of
		true 	-> true;
		false	-> existsInChannels(Element, ListTail)
	end.



index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> 
	io:fwrite(" not found "),
not_found;
index_of(Item, [#channel_st{channelName = Item}|_], Index) -> 
		io:fwrite("found "),
		Index;
index_of(Item, [_|Tail], Index) -> index_of(Item, Tail, Index+1).

