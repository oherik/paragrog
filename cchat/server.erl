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

%   TODO Bind nicket till PIDet här och skicka med


handle(St, {connect, User}) ->
    io:fwrite("Server received: ~p~n", [User]),
    case lists:member(User, St#server_st.connectedUsers) of
    			true -> {reply, user_already_connected, St};
    			false -> Updated_St = St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [User])},
    				{reply, ok, Updated_St}
	end;
handle(St, {disconnect, User, Client}) ->
	io:fwrite("Server received: ~p~n", [User]),
	case existsInChannels(User, St#server_st.channelList) of
		true -> {reply, leave_channels_first, St};
		false -> Updated_St = St#server_st{connectedUsers = lists:delete(User, St#server_st.connectedUsers)},
			{reply, ok, Updated_St}
	end;
handle(St, {join, User, Client, Channel}) ->
	io:fwrite("Server received: ~p~n", [Channel]), 
	case lists:keyfind(Channel,1,St#server_st.channelList) of
		false ->
		  {reply, ok, St#server_st{channelList = lists:append(St#server_st.channelList, [{Channel, [{User, Client}]}])}};
		 {_,Users} ->
			case lists:keymember(User,1,Users) of
				true -> {reply, user_already_joined, St};
				false ->  
					{reply, ok, St#server_st{channelList = lists:keyreplace(Channel, 1, St#server_st.channelList, {Channel, lists:append(Users, [{User, Client}])})}}
		end                       
    end;

handle(St, {send, User, Channel, Msg}) ->
	io:fwrite("Server recieved message : ~p~n",[Msg]),
	case lists:keyfind(list_to_atom(Channel),1,St#server_st.channelList) of
			false -> 
				{reply, channel_not_found, St};		
			{_,Users} ->
				case lists:keymember(User,1,Users) of
					false ->  io:fwrite("not found"),{reply, user_not_joined, St};
					true -> 
						 Usernames = [ClientPID || {Nick, ClientPID} <- Users],% Nick /= User],  TODO lägg in detta till vänster så man inte kan skicka till sig själv
						 send(Usernames, Channel, User, Msg),
			  			{reply, ok, St}

				end
	end.

send([], Channel, User, Msg) -> ok;
send([ClientPID|Tail], Channel, User, Msg) ->
	io:fwrite("Test: mottagerens PID är ~p~n", [ClientPID]), %%TODO Debug
	%% TODO lägg till if inte sändaren
	genserver:request(ClientPID, {incoming_msg, Channel, User, Msg}),
	send(Tail, Channel, User, Msg).
		
existsInChannels( Element, []) ->
	false;

existsInChannels( Element, [Item | ListTail]) ->
	{_,X} = Item,
	case lists:member(Element, X)  of
		true 	-> true;
		false	-> existsInChannels(Element, ListTail)
	end.

