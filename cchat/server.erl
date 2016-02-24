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
    CurrentUser = lists:keyfind(UserState#client_st.nick, #client_st.nick, St#server_st.connectedUsers),
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
			NewState = St#server_st{channelList = lists:append(St#server_st.channelList, [Channel])};

			% Registers a new channel process if the channel name is not already registered  
		true -> already_registered,
			NewState = St
	end,

	Data = {join, User},
	Response = genserver:request(Channel, Data),
    {reply, Response, NewState};

handle(St, {leave, User, Channel}) ->
	io:fwrite("Server received: ~p~n", [Channel]),	% TODO debug
	case  lists:member(Channel,St#server_st.channelList) of
		false ->
			{reply, channel_not_found, St};
	 	true ->
	 		Data = {leave, User},
			Response = genserver:request(Channel, Data),
			{reply, Response, St}                   
    end;
handle(St, {msg_from_GUI, User, Channel, Msg}) ->
	case lists:member(list_to_atom(Channel),St#server_st.channelList) of
	 	false -> {reply, channel_not_found, St};
		true ->
			Data = {msg_from_GUI, User, Msg},
			Response = genserver:request(list_to_atom(Channel), Data),
			{reply, Response, St}
		end.
   
existsInChannels( _, []) ->
	false;

existsInChannels(User, [Channel | ListTail]) ->
	Data = {find_user, User},
	Response = genserver:request(Channel, Data),
	case Response of 
		false -> existsInChannels(User, ListTail);
		true -> true
	end.
