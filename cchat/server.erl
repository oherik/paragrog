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
case lists:member(UserState#client_st.nick, [User#client_st.nick || User <- St#server_st.connectedUsers]) of
	false ->
    	{reply, ok, St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [UserState])}};
    true ->
    	{reply, user_already_connected,St}
    end;

handle(St, {disconnect, UserState}) ->
	{reply, ok, St#server_st{connectedUsers = lists:delete(UserState, St#server_st.connectedUsers)}};

handle(St, {join, User, Channel}) ->
	ChannelAtom = list_to_atom(Channel),
	ChannelPID = whereis(ChannelAtom),
	if 	ChannelPID == undefined ->
			% Register a new channel process if the channel name is not already registered  
			genserver:start(ChannelAtom, channel:initial_state(Channel), fun channel:handle/2);		
		true -> channel_already_running
	end,
    {reply,	genserver:request(ChannelAtom, {join, User}), 
    	St#server_st{channelList = lists:append(St#server_st.channelList, [ChannelAtom])}};

handle(St, {leave, User, Channel}) ->
	case lists:member(list_to_atom(Channel),St#server_st.channelList) of
		false ->
			{reply, channel_not_found, St};
	 	true ->
			{reply, genserver:request(list_to_atom(Channel), {leave, User}), St}                   
    end.