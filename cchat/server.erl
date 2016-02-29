-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used together with the genserver module,
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

%	Arguments:
%		User: the nick of the client
%	
%	Adds the user's nick to the list of connected users, if it's not already connected. If it is connected,
%	it replies with and user_already_connected message.
handle(St, {connect, User}) ->
case lists:member(User, St#server_st.connectedUsers) of
	false ->
    	{reply, ok, St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [User])}};
    true ->
    	{reply, user_already_connected, St}
    end;

%	Arguments:
%		User: the nick of the client
%	
%	Removes the user's nick from the list of connected users.
handle(St, {disconnect, User}) ->
	{reply, ok, St#server_st{connectedUsers = lists:delete(User, St#server_st.connectedUsers)}};

%	Arguments:
%		UserPid: the connecting client's pid
%		Channel: the channel the user wishes to connect to
%	
%	First, the function checks if the channel is already started. In order to mantain the property that
%	several server should be able to have channels with the same name, a new channel is started and 
%	registered in genserver using a combination of the server name and the channel name. This is 
%	possible, since the combination (server name, channel name) is unique. If this was left out, it
%	would not be possible to have ServerA with a channel #Channel as well as ServerB with a channel 
% 	#Channel. Thus, a full channel atom is created, which is the server's name followed by the channel's 
%	name.
%
%	A request is then sent to the server running and registered with this full name atom, which calls
%	for its join method.
%
%	The checks if the channel is valid to join for this particular user is made in the client process in
%	order to avoid server bottlenecks. However, this method is written here in the server process since 
%	starting channels via the channel was a specification in the lab PM. 
handle(St, {join, UserPid, Channel}) ->
	FullChannelAtom = list_to_atom(St#server_st.serverName ++ Channel),
	ChannelPID = whereis(FullChannelAtom),
	if 	ChannelPID == undefined ->
			% Register a new channel process if the channel name is not already registered  
			genserver:start(FullChannelAtom, channel:initial_state(Channel), fun channel:handle/2);		
		true -> channel_already_running
	end,
    {reply,	genserver:request(FullChannelAtom, {join, UserPid}),St}.