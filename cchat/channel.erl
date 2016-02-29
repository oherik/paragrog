-module(channel).
-export([handle/2, initial_state/1, spawnMessage/4]).
-include_lib("./defs.hrl").

%%	initial_state/1

%	Produces the initial state
initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

%%	handle/2

% 	Arguments:
%		St: the current state for the channel
%
%	Handles the various requests from the server and clients
%
% 	Returns: a tuple consisting of 'reply', 'ok' and the updated channel state

% 	Adds the user's pid to the list of connected users. The check if the user is already connected is 
%	assumed to be handled in the client.
handle(St, {join, UserPid}) ->
	{reply, ok, St#channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, [UserPid])}};

% 	Removes the user's pid from the list of connected users. The check if the user is connected is 
%	assumed to be handled in the client.
handle(St, {leave, UserPid}) ->
	{reply, ok, St#channel_st{connectedUsers = lists:delete(UserPid, St#channel_st.connectedUsers)}};

% 	Retrieves all the connected users' pids where the pid is not the same as the sender's. This is done 
% 	to make sure that the sender doesn't send its message to itself. When that is done the list is used
%	to create a pmap, which in turn starts the spawnMessage function for all the contained pids. As 
%	additional arguments it sends the channel name, the sender's nick and the message itself
handle(St, {msg_from_GUI, UserNick, UserPid, Message}) ->
	ConnectedPids = lists:delete(UserPid, St#channel_st.connectedUsers),
	rpc:pmap({channel, spawnMessage}, [St#channel_st.channelName,UserNick, Message], ConnectedPids),
	{reply, ok, St}.


%%	spawnMessage/4

%	Used in combination with the other spawnMessage method when the latter is to be used in
%	conjunction with a pmap. This is needed for the case where there are no clients left to
%	sen the message to, as indicated by the empty list argument.
spawnMessage([],_,_,_) -> no_client;

%	Creates a new process which spawns a genserver request in order to send a message to a client. 
%	The process creation is needed in order to avoid deadlock, which would occur if the function
%	simply made a request without starting a new process.
%
%	Returns: the pid for the created process.
spawnMessage(Client, Channel, UserNick, Message) -> spawn (fun () ->
	genserver:request(Client, {incoming_msg, Channel, UserNick, Message})
end ).

