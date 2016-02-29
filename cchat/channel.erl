-module(channel).
-export([handle/2, initial_state/1, spawnMessage/4]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

handle(St, {join, User}) ->
	{reply, ok, St#channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, [User#client_st.pid])}};

handle(St, {leave, User}) ->
	{reply, ok, St#channel_st{connectedUsers = lists:delete(User#client_st.pid, St#channel_st.connectedUsers)}};

handle(St, {msg_from_GUI, User, Msg}) ->
	PID = User#client_st.pid,
	Clients = [ChannelUser || ChannelUser <- St#channel_st.connectedUsers, 
		ChannelUser/= PID],
	rpc:pmap({channel, spawnMessage}, [St#channel_st.channelName,User, Msg], Clients),
	{reply, ok, St}.

spawnMessage([],_,_,_) -> no_client;

spawnMessage(Client, Channel, User, Message) -> spawn (fun () ->
genserver:request(Client, {incoming_msg, Channel, User#client_st.nick, Message})
 end ).

