-module(channel).
-export([handle/2, initial_state/1, spawnMessage/4]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

handle(St, {join, UserPid}) ->
	{reply, ok, St#channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, [UserPid])}};

handle(St, {leave, UserPid}) ->
	{reply, ok, St#channel_st{connectedUsers = lists:delete(UserPid, St#channel_st.connectedUsers)}};

handle(St, {msg_from_GUI, {UserNick, UserPid}, Msg}) ->
	ConnectedPids = [ConnectedPid || {ConnectedPid,_} <- St#channel_st.connectedUsers, 
		ConnectedPid/= UserPid],
	rpc:pmap({channel, spawnMessage}, [St#channel_st.channelName,UserNick, Msg], ConnectedPids),
	{reply, ok, St}.

spawnMessage([],_,_,_) -> no_client;

spawnMessage(Client, Channel, UserNick, Message) -> spawn (fun () ->
	genserver:request(Client, {incoming_msg, Channel, UserNick, Message})
end ).

