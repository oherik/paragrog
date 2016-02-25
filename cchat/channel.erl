-module(channel).
-export([handle/2, initial_state/1, spawnMessage/4]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

handle(St, {join, User}) ->
	PID = User#client_st.pid,
	case lists:member(PID, St#channel_st.connectedUsers) of
		false ->
		UpdatedState = St#channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, [PID])},
		  {reply, ok, UpdatedState};
		 true ->
			{reply, user_already_joined, St}
    end;

handle(St, {leave, User}) ->
	PID = User#client_st.pid,
		case lists:member(PID, St#channel_st.connectedUsers) of
			true -> {reply, ok, St#channel_st{connectedUsers = lists:delete(PID, St#channel_st.connectedUsers)}};
			false -> {reply, user_not_joined, St}
		end;

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

