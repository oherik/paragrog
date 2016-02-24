-module(channel).
-export([handle/2, initial_state/1, sendMessage/4, spawnMessage/4]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

handle(St, {join, User}) ->
	case lists:member(User, St#channel_st.connectedUsers) of
		false ->
		UpdatedState = St#channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, [User])},
		  {reply, ok, UpdatedState};
		 true ->
			{reply, user_already_joined, St}
    end;

handle(St, {leave, User}) ->
		case lists:member(User, St#channel_st.connectedUsers) of
			true -> {reply, ok, St#channel_st{connectedUsers = lists:delete(User, St#channel_st.connectedUsers)}};
			false -> {reply, user_not_joined, St}
		end;

handle(St, {msg_from_GUI, User, Msg}) ->
	case lists:member(User, St#channel_st.connectedUsers) of
		true -> 
		Clients = [ChannelUser || ChannelUser <- St#channel_st.connectedUsers, 
			ChannelUser/= User],
		rpc:pmap({channel, spawnMessage}, [St#channel_st.channelName,User, Msg], Clients),
		{reply, ok, St};
	false ->	
		{reply, user_not_joined, St}	% The user hasn't joined a channel
	end;

handle(St, {find_user, User}) ->
	{reply, lists:member(User, St#channel_st.connectedUsers), St}.

spawnMessage([],_,_,_) -> no_client;
spawnMessage(Client, Channel, User, Message) -> spawn (fun () ->sendMessage(Client, Channel, User, Message) end ).
sendMessage(Client, Channel, User, Message) -> 
	genserver:request(Client#client_st.pid, {incoming_msg, Channel, User#client_st.nick, Message}).
