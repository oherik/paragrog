-module(channel).
-export([handle/2, initial_state/1, sendMessage/2]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

handle(St, {join, User}) ->
	case lists:member(User, St#channel_st.connectedUsers) of
		false ->
		UpdatedState = #channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, [User])},
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

				Pids = [ChannelUser || ChannelUser <- St#channel_st.connectedUsers, 
					ChannelUser#client_st.nick/= User],
					MessageArguments = {St#channel_st.channelName, User, Msg},
				rpc:pmap(fun sendMessage/2, MessageArguments, Pids),
				%list_to_atom(Channel)!{send, MessageArguments},	% Send a message to the channel
				{reply, ok, St};
			false ->	
				{reply, user_not_joined, St}	% The user hasn't joined a channel
			end.


sendMessage(Client, {Channel, User, Message}) ->
	genserver:request(Client#client_st.pid, {incoming_msg, Channel, User, Message}).
