-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
	#channel_st{channelName = ChannelName}.

handle(St, {join, User}) ->
	case lists:member(User, St#channel_st.connectedUsers) of
		false ->
		UpdatedState = #channel_st{connectedUsers = lists:append(St#channel_st.connectedUsers, User)},
		  {reply, ok, UpdatedState};
		 true ->
			{reply, user_already_joined, St}
    end.
