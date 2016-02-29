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

handle(St, {connect, User}) ->
case lists:member(User, St#server_st.connectedUsers) of
	false ->
    	{reply, ok, St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [User])}};
    true ->
    	{reply, user_already_connected,St}
    end;

handle(St, {disconnect, User}) ->
	{reply, ok, St#server_st{connectedUsers = lists:delete(User, St#server_st.connectedUsers)}}.