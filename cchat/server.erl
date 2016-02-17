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
    io:fwrite("Server received: ~p~n", [User]),
    case listFind(User, St#server_st.connectedUsers) of
    			true -> {reply, user_already_connected, St};
    			false -> Updated_St = St#server_st{connectedUsers = lists:append(St#server_st.connectedUsers, [User])},
    			{reply, ok, Updated_St}
	end;
handle(St, {disconnect, User}) ->
	io:fwrite("Server received: ~p~n", [User]),
	case channelFind(User, St#server_st.channelList) of
		true -> {reply, leave_channels_first, St};
		false -> Updated_St = St#server_st{connectedUsers = lists:delete(User, St#server_st.connectedUsers)},
		{reply, ok, Updated_St}
	end.




listFind ( Element, [] ) ->
    false;

listFind ( Element, [ Item | ListTail ] ) ->
    case ( iolist_equal(Item, Element) ) of
        true    ->  true;
        false   ->  listFind(Element, ListTail)
    end.

iolist_equal(A, B) ->
    iolist_to_binary(A) =:= iolist_to_binary(B).
    
channelFind( Element, []) ->
	false;

channelFind( Element, [Item | ListTail]) ->
	case listFind(Element, Item)  of
		true 	-> true;
		false	-> channelFind(Element, ListTail)
	end.

