-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
        #client_st { 
        nick = list_to_atom(Nick),
        gui = list_to_atom(GUIName)
    }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    Data = {connect, St#client_st.nick},
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    Response = try genserver:request(ServerAtom, Data)
        catch 
           _:_ -> server_not_reached
        end,
    io:fwrite("Client received: ~p~n", [Response]),
    Message = if Response == ok -> ok;
                Response == user_already_connected -> {error, user_already_connected, "User is already connected"};
                Response ==  server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                true -> {'EXIT', "Something went wrong"}
            end,
    St_update = if Response == ok -> St#client_st{server = ServerAtom};
                true -> St
            end,
    {reply, Message, St_update} ;

%% Disconnect from server
handle(St, disconnect) ->
    Data = {disconnect, St#client_st.nick},
    Response = if St#client_st.server == [] -> user_not_connected;
                true -> try genserver:request(St#client_st.server, Data)
                    catch
                        _:_ -> server_not_reached
                    end
             end,
    Message = if Response == ok -> ok;
                Response == leave_channels_first -> {error, leave_channels_first, "User has to leave all channels first"};
                Response == user_not_connected -> {error, user_not_connected, "User is not connected"};
                Response == server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                true -> {'EXIT', "Something went wrong"}
            end,
    St_update = if Response == ok -> St#client_st{server = []};
                 true -> St
            end,
    % {reply, ok, St} ;
    {reply, Message, St_update} ;

% Join channel
handle(St, {join, Channel}) ->
    Data = {join, St#client_st.nick, Channel},
    Response = try genserver:request(St#client_st.server, Data)
        catch
            _:_ -> server_not_reached
        end,
    Message = if Response == ok -> ok;
                Response == server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                Response == user_already_joined -> {error, user_already_joined, "User already joined the channel"};
                true -> {'EXIT', "Something went wrong"}
            end,
    {reply, Message, St};

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, atom_to_list(St#client_st.nick), St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, ok, St#client_st{nick = list_to_atom(Nick)}} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
