-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% We decided to do as many checks as possible on the client to avoid bottlenecks on the server. 
%% This is why the channels the client is connected to are saved in the client state, instead of e.g.
%% having to iterate through a list in the server. This is also why the client in some instances communicates
%% directly with the channel (altohugh via the genserver) instead of via the server.

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
% Request is sent to connect to the server if the user is not already connected. The server is also added to the client.
handle(St, {connect, Server}) ->
    %Check if user is already connected
    case St#client_st.server /= '' of
        true -> {reply, {error, user_already_connected, "User is already connected"}, St};
        %if user is not connected, send request to server
        false ->
            try 
                case genserver:request(list_to_atom(Server), {connect, St#client_st.nick, self()}) of
                nick_taken ->      %A user with the same nick is already connected
                    {reply, {error, nick_taken , "A user with this nick is already connected"}, St};
                ok->        %user is now connected to the server and server is added to client state
                    {reply, ok, St#client_st{server = list_to_atom(Server)}}
                end
            catch 
                %The server could not be reached
                _:_ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
            end
    end;

%% Disconnect from server
% Request is sent to the server to disconnect if the user is connected to any server. The 
% server is also removed from the client.
handle(St, disconnect) ->
    %Check if user is connected, if not no disconnection is needed
    if St#client_st.server == '' -> {reply, {error, user_not_connected, "User is not connected"}, St};
        %Check if user has left all channels
        St#client_st.channelList /= [] -> {reply, {error, leave_channels_first, "Leave all channels first"}, St};
        %Request is sent ot server to disconnect user and server is removed from client state
        true -> 
            try 
                genserver:request(St#client_st.server, {disconnect, St#client_st.nick, self()}),
                {reply, ok, St#client_st{server = ''}}
           catch
                %The server could not be reached
                _:_ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
            end
     end;

% Join channel
% Request is sent to the server to join the channel if the user has not already joined the channel. 
% The channel is also added to the client.
handle(St, {join, Channel}) ->
    %Check if user has already joined the channel
   case lists:member(list_to_atom(Channel), St#client_st.channelList) of
            %If user has not joined the channel, send a request to server to join Channel and 
            % add channel to channel list in client state
            false -> 
                try 
                    genserver:request(St#client_st.server, {join, self(), Channel}),
                    {reply, ok, St#client_st{channelList = lists:append(St#client_st.channelList, [list_to_atom(Channel)])}}
                catch
                    %Server could not be reached
                     _:_ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
                end;
            %User has already joined the channel
            true -> {reply, {error, user_already_joined, "User already joined the channel"}, St}
        end;
    
%% Leave channel
% Request is sent to the channel to leave the channel if the user has joined the channel. 
% The channel is also removed from the client.
handle(St, {leave, Channel}) ->
    % Check if user has joined the channel
    case lists:member(list_to_atom(Channel), St#client_st.channelList) of
        % User has not joined channel
         false -> {reply, {error, user_not_joined, "User has not joined the channel"}, St};
         %U ser has joined channel, request is sent to server to leave the channel and the 
         % channel is removed from the channel list in client state
         true -> 
            try 
                genserver:request(list_to_atom(atom_to_list(St#client_st.server) ++ Channel), {leave, self()}),
                {reply, ok, St#client_st{channelList = lists:delete(list_to_atom(Channel), St#client_st.channelList)}}
            catch
                %server could not be reached
                 _:_ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
            end
        end;

% Sending messages
% A request is sent to the Channel to send the message if the user is has joined the Channel
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % Check if user has joined the channel
    case lists:member(list_to_atom(Channel), St#client_st.channelList) of
        false -> {reply, {error, user_not_joined, "User has not joined the channel"}, St};
        %If user has joined the channel, request is sent to channel to send message.
        true -> 
            try     
                FullChannelName = atom_to_list(St#client_st.server) ++ Channel,
                genserver:request(list_to_atom(FullChannelName), {msg_from_GUI, St#client_st.nick, self(), Msg}),
                {reply, ok, St}
            catch
                  _:_ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
            end
     end;
    
% Get current nick
% The nick saved in client state is returned.
handle(St, whoami) ->
    {reply, atom_to_list(St#client_st.nick), St} ;

%% Change nick
% The nick is changed and saved in client state as long as the user is not connected to any server.
handle(St, {nick, Nick}) ->
    if St#client_st.server == '' -> 
        {reply, ok, St#client_st{nick = list_to_atom(Nick)}} ;
        true -> {reply, {error, user_already_connected, "Changing nick is not allowed when connected"}, St}
    end;    

%% Incoming message is recieved from channel and sent to the gui
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(GUIName, {msg_to_GUI, Channel, atom_to_list(Name)++"> "++Msg}),
    {reply, ok, St}.
