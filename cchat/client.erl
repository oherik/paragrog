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
    case St#client_st.server /= '' of
            true -> {reply, {error, user_already_connected, "User is already connected"}, St};
            false ->try genserver:request(list_to_atom(Server), {connect, St#client_st{pid = self()}}),
                {reply, ok, St#client_st{server = list_to_atom(Server), pid = self()}}
                 catch 
                    _:_ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
                 end
             end;

%% Disconnect from server
handle(St, disconnect) ->
    Response = if St#client_st.server == '' -> user_not_connected;

                St#client_st.channelList /= [] -> leave_channels_first;
                true -> try genserver:request(St#client_st.server, {disconnect, St})
                   catch
                        _:_ -> server_not_reached
                    end
             end,
    Message = if Response == ok -> ok;
                Response == user_not_connected -> {error, user_not_connected, "User is not connected"};
                Response ==  leave_channels_first -> {error, leave_channels_first, "Leave all channels first"};
                Response == server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                true -> {'EXIT', "Something went wrong"}
            end,
    St_update = if Response == ok -> St#client_st{server = '', pid = ''};
                 true -> St
                end,
 
    {reply, Message, St_update} ;

% Join channel
handle(St, {join, Channel}) ->
    Response =  case lists:member(list_to_atom(Channel), St#client_st.channelList) of
            false -> try genserver:request(St#client_st.server, {join, St, Channel})
                 catch
                     _:_ -> server_not_reached
                end;
            true -> user_already_joined
        end, 
        
      
    Message = if Response == ok -> ok;
                Response == server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                Response == user_already_joined -> {error, user_already_joined, "User already joined the channel"};
                true -> {'EXIT', "Something went wrong"}
            end,
    St_update = if Response == ok -> St#client_st{channelList = lists:append(St#client_st.channelList, [list_to_atom(Channel)])};
                true -> St
            end,
    {reply, Message, St_update};

%% Leave channel
handle(St, {leave, Channel}) ->
    Response =  case lists:member(list_to_atom(Channel), St#client_st.channelList) of
         false -> user_not_joined;
        true -> try genserver:request(St#client_st.server, {leave, St, Channel})
                catch
                     _:_ -> server_not_reached
                end
            end,
    Message = if Response == ok -> ok;
                Response == server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                Response == user_not_joined -> {error, user_not_joined, "User has not joined the channel"};
                Response == channel_not_found -> {error, channel_not_found, "The channel does not exist"};
                true -> {'EXIT', "Something went wrong"}
            end,
     St_update = if Response == ok -> St#client_st{channelList = lists:delete(list_to_atom(Channel), St#client_st.channelList)};
                true -> St
            end,       
    {reply, Message, St_update};

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Response =  case lists:member(list_to_atom(Channel), St#client_st.channelList) of
                false -> user_not_joined;
                true -> try genserver:request(list_to_atom(Channel), {msg_from_GUI, St, Msg})
           
                        catch
                          _:_ -> server_not_reached
                        end
                end,
    Message = if Response == ok -> ok;
                Response == server_not_reached -> {error, server_not_reached, "Server could not be reached"};
                Response == user_not_joined -> {error, user_not_joined, "User has not joined the channel"};
                true -> {'EXIT', "Something went wrong"}
            end,
    {reply, Message, St};

%% Get current nick
handle(St, whoami) ->
    {reply, atom_to_list(St#client_st.nick), St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    if St#client_st.server == '' -> 
        {reply, ok, St#client_st{nick = list_to_atom(Nick)}} ;
        true -> {reply, {error, user_already_connected, "Changing nick is not allowed when connected"}, St}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(GUIName, {msg_to_GUI, Channel, atom_to_list(Name)++"> "++Msg}),
    {reply, ok, St}.
