ssert_ok(to_string(_ClientAtom1)++" pings "++_Nick2, Result2),

    % Make sure pong is received
    % Don't check message format, since students may change it
    receive
        {msg_to_SYSTEM, _Msg} ->
            assert_ok(to_string(_ClientAtom1)++" receives pong",ok),
            putStrLn(green(_Msg))
    after
        1000 ->
            putStrLn(red("nothing received after 1000ms")),
            ?assert(false)
    end.

% --- Bad unit tests ---------------------------------------------------------

% Connecting to non-existent server
connect_nonexistent_server_test() ->
    init("connect_nonexistent_server"),
    putStrLn("Wait a few seconds for timeout..."),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom, {connect, "mordor"}),
    assert_error("connecting to server mordor", Result, server_not_reached).

% Connecting to non-responding server
connect_nonresponding_server_test() ->
    Name = "connect_nonresponding_server",
    putStrLn(blue("\n# Test: "++Name)),
    Pid = genserver:start(?SERVERATOM, {}, fun (St, _Msg) -> timer:sleep(100000), {dead, St} end), %% blocking server
    assert("server startup", is_pid(Pid)),
    putStrLn("Wait a few seconds for timeout..."),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom, {connect, ?SERVER}),
    assert_error("connecting to non-responsive server", Result, server_not_reached).

% Logging in with a name that is taken
connect_registered_nick_test() ->
    init("connect_registered_nick"),

    % Client 1
    {_Pid1, Nick1, _ClientAtom1} = new_client_connect(),

    % Client 2, set nick to client1's
    {_Pid2, _Nick2, ClientAtom2} = new_client(Nick1),
    Result = request(ClientAtom2, {connect, ?SERVER}),
    assert_error(to_string(ClientAtom2)++" connecting as "++_Nick2, Result, nick_taken).

% Disconnect when not connected
disconnect_not_connected_test() ->
    init("disconnect_not_connected"),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom, disconnect),
    assert_error("disconnecting when not connected", Result, user_not_connected).

% Disconnect when still in channels
disconnect_leave_channels_first_test() ->
    init("disconnect_leave_channels_first"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client_connect(),
    join_channel(ClientAtom, Channel),

    Result2 = request(ClientAtom, disconnect),
    assert_error(to_string(ClientAtom)++" disconnects without leaving "++Channel, Result2, leave_channels_first).

% Joining already joined
join_already_joined_test() ->
    init("join_already_joined"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client_connect(),
    join_channel(ClientAtom, Channel),

    Result2 = request(ClientAtom,{join,Channel}),
    assert_error(to_string(ClientAtom)++" joins "++Channel, Result2, user_already_joined).

% Writing when not joined
write_not_joined_test() ->
    init("write_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(),
    Result = request(ClientAtom2,{msg_from_GUI,Channel,"hi"}),
    assert_error(to_string(ClientAtom2)++" writing to "++Channel, Result, user_not_joined).

% Leaving when not joined
leave_not_joined_test() ->
    init("leave_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(),
    Result2 = request(ClientAtom2,{leave,Channel}),
    assert_error(to_string(ClientAtom2)++" leaving "++Channel, Result2, user_not_joined).

% Trying to take a nick which is taken
nick_taken_test_DISABLED() ->
    init("nick_taken"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, Nick2, ClientAtom2} = new_client_connect(),
    join_channel(ClientAtom2, Channel),

    % Change nick of 1 to 2
    Result = request(ClientAtom1,{nick,Nick2}),
    assert_error(to_string(ClientAtom1)++" changing nick to "++Nick2, Result, nick_taken).

% --- Concurrency unit tests -------------------------------------------------

robustness_channel_test_DISABLE() ->
    {timeout, 10, [{test_client,robustness_channel}]}.

-define(CONC_1_CHANS, 4).
-define(CONC_1_USERS, 3). % per channel
-define(CONC_1_MSGS, 2). % per user

% Force one request to hang and see if the others still make progress
%
%    ch1       ch2       ch3
%   / | \     / | \     / | \
% u1 u2 u3  u4 u5 u6  u7 u8 u9
robustness_channel() ->
  NRecvs = ?CONC_1_CHANS * ?CONC_1_USERS * (?CONC_1_USERS - 1) * ?CONC_1_MSGS, % sent to clients
  random:seed(os:timestamp()),
  SleepCount = random:uniform(NRecvs div 4), % how many will sleep
  SleepNs = lists:usort([random:uniform(NRecvs) || _ <- lists:seq(1, SleepCount)]),

  % The sleepy process will tell request to sleep, if N in SleepNs it's a client
  % Everyone else can continue
  Sleepy = fun (F, {N, ClientPids}) ->
    receive
      {add_client, Pid} ->
        F(F, {N, ClientPids ++ [Pid]}) ;
      {hi, ToPid, Pid} ->
        IsToClient = lists:member(ToPid, ClientPids),
        ShallSleep = lists:member(N, SleepNs),
        if
          (not IsToClient) ->
            Pid ! {go},
            F(F, {N, ClientPids}) ;
          (ShallSleep) ->
            Pid ! {wait, 500000}; % ms
          true ->
            Pid ! {go}
        end,
        F(F, {N+1, ClientPids})
    end
  end,
  catch(unregister(sleepy)),
  register(sleepy, spawn(fun () -> Sleepy(Sleepy, {1, []}) end)),

  init("robustness_channel"),
  ParentPid = self(),
  UsersSeq = lists:seq(1, ?CONC_1_USERS * ?CONC_1_CHANS),
  MsgsSeq  = lists:seq(1, ?CONC_1_MSGS),

  % Connect and join channel
  Fjoin = fun (I) ->
    try
      output_off(),
      Is = lists:flatten(integer_to_list(I)),
      Nick = "user_conc1_"++Is,
      ClientName = "client_conc1_"++Is,
      ClientAtom = list_to_atom(ClientName),
      GUIName = "gui_conc1_"++Is,
      new_gui(GUIName, ParentPid),
      ClientPid = genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
      sleepy ! {add_client, ClientPid},
      connect(ClientAtom),

      Ch_Ix = (I rem ?CONC_1_CHANS) + 1,
      Ch_Ixs = lists:flatten(io_lib:format("~p", [Ch_Ix])),
      Channel = "#channel_"++Ch_Ixs,
      join_channel(ClientAtom, Channel),
      {ClientAtom, Channel}
    catch Ex ->
      ParentPid ! {failed, Ex} % ignored
    end
  end,

  % Send messages
  Fsend = fun ({ClientAtom,Channel}) ->
    try
      % send all messages
      Send = fun (I2) ->
        Is2 = lists:flatten(io_lib:format("~p", [I2])),
        Msg = "message_"++Is2,
        request(ClientAtom, {msg_from_GUI,Channel,Msg})
      end,
      spawn(fun () ->
        lists:foreach(Send, MsgsSeq),
        ParentPid ! {ready, ClientAtom} % ignored
      end),
      ClientAtom
    catch Ex ->
      ParentPid ! {failed, Ex} % ignored
    end
  end,
  putStrLn("spawning ~p channels x ~p clients x ~p messages each (~p of ~p requests will block)", [?CONC_1_CHANS, ?CONC_1_USERS, ?CONC_1_MSGS, SleepCount, NRecvs]),
  ClientAtoms = lists:map(Fjoin, UsersSeq),
  output_on(),

  lists:foreach(fun (I) -> spawn(fun() -> Fsend(I) end) end, ClientAtoms),

  % Receive all pending messages
  Recv = fun (Fn, N) ->
    receive
      {msg_to_GUI, _From, _Msg} -> Fn(Fn, N+1)
    after
      500 -> N
    end
  end,
  Oks = Recv(Recv, 0),
  Timeouts = NRecvs - Oks,
  putStrLn("messages: ~p successful, ~p timed out, ~p total", [Oks, Timeouts, NRecvs]),
  MinRecvs = NRecvs - SleepCount,
  Cond = (Oks >= MinRecvs),
  Msg = sprintf("successful messages is at least ~p", [MinRecvs]),
  assert(Msg, Cond).


robustness_server_test_DISABLE() ->
    {timeout, 10, [{test_client,robustness_server}]}.

-define(CONC_3_CHANS, 4).
-define(CONC_3_USERS, 3). % per channel
-define(CONC_3_MSGS, 2). % per user

% Kill the server and see if channels still make progress
%
%    ch1       ch2       ch3
%   / | \     / | \     / | \
% u1 u2 u3  u4 u5 u6  u7 u8 u9
robustness_server() ->
  NRecvs = ?CONC_3_CHANS * ?CONC_3_USERS * (?CONC_3_USERS - 1) * ?CONC_3_MSGS, % sent to clients

  ServerPid = init("robustness_server"),
  ParentPid = self(),
  UsersSeq = lists:seq(1, ?CONC_3_USERS * ?CONC_3_CHANS),
  MsgsSeq  = lists:seq(1, ?CONC_3_MSGS),

  % Connect and join channel
  Fjoin = fun (I) ->
    try
      output_off(),
      Is = lists:flatten(integer_to_list(I)),
      Nick = "user_conc3_"++Is,
      ClientName = "client_conc3_"++Is,
      ClientAtom = list_to_atom(ClientName),
      GUIName = "gui_conc3_"++Is,
      new_gui(GUIName, ParentPid),
      genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
      connect(ClientAtom),

      Ch_Ix = (I rem ?CONC_3_CHANS) + 1,
      Ch_Ixs = lists:flatten(io_lib:format("~p", [Ch_Ix])),
      Channel = "#channel_"++Ch_Ixs,
      join_channel(ClientAtom, Channel),
      {ClientAtom,Channel}
    catch Ex ->
      ParentPid ! {failed, Ex} % ignored
    end
  end,

  % Send messages
  Fsend = fun ({ClientAtom,Channel}) ->
    try
      Send = fun (I2) ->
        Is2 = lists:flatten(io_lib:format("~p", [I2])),
        Msg = "message_"++Is2,
        request(ClientAtom, {msg_from_GUI,Channel,Msg})
      end,
      spawn(fun () ->
        lists:foreach(Send, MsgsSeq),
        ParentPid ! {ready, ClientAtom} % ignored
      end)
    catch Ex ->
      ParentPid ! {failed, Ex} % ignored
    end
  end,
  putStrLn("spawning ~p channels x ~p clients each", [?CONC_3_CHANS, ?CONC_3_USERS]),
  ClientAtoms = lists:map(Fjoin, UsersSeq),
  output_on(),

  Killed = exit(ServerPid, kill),
  assert("killing server", Killed),

  putStrLn("sending messages"),
  lists:foreach(fun (I) -> spawn(fun() -> Fsend(I) end) end, ClientAtoms),

  % Receive all pending messages
  Recv = fun (Fn, N) ->
    receive
      {msg_to_GUI, _From, _Msg} -> Fn(Fn, N+1)
    after
      500 -> N
    end
  end,
  Oks = Recv(Recv, 0),
  Timeouts = NRecvs - Oks,
  putStrLn("messages: ~p successful, ~p timed out, ~p total", [Oks, Timeouts, NRecvs]),
  Cond = (Oks =:= NRecvs),
  Msg = "all messages successful",
  assert(Msg, Cond).


% Counts how many processes are created when clients join channels

-define(CONC_2_CHANS, 4).
-define(CONC_2_USERS, 3).

process_usage_test_DISABLED() ->
  init("process_usage"),
  ParentPid = self(),
  ChansSeq = lists:seq(1, ?CONC_2_CHANS),
  UsersSeq = lists:seq(1, ?CONC_2_USERS),
  Procs1 = length(erlang:processes()),
  Fconnect = fun (I) ->
    Is = lists:flatten(integer_to_list(I)),
    Nick = "user_conc2_"++Is,
    ClientName = "client_conc2_"++Is,
    ClientAtom = list_to_atom(ClientName),
    GUIName = "gui_conc2_"++Is,
    new_gui(GUIName),
    genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
    connect(ClientAtom),
    ClientAtom
  end,
  Fjoin = fun (ClientAtom) ->
    fun () ->
      output_off(),
      G = fun(Ch_Ix) ->
        Ch_Ixs = lists:flatten(io_lib:format("~p", [Ch_Ix])),
        Channel = "#channel_"++Ch_Ixs,
        join_channel(ClientAtom, Channel)
      end,
      lists:foreach(G, ChansSeq),
      ParentPid ! {ready, 123}
    end
  end,
  putStrLn("spawning ~p clients and connecting to server", [?CONC_2_USERS]),
  output_off(),
  ClientAtoms = lists:map(Fconnect, UsersSeq),
  output_on(),
  Procs2 = length(erlang:processes()),
  Msg2 = sprintf("processes scale with clients (~p -> ~p)", [Procs1, Procs2]),
  assert(Msg2, (Procs2 - Procs1) =:= (2 * ?CONC_2_USERS)), % 2 per client

  putStrLn("each client joins the same ~p channels", [?CONC_2_CHANS]),
  lists:foreach(fun (Atom) -> spawn(Fjoin(Atom)) end, ClientAtoms),
  Recv = fun (_) ->
    receive
      {ready, Time} -> Time ;
      {failed, Ex} -> putStrLn(Ex), throw("")
    end
  end,
  lists:map(Recv, UsersSeq),
  Procs3 = length(erlang:processes()),
  Msg3 = sprintf("processes scale with channels (~p -> ~p)", [Procs2, Procs3]),
  Cond = (Procs3 > Procs2) and ((Procs3 - Procs2) rem (?CONC_2_CHANS) =:= 0), % at least one each
  assert(Msg3, Cond),
  ok.

% --- Performance unit tests -------------------------------------------------

-define(PERF_DELAY, 100). % ms

% many_users_one_channel_test_() ->
%     {timeout, 60, [{test_client,many_users_one_channel}]}.
%
% Tests that broadcasting is concurrent
% Creates many users (this needs to be spaced out as logging in might be a bottle neck),
% and sends one message to a channel where all of the users are registered.
many_users_one_channel_one_message(NumUsers) ->
    init("many_users_one_channel_one_message"),
    _ServerPid = whereis(?SERVERATOM),
    Channel = new_channel(),
    ParentPid = self(),
    Ref = make_ref(),
    F = fun (I) ->
                fun () ->
                    try
                        output_off(),
                        Is = lists:flatten(io_lib:format("~p", [I])),
                        % {Pid, Nick, ClientAtom} = new_client("user_"++I),
                        Nick = "user_perf1_"++Is,
                        ClientName = "client_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf1_"++Is,
                        new_gui(GUIName),
                        ClientPid = genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
                        ParentPid ! {client, ClientPid, Ref},
                        T1 = os:timestamp(),
                        receive go -> ok end,
                        try
                          try
                            connect(ClientAtom),
                            join_channel(ClientAtom, Channel)
                          catch
                            T:E ->
                              if I == 1 -> ParentPid ! {msgSent, Ref};
                                 true   -> ok end,
                              apply(erlang, T, [E])
                          after
                            ParentPid ! {joined, Ref}
                          end,
                          receive go2 -> ok end,
                          output_on(),
                          if I == 1 ->
                            TX1 = os:timestamp(),
                            try
                              send_message(ClientAtom, Channel, "message_"++Is++"_1"),
                              TX2 = os:timestamp(),
                              Diff = timer:now_diff(TX2, TX1) div 1000,
                              ColourFun = if
                                Diff > ?PERF_DELAY * 2 -> fun red/1 ;
                                true -> fun green/1
                              end,
                              putStrLn("sending message took ~p ms "++ColourFun("(should be ~~~p ms)"), [Diff, ?PERF_DELAY])
                            after
                            ParentPid ! {msgSent, Ref}
                            end;
                             true   -> ok
                          end,
                          output_off(),
                          receive go3 -> ok end,
                          leave_channel(ClientAtom, Channel),
                          disconnect(ClientAtom)
                        after
                          ParentPid ! {disconnected, Ref}
                        end,
                        T2 = os:timestamp(),
                        ParentPid ! {ready, Ref, timer:now_diff(T2, T1)}
                    catch Ex ->
                        ParentPid ! {failed, Ref, Ex}
                    end
                end
        end,
    Seq = lists:seq(1, NumUsers),
    Spawn = fun (I) -> spawn(F(I)) end,
    Recv  = fun (_) ->
                    receive
                        {ready, Ref, Time} -> Time ;
                        {failed, Ref, Ex} -> putStrLn(Ex),
                                        throw("")
                    end
            end,
    % The sleepy process will tell request every request to a client to sleep for 100ms
    Sleepy = fun (FF, Pids) ->
      receive
        {hi, ToPid, _Pid} ->
          case lists:member(ToPid, Pids) of
            true  -> ToPid ! {wait, ?PERF_DELAY}; % ms
            false -> ToPid ! {go}
          end,
          FF(FF, Pids);
        {add_client, Pid} ->
          FF(FF, [Pid|Pids]);
        stop -> stop
      end
    end,
    catch(unregister(sleepy)),
    register(sleepy, spawn(fun () -> Sleepy(Sleepy, []) end)),
    putStrLn("spawning ~p clients, each connecting to 1 common channel; one message is sent...", [NumUsers]),
    Pids = lists:map(Spawn, Seq),
    [receive {client, CPid, Ref} -> sleepy!{add_client, CPid} end || _ <- Pids ],
    [begin P ! go, receive {joined, Ref} -> ok end end || P <- Pids ],
    putStrLn("all clients connected and joined the channel"),
    [P ! go2 || P <- Pids ],
    receive {msgSent, Ref} -> ok end,
    putStrLn("clients disconnecting..."),
    [begin P ! go3, receive {disconnected, Ref} -> ok end end || P <- Pids ],
    Times = lists:map(Recv, Seq),
    sleepy ! stop,
    summary(Times).

% Similar to previous case, except many messages are sent
many_users_one_channel_many_messages(NumUsers) ->
    init("many_users_one_channel_many_messages"),
    Channel = new_channel(),
    ParentPid = self(),
    Ref = make_ref(),
    F = fun (I) ->
                fun () ->
                    try
                        output_off(),
                        Is = lists:flatten(io_lib:format("~p", [I])),
                        % {Pid, Nick, ClientAtom} = new_client("user_"++I),
                        Nick = "user_perf1_"++Is,
                        ClientName = "client_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf1_"++Is,
                        new_gui(GUIName),
                        ClientPid = genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
                        ParentPid ! {client, ClientPid, Ref},
                        T1 = os:timestamp(),
                        receive go -> ok end,
                        try
                          try
                            connect(ClientAtom),
                            join_channel(ClientAtom, Channel)
                          catch
                            T:E ->
                              if I == 1 -> ParentPid ! {msgSent, Ref};
                                 true   -> ok end,
                              apply(erlang, T, [E])
                          after
                            ParentPid ! {joined, Ref}
                          end,
                          receive go2 -> ok end,
                          output_on(),
                          TX1 = os:timestamp(),
                          try
                            send_message(ClientAtom, Channel, "message_"++Is++"_1"),
                            TX2 = os:timestamp(),
                            Diff = timer:now_diff(TX2, TX1) div 1000,
                            ColourFun = if
                              Diff > ?PERF_DELAY * 2 -> fun red/1 ;
                              true -> fun green/1
                            end,
                            putStrLn("~s: sending message took ~p ms "++ColourFun("(should be ~~~p ms)"), [ClientName, Diff, ?PERF_DELAY])
                          after
                            ParentPid ! {msgSent, Ref}
                          end,
                          output_off(),
                          receive go3 -> ok end,
                          leave_channel(ClientAtom, Channel),
                          disconnect(ClientAtom)
                        after
                          ParentPid ! {disconnected, Ref}
                        end,
                        T2 = os:timestamp(),
                        ParentPid ! {ready, Ref, timer:now_diff(T2, T1)}
                    catch Ex ->
                        ParentPid ! {failed, Ref, Ex}
                    end
                end
        end,
    Seq = lists:seq(1, NumUsers),
    Spawn = fun (I) -> spawn(F(I)) end,
    Recv  = fun (_) ->
                    receive
                        {ready, Ref, Time} -> Time ;
                        {failed, Ref, Ex} -> putStrLn(Ex),
                                        throw("")
                    end
            end,
    % The sleepy process will tell request every request to a client to sleep for 100ms
    Sleepy = fun (FF, Pids) ->
      receive
        {hi, ToPid, _Pid} ->
          case lists:member(ToPid, Pids) of
            true  -> ToPid ! {wait, ?PERF_DELAY}; % ms
            false -> ToPid ! {go}
          end,
          FF(FF, Pids);
        {add_client, Pid} ->
          FF(FF, [Pid|Pids]);
        stop -> stop
      end
    end,
    catch(unregister(sleepy)),
    register(sleepy, spawn(fun () -> Sleepy(Sleepy, []) end)),
    putStrLn("spawning ~p clients, each connecting to 1 common channel; each client sends a message...", [NumUsers]),
    Pids = lists:map(Spawn, Seq),
    [receive {client, CPid, Ref} -> sleepy!{add_client, CPid} end || _ <- Pids ],
    [begin P ! go, receive {joined, Ref} -> ok end end || P <- Pids ],
    putStrLn("all clients connected and joined the channel"),
    [P ! go2 || P <- Pids ],
    [receive {msgSent, Ref} -> ok end || _ <- Pids ],
    putStrLn("clients disconnecting..."),
    [begin P ! go3, receive {disconnected, Ref} -> ok end end || P <- Pids ],
    Times = lists:map(Recv, Seq),
    sleepy ! stop,
    summary(Times).

% many_users_many_channels_test_() ->
%     {timeout, 60, [{test_client,many_users_many_channels}]}.

% Tests that channels are implemented concurrently
many_users_many_channels(NumUsers) ->
    init("many_users_many_channels"),
    ParentPid = self(),
    UsersSeq = lists:seq(1, NumUsers),
    ChansNames = [ "#channel_"++integer_to_list(C) || C <- UsersSeq],
    ChansNames2 = ChansNames ++ [hd(ChansNames)],
    Ref = make_ref(),
    F = fun (I) ->
                fun () ->
                    try
                        output_off(),
                        Is = integer_to_list(I),
                        Nick = "user_perf2_"++Is,
                        ClientName = "client_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf2_"++Is,
                        new_gui(GUIName),
                        genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
                        T1 = os:timestamp(),
                        MyChs = lists:sublist(ChansNames2, I, 2),
                        receive go -> ok end,
                        try
                          try
                            try
                              connect(ClientAtom),
                              [join_channel(ClientAtom, Channel) || Channel <- MyChs]
                            after
                              ParentPid ! {joined, Ref}
                            end,
                            receive go2 -> ok end,
                            output_on(),
                            if I < NumUsers ->
                              TX1 = os:timestamp(),
                              send_message(ClientAtom, hd(MyChs), "message_"++Is),
                              TX2 = os:timestamp(),
                              Diff = timer:now_diff(TX2, TX1) div 1000,
                              ColourFun = if
                                Diff > ?PERF_DELAY * 2 -> fun red/1 ;
                                true -> fun green/1
                              end,
                              putStrLn("~s: sending message took ~p ms "++ColourFun("(should be ~~~p ms)"), [ClientName, Diff, ?PERF_DELAY]) ;
                              true         -> ok
                            end
                          after
                            ParentPid ! {sent, Ref}
                          end,
                          output_off(),
                          receive go3 -> ok end,
                          [leave_channel(ClientAtom, Channel) || Channel <- MyChs],
                          disconnect(ClientAtom)
                        after
                          ParentPid ! {disconnected, Ref}
                        end,
                        T2 = os:timestamp(),
                        ParentPid ! {ready, Ref, timer:now_diff(T2, T1)}
                    catch Ex ->
                        ParentPid ! {failed, Ref, Ex}
                    end
                end
        end,
    Spawn = fun (I) -> spawn(F(I)) end,
    Recv  = fun (_) ->
                    receive
                        {ready, Ref, Time} -> Time ;
                        {failed, Ref, Ex} -> putStrLn(Ex),
                                        throw("")
                    end
            end,
    % The sleepy process will tell request every request to sleep for 100ms
    Sleepy = fun (FF) ->
      receive
        {hi, ToPid, _Pid} ->
          ToPid ! {wait, 50}, % ms
          FF(FF);
        stop -> stop
      end
    end,
    catch(unregister(sleepy)),
    register(sleepy, spawn(fun () -> Sleepy(Sleepy) end)),
    putStrLn("spawning ~p clients, each connecting to 2 channels; ~p messages are sent...", [NumUsers, NumUsers-1]),
    putStrLn("ch1 ch2 ch3 ch4 ... chN (back to ch1)", []),
    putStrLn(" | / | / | / | /   / | /", []),
    putStrLn("u1  u2  u3  u4  ... uN", []),
    putStrLn("clients 1 to ~p send a message, client I sends a message to channel I", [NumUsers-1]),
    Pids = lists:map(Spawn, UsersSeq),
    [begin P ! go, receive {joined, Ref} -> ok end end || P <- Pids ],
    putStrLn("all clients connected and joined the channels"),
    [P ! go2 || P <- Pids ],
    [receive {sent, Ref} -> ok end || _ <- Pids ],
    [begin P ! go3, receive {disconnected, Ref} -> ok end end || P <- Pids ],
    Times = lists:map(Recv, UsersSeq),
    sleepy ! stop,
    summary(Times).

% Display timing summary for perf tests
% Input: list of client times in microseconds
summary(MTimes) ->
    Times = lists:map(fun(X) -> X/1000 end, MTimes),
    Tot = lists:sum(Times),
    Avg = Tot / length(Times),
    Med = lists:nth(length(Times) div 2, lists:sort(Times)),
    putStrLn("Time elapsed: ~wms average / ~wms median", [round(Avg), round(Med)]).


% Workers test (lab 4)
-define(WORKERS, 3).
workers() ->
  _ServerPid = init("workers"),
  ParentPid = self(),
  UsersSeq = lists:seq(1, ?WORKERS),

  % Connect
  Fjoin = fun (I) ->
    try
      output_off(),
      Is = lists:flatten(integer_to_list(I)),
      Nick = "user_conc3_"++Is,
      ClientName = "client_conc3_"++Is,
      ClientAtom = list_to_atom(ClientName),
      GUIName = "gui_conc3_"++Is,
      new_gui(GUIName, ParentPid),
      ClientPid = genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:handle/2),
      connect(ClientAtom),
      {ClientAtom,ClientPid}
    catch Ex ->
      ParentPid ! {failed, Ex} % ignored
    end
  end,

  putStrLn("spawning ~p clients", [?WORKERS]),
  _ClientAtoms = lists:map(Fjoin, UsersSeq),
  output_on(),

  Function = fun(X) -> timer:sleep(200 * abs(X)), X + X end,
  Input = [4,1,7,3,-2,5,2],

  Gold = lists:map(fun(X) -> X + X end, Input),

  putStrLn("sending input: ~p (~p tasks)", [Input, length(Input)]),

  TX1 = os:timestamp(),
  Result = cchat:send_job(?SERVER, Function, Input),
  TX2 = os:timestamp(),
  putStrLn("results: ~p received in ~pms", [Result, timer:now_diff(TX2, TX1) div 1000]),
  Msg = "results received correctly",
  assert(Msg, Result =:= Gold).