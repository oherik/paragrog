% This record defines the structure of the client process.
% It contains the following fields:
%	nick: the nick (username) of the client
%   gui: the name (or Pid) of the GUI process.
%	server: the server name
%	channelList: a list of all the channels the user is connected to
-record(client_st, {nick, gui, server = '', channelList = []}).

% This record defines the structure of the server process.
% It contains the following fields:
%	serverName: the name of the server
%   connectedUsers: a list of all users connected to the server
-record(server_st, {serverName, connectedUsers = []}).

% This record defines the structure of the channel process
% It contains the following fields:
%	channelName: the name of the channel
%   connectedUsers: a list of all users connected to the channel
-record(channel_st, {channelName, connectedUsers = []}).