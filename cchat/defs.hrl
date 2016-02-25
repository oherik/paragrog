% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(client_st, {nick, gui, server = '', pid = '', channelList = []}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {serverName, connectedUsers = [], channelList = []}).

-record(channel_st, {channelName, connectedUsers = []}).