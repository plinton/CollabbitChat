-module(message_router).

-define(SERVER, message_router).

-compile(export_all).

start() ->
  server_util:start(?SERVER, {message_router, route_messages, 
    [dict:new(), dict:new()]}),
  message_store:start().

stop() ->
  server_util:stop(?SERVER),
  message_store:stop().

send_chat_message(ClientName, MessageBody) ->
  global:send(?SERVER, {broadcast, ClientName, MessageBody}).

register_nick(ClientName, ClientPid) ->
  global:send(?SERVER, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
  io:format("Unregistering nickname: ~p~n", [ClientName]),
  global:send(?SERVER, {unregister_nick, ClientName}).

route_messages(Clients, Chatrooms) ->
  receive
    {broadcast, Sender, MessageBody} ->
      ClientList = dict:fetch(element(2, Sender), Chatrooms),
      lists:foreach(fun(Client) -> global:send(?SERVER, 
        {send_chat_msg, Client, MessageBody}) end, ClientList),
        route_messages(Clients, Chatrooms);   
    %TODO: change save_message so that it saves the sender    
    {send_chat_msg, ClientName, MessageBody} ->
      case dict:find(ClientName, Clients) of
	    {ok, ClientPid} ->
	      ClientPid ! {printmsg, ClientName, MessageBody};
	    error ->
	      message_store:save_message(ClientName, MessageBody),
	      io:format("Archived message for ~p~n", [ClientName])
      end,
      route_messages(Clients, Chatrooms);
    {register_nick, ClientName, ClientPid} ->
      Messages = message_store:find_messages(ClientName),
      lists:foreach(fun(Msg) -> ClientPid ! {printmsg, ClientName, Msg} end, 
        Messages),
      RoomName = element(2, ClientName),
      Rooms = dict:append(RoomName, ClientName, Chatrooms),  
      route_messages(dict:store(ClientName, ClientPid, Clients), Rooms);
    {unregister_nick, ClientName} ->
      case dict:find(ClientName, Clients) of
	    {ok, ClientPid} ->
	      RoomList = dict:fetch(element(2, ClientName), Chatrooms),
          NewRoomList = lists:delete(ClientName, RoomList),
          ClientPid ! stop,
	      route_messages(dict:erase(ClientName, Clients), 
            dict:store(element(2, ClientName), NewRoomList, Chatrooms));
	    error ->
	      io:format("Error! Unknown client: ~p~n", [ClientName]),
	      route_messages(Clients, Chatrooms)
      end;
    shutdown ->
      io:format("Shutting down~n");
    Oops ->
      io:format("Warning! Received: ~p~n", [Oops]),
      route_messages(Clients, Chatrooms)
  end.
