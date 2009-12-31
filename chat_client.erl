-module(chat_client).

-compile(export_all).

register_nickname(Name, Room) ->
    Nickname = {Name, Room},
    Pid = spawn(chat_client, handle_messages, [Nickname]),
    message_router:register_nick(Nickname, Pid).

unregister_nickname(Name, Room) ->
    Nickname = {Name, Room},
    message_router:unregister_nick(Nickname).

send_message(OwnName, Room, MessageBody) ->
    Sender = {OwnName, Room},
    message_router:send_chat_message(Sender, MessageBody).

handle_messages(Nickname) ->
    receive
	{printmsg, MessageBody} ->
	    io:format("~p sent: ~p~n", [element(1,Nickname), MessageBody]),
	    handle_messages(Nickname);
	stop ->
	    ok
    end.

start_router() ->
    message_router:start().
