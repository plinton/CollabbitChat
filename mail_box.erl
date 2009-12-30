-module(mail_box).

-define(CHATROOM, chat_room).

-compile(export_all).

start(UserName, ChatPid) ->
    erlang:register(?CHATROOM, ChatPid),    
    spawn(mail_box, message_holding, [UserName, Msgs = []]).


message_holding(UserName, Msgs) ->
    receive
        {store_message, Name, MessageBody} ->
            NewMessages = lists:append(Msgs, [{Name, MessageBody}]),
            message_holding(UserName, NewMessages);
        get_messages ->
            ?CHATROOM ! {return_messages, UserName, Msgs},
            Messages = [],
            message_holding(UserName, Messages);
        leave -> ok;
        Oops ->
            message_holding(UserName, Msgs)
    end.
