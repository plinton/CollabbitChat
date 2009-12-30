-module(chat_room).

-define(SERVER, chat_room).

-compile(export_all).

start(RoomName) ->
    Pid = spawn(chat_room, route_messages, [RoomName, dict:new()]),
    erlang:register(?SERVER, Pid),    
    Pid.

send_message(UserList, Name, Message) ->
    case length(UserList) of
        1 ->
            User = lists:last(UserList),
            element(2, User) ! {store_message, Name, Message};
        _ ->
            User = lists:last(UserList),
            element(2, User) ! {store_message, Name, Message},
            send_message(lists:delete(User, UserList), Name, Message)
    end.
        

route_messages(RoomName, Users) ->
    receive
        {join, Name} ->
            case dict:is_key(Name, Users) of
                true -> route_messages(RoomName, Users);
                _ ->
                    NewBox = mail_box:start(Name, ?SERVER),
                    route_messages(RoomName, dict:store(Name, NewBox, Users))
            end;
        {leave, Name} ->
            case dict:is_key(Name, Users) of
                true ->
                    dict:fetch(Name, Users) ! leave,
                    case dict:size(Users) of
                        0 -> ok;
                        _ -> 
                            route_messages(RoomName, dict:erase(Name, Users))
                    end;
                _ -> route_messages(RoomName, Users)
            end;
        {get_messages, Name} ->
            dict:fetch(Name, Users) ! {get_messages, ?SERVER},
            route_messages(RoomName, Users);
        {return_messages, Name, Msgs} ->
            io:format(Msgs),
            route_messages(RoomName, Users);
        get_users ->
            io:format(dict:to_list(Users)),
            route_messages(RoomName, Users);
        {broadcast, Name, MessageBody} ->
            UserList = dict:to_list(Users),
            send_message(UserList, Name, MessageBody),
            route_messages(RoomName, Users);
        _ -> 
            io:format("There was an error~n"),
            route_messages(RoomName, Users)
        end.
    
