-module(mucc).

-define(SERVER, mucc).

-compile(export_all).

start() ->
  server_util:start(?SERVER, {mucc, server_loop, [dict:new()]}).

stop() ->
  server_util:stop(?SERVER).

register_nickname(Nickname) ->
  global:send(?SERVER, {register, Nickname, self()}),
  receive
    ok ->
      ok;
    {error, Error} ->
      Error
  end.

poll(Nickname) ->
  global:send(?SERVER, {poll, Nickname, self()}),
  receive
    {ok, Messages} ->
      Messages;
    Error ->
      Error
  end.

send_message(Sender, Addressee, Message) ->
  global:send(?SERVER, {send_message, Sender, Addressee, Message}).

unregister(Nickname) ->
  global:send(?SERVER, {unregister, Nickname}).

crash(Nickname) ->
  global:send(?SERVER, {crash, Nickname}).

server_loop(Proxies) ->
  receive
    {register, Nickname, Caller} ->
      case dict:find(Nickname, Proxies) of
	error ->
	  Pid = spawn(fun() ->
			  process_flag(trap_exit, true),
			  proxy_client([]) end),
	  erlang:monitor(process, Pid),
	  message_router:register_nick(Nickname, Pid),
	  Caller ! ok,
	  server_loop(dict:store(Nickname, Pid, Proxies));
	{ok, _} ->
	  Caller ! {error, duplicate_nick_found},
	  server_loop(Proxies)
      end;
    {poll, Nickname, Caller} ->
      case dict:find(Nickname, Proxies) of
	error ->
	  Caller ! {error, unknown_nick};
	{ok, Pid} ->
	  Pid ! {get_messages, self()},
	  receive
	    {messages, Messages} ->
	      Caller ! {ok, Messages}
	  end
      end,
      server_loop(Proxies);
    {send_message, Sender, Addressee, Message} ->
      case dict:find(Sender, Proxies) of
	error ->
	  ok;
	{ok, Pid} ->
	  Pid ! {send_message, Addressee, Message}
      end,
      server_loop(Proxies);
    {unregister, Nickname} ->
      case dict:find(Nickname, Proxies) of
	error ->
	  server_loop(Proxies);
	{ok, Pid} ->
	  Pid ! stop,
	  server_loop(dict:erase(Nickname, Proxies))
      end
  end.

proxy_client(Messages) ->
  receive
    {printmsg, MessageBody} ->
      proxy_client([MessageBody|Messages]);
    {get_messages, Caller} ->
      Caller ! {messages, lists:reverse(Messages)},
      proxy_client([]);
    {send_message, Addressee, Message} ->
      message_router:send_chat_message(Addressee, Message),
      proxy_client(Messages);
    stop ->
      io:format("Proxy stopping...~n"),
      ok
  end.

find_nick(Pid, Proxies) when is_pid(Pid) ->
  Nicks = dict:fold(fun(Key, Value, Acc) ->
			if
			  Value == Pid ->
			    [Key|Acc];
			  true ->
			    Acc
			end
		    end,
		    [],
		    Proxies),
  case length(Nicks) == 0 of
    true ->
      not_found;
    false ->
      lists:nth(1, Nicks)
  end.
