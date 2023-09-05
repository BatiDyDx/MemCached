-module(test_server).
-export([start_clients/1, client/1]).

sleep(Ms) ->
  receive
  after
    Ms -> ok
  end.

client(Socket) ->
  gen_tcp:send(Socket, <<"PUT abc asdfghjkl", $\n>>),
  case gen_tcp:recv(Socket, 3, 1000) of
    {ok, <<"OK", $\n>>} -> ok;
    {error, timeout} -> io:fwrite("Sin tiempo~n");
    BadAns -> io:fwrite("Respuesta incorrecta: ~p~n", [BadAns]),
              notok
  end,
  sleep(1000),
  client(Socket).

start_clients(0) -> ok;
start_clients(N) when is_integer(N) ->
  {ok, Socket} = gen_tcp:connect("localhost", 8888, [binary, {active, false}]),
  spawn(?MODULE, client, [Socket]),
  start_clients(N - 1).
