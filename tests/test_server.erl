-module(test_server).
-export([start_clients/1, client/2]).

sleep(Ms) ->
  receive
  after
    Ms -> ok
  end.

client(N, Socket) ->
  V = integer_to_list(N),
  Put = list_to_binary(lists:concat(["PUT ", V, " ", V, [$\n]])),
  Get = list_to_binary(lists:concat(["GET ", V, [$\n]])),
  gen_tcp:send(Socket, Put),
  gen_tcp:send(Socket, Get),
  gen_tcp:recv(Socket, 3),
  gen_tcp:recv(Socket, 3),
  case gen_tcp:recv(Socket, length(V) + 1, 1000) of
    {ok, B} ->  S = binary_to_list(B),
                K = list_to_integer(lists:droplast(S)),
                if  N /= K -> io:fwrite("Error, N: ~p, K: ~p~n", [N, K]);
                    true   -> ok
                end;
    {error, timeout} -> io:fwrite("Sin tiempo~n");
    BadAns -> io:fwrite("Respuesta incorrecta: ~p~n", [BadAns]),
              notok
  end,
  %sleep(1000),
  client(N, Socket).

start_clients(0) -> ok;
start_clients(N) when is_integer(N) ->
  {ok, Socket} = gen_tcp:connect("localhost", 8888, [binary, {active, false}]),
  spawn(?MODULE, client, [N, Socket]),
  start_clients(N - 1).
