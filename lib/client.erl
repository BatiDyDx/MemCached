-module(client).
-export([start/2,put/3,del/2,get/2,stats/1,exit/1]).

-define(PORT, 8889).
-define(PPORT, 889). % Puerto privilegiado

-define(PUT, 11).
-define(DEL, 12).
-define(GET, 13).
-define(STATS, 21).
-define(OK, 101).
-define(EINVALID, 111).
-define(ENOTFOUND, 112).
-define(EBINARY, 113).
-define(EBIG, 114).
-define(EUNK, 115).
-define(EOOM, 116).

-define(RECV(Socket, Len),
case gen_tcp:recv(Socket, Len) of
    {error, Reason} -> throw(Reason);
    {ok, Data} -> Data
end).

-define(WAIT_RES,
receive
    {ans, Res} -> Res;
    closed -> closed
end).

recv_ans(Socket) ->
  Ans = ?RECV(Socket, 1),
  case Ans of
      <<?OK>>        -> ok;
      <<?EINVALID>>  -> einvalid;
      <<?ENOTFOUND>> -> enotfound;
      <<?EBINARY>>   -> ebinary;
      <<?EBIG>>      -> ebig;
      <<?EUNK>>      -> eunk;
      <<?EOOM>>      -> eoom;
      _             -> io:fwrite("Respuesta desconocida~n"),
                       error
  end.

recv_ans_data(Socket) ->
    case recv_ans(Socket) of
        ok -> case gen_tcp:recv(Socket,4) of
                  {ok, <<Len:32/big>>} -> {ok, ?RECV(Socket, Len)};
                  {error, Reason} -> throw(Reason)
              end;
        MemError -> MemError
    end.

start(Hostname, Port) ->
    spawn(fun() ->
        % TENEMOS QUE CAPTURAR ERROR DE CONEXION
        {ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active, false},binary]),
        client(Socket)
    end).

process_request(Socket, Data) ->
  {Op, Cmd} = Data,
  case gen_tcp:send(Socket, Cmd) of
    {error, Reason} -> throw(Reason);
    _ -> ok
  end,

  case Op of
    put -> Ans = recv_ans(Socket);
    del -> Ans = recv_ans(Socket);
    stats -> case recv_ans_data(Socket) of
                {ok, Res} -> Ans = {ok, binary_to_list(Res)};
                MemError -> Ans = MemError
              end;
    get -> case recv_ans_data(Socket) of
            {ok, Res} -> Ans = {ok, binary_to_term(Res)};
            MemError -> Ans = MemError
        end
  end,
  Ans.

client(Socket) ->
    receive
      {req, Pid, Data} ->
        try process_request(Socket, Data) of
          Ans -> Pid ! {ans, Ans},
          client(Socket)
        catch
          throw:_ -> close_conn(Socket)
        end;
      exit -> close_conn(Socket)
    end.

close_conn(Socket) ->
    io:fwrite("Conexión terminada~n"),
    gen_tcp:close(Socket),
    closed.

put(Id, Key, Value) ->
    Bkey = term_to_binary(Key),
    Bval = term_to_binary(Value),
    Klen = byte_size(Bkey),
    Vlen = byte_size(Bval),
    Cmd = <<?PUT:8,Klen:32/big,Bkey/binary,Vlen:32/big,Bval/binary>>,
    Id ! {req, self(), {put, Cmd}},
    ?WAIT_RES.

get(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?GET:8,Klen:32/big,Bkey/binary>>,
    Id ! {req, self(), {get, Cmd}},
    ?WAIT_RES.

del(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?DEL:8,Klen:32/big,Bkey/binary>>,
    Id ! {req, self(), {del, Cmd}},
    ?WAIT_RES.

stats(Id) ->
    Cmd = <<?STATS:8>>,
    Id ! {req, self(), {stats, Cmd}},
    ?WAIT_RES.

exit(Id) ->
    Id ! exit,
    closed.
