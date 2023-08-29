-module(client).
-export([start/2,put/3,del/2,get/2,stats/1,exit/1]).

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

-define(SEND(Socket, Data),
case gen_tcp:send(Socket, Data) of
    {error, Reason} -> throw(Reason);
    _ -> ok
end).

-define(RECV(Socket, Len),
case gen_tcp:recv(Socket, Len) of
    {error, Reason} -> throw(Reason);
    {ok, Data} -> Data
end).

-define(WAIT_RES,
receive
    {ans, RES} -> RES
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
                  {ok, <<Len:32/big>>} -> ?RECV(Socket, Len);
                  {error, Reason} -> {error, Reason}
              end;
        Error -> Error
    end.

start(Hostname, IsPriv) ->
    if
        IsPriv -> 
            Port = 889;
        true ->  
            Port = 8889
    end,
    spawn(fun() ->
        % TENEMOS QUE CAPTURAR ERROR DE CONEXION
        {ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active, false},binary]),
        worker_thread(Socket)
    end).

process_request(Socket) ->
  receive   % TODO Refactorizar
    {put, Pid, Cmd} -> ?SEND(Socket, Cmd),
                      Res = recv_ans(Socket),
                      Pid ! {ans, Res};
    {del, Pid, Cmd} -> ?SEND(Socket, Cmd),
                      Res = recv_ans(Socket),
                      Pid ! {ans, Res};
    {stats, Pid, Cmd} -> ?SEND(Socket, Cmd),
                        Res = recv_ans_data(Socket),                                        
                        Pid ! {ans, Res};
    {get, Pid, Cmd} -> ?SEND(Socket, Cmd),
                        case recv_ans_data(Socket) of
                            {error, Error} -> Res = Error;
                            Data -> Res = binary_to_term(Data)
                        end,
                        Pid ! {ans, Res};
    exit -> throw(closed)
  end.

worker_thread(Socket) ->
    try process_request(Socket) of
      _ -> worker_thread(Socket)
    catch
      throw:_ -> close_conn(Socket)
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
    Id ! {put, self(), Cmd},
    ?WAIT_RES.

get(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?GET:8,Klen:32/big,Bkey/binary>>,
    Id ! {get, self(), Cmd},
    ?WAIT_RES.

del(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?DEL:8,Klen:32/big,Bkey/binary>>,
    Id ! {del, self(), Cmd},
    ?WAIT_RES.

stats(Id) ->
    Cmd = <<?STATS:8>>,
    Id ! {stats, self(), Cmd},
    ?WAIT_RES.

exit(Id) ->
    Id ! exit.
