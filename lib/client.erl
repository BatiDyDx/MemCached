-module(client).
-export([start/2,put/3,del/2,get/2,stats/1,exit/1]).

-define(PUT, 11).
-define(DEL, 12).
-define(GET, 13).
-define(STATS, 21).

start(Hostname, IsPriv) ->
    if
        IsPriv -> 
            Port = 889;
        true ->  
            Port = 8889
    end,
    spawn(fun() ->
        {ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active,true}]),
        worker_thread(Socket)
    end).

worker_thread(Socket) ->
    receive
        {tcp, Socket, Data} -> 
            io:fwrite("> ~s~n",[Data]),
            worker_thread(Socket);
        {send, Cmd} -> case gen_tcp:send(Socket, Cmd) of
                            ok -> io:fwrite("Enviado!~n");
                            {error, Reason} -> io:fwrite("Error: ~s~n", [Reason])
                        end,
                        worker_thread(Socket);
        {tcp_closed, _} -> io:fwrite("Conexion terminada~n"),
                           gen_tcp:close(Socket);
        exit -> gen_tcp:close(Socket)
    end.

put(Id, Key, Value) ->
    Bkey = term_to_binary(Key),
    Bval = term_to_binary(Value),
    Klen = byte_size(Bkey),
    Vlen = byte_size(Bval),
    Cmd = <<?PUT:8,Klen:32/big,Bkey/binary,Vlen:32/big,Bval/binary>>,
    Id ! {send, Cmd}.

get(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?GET:8,Klen:32/big,Bkey/binary>>,
    Id ! {send, Cmd}.

del(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?DEL:8,Klen:32/big,Bkey/binary>>,
    Id ! {send, Cmd}.

stats(Id) ->
    Cmd = <<?STATS:8>>,
    Id ! {send, Cmd}.

exit(Id) ->
    Id ! exit.

