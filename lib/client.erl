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
end).

-define(RECV(Socket, Len),
case gen_tcp:recv(Socket, Len) of
    {error, Reason} -> throw(Reason);
	Data -> Data
end).

-define(WAIT_RES,
receive
    RES -> RES
end).

start(Hostname, IsPriv) ->
    if
        IsPriv -> 
            Port = 889;
        true ->  
            Port = 8889
    end,
    spawn(fun() ->
        %activo
        %{ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active,true},binary]),
        %pasivo
        % TENEMOS QUE CAPTURAR ERROR DE CONEXION
        {ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active,false},binary]),
        worker_thread(Socket)
    end).

worker_thread(Socket) ->
    % -------------------- PASIVO ---------------------
    receive   % TODO Refactorizar
        {put, Pid, Cmd} -> ?SEND(Socket, Cmd),
                           Res = recv_ans(Socket),
	                       Pid ! Res,
	                       worker_thread(Socket);
        {del, Pid, Cmd} -> ?SEND(Socket, Cmd),
                           Res = recv_ans(Socket),
	                       Pid ! Res,
	                       worker_thread(Socket);
        {stats, Pid, Cmd} -> ?SEND(Socket, Cmd),
                             Res = rcv_ans_data(Socket),                                        
	                         Pid ! Res,
	                         worker_thread(Socket);
        {get, Pid, Cmd} -> ?SEND(Socket, Cmd),
                           Res = rcv_ans_data(Socket),
	                       Pid ! Res,
	                       worker_thread(Socket);
        exit -> close_conn(Socket)
    end.

    % ------------------ ACTIVO -----------------
    % receive
    %     {tcp, Socket, Data} -> 
    %         parse_data(Data, Socket);
    %     {send, Cmd} -> case gen_tcp:send(Socket, Cmd) of
    %                         ok -> io:fwrite("Enviado!~n");
    %                         {error, Reason} -> io:fwrite("Error: ~s~n", [Reason])
    %                     end,
    %                     worker_thread(Socket);
    %     {tcp_closed, _} -> io:fwrite("Conexion terminada~n"),
    %                        gen_tcp:close(Socket);
    %     exit -> gen_tcp:close(Socket)
    % end.

close_conn(Socket) ->
    io:fwrite("Conexión terminada"),
    gen_tcp:close(Socket).

put(Id, Key, Value) ->
    Bkey = term_to_binary(Key),
    Bval = term_to_binary(Value),
    Klen = byte_size(Bkey),
    Vlen = byte_size(Bval),
    Cmd = <<?PUT:8,Klen:32/big,Bkey/binary,Vlen:32/big,Bval/binary>>,
    Id ! {put, Cmd},
    ?WAIT_RES.

get(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?GET:8,Klen:32/big,Bkey/binary>>,
    Id ! {get, Cmd},
    ?WAIT_RES.

del(Id, Key) ->
    Bkey = term_to_binary(Key),
    Klen = byte_size(Bkey),
    Cmd = <<?DEL:8,Klen:32/big,Bkey/binary>>,
    Id ! {del, Cmd},
    ?WAIT_RES.

stats(Id) ->
    Cmd = <<?STATS:8>>,
    Id ! {stats, Cmd},
    ?WAIT_RES.

exit(Id) ->
    Id ! exit.

recv_ans(Socket) ->
	Data = RECV(Socket, 1),
	case Data of
	    <<?OK>>        -> ok;
	    <<?EINVALID>>  -> einvalid;
	    <<?ENOTFOUND>> -> enotfound;
	    <<?EBINARY>>   -> ebinary;
	    <<?EBIG>>      -> ebig;
	    <<?EUNK>>      -> eunk;
	    <<?EOOM>>      -> eoom;
	    _             -> io:fwrite("Respuesta desconocida~n"),
	                     error
	end;

recv_ans_data(Socket) ->
    case recv_ans(Socket) of
        ok ->  case gen_tcp:recv(Socket,4) of
                   {ok, <<Len:32/big-endian>>} ->
				       case gen_tcp:recv(Socket, Len) of
					       {ok, Data} -> Data;
                           {error, Reason} -> 
                {error, Reason} -> 
        Error -> Error
    end.

