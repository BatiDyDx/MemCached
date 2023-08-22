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
        %activo
        %{ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active,true},binary]),
        %pasivo
        % TENEMOS QUE CAPTURAR ERROR DE CONEXION
        {ok, Socket} = gen_tcp:connect(Hostname,Port,[{reuseaddr, true},{active,false},binary]),
        worker_thread(Socket)
    end).

worker_thread(Socket) ->
    % -------------------- PASIVO ---------------------
    receive
        {send, Cmd} -> case gen_tcp:send(Socket, Cmd) of
                            ok -> get_ans(Socket, false);
                            {error, Reason} -> io:fwrite("Error: ~s~n", [Reason])
                        end,
                        worker_thread(Socket);
        {send_stats, Cmd} -> case gen_tcp:send(Socket, Cmd) of
                                ok -> get_ans(Socket, true);
                                {error, Reason} -> io:fwrite("Error: ~s~n", [Reason])
                            end;
        {tcp_closed, _} -> close_conn(Socket);
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

get_ans(Socket, Stats) ->
    case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
        parse_data(Data, Stats);
    {error, closed} ->
        close_conn(Socket)
    end,
    worker_thread(Socket).

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
    Id ! {send_stats, Cmd}.

exit(Id) ->
    Id ! exit.

parse_data(Data, Stats) ->
    Nbyte = byte_size(Data),
    io:fwrite("byte data ~p~n",[Nbyte]),
    case Data of 
        <<101,_:32,Rest/binary>> ->
            case Rest of
                <<>> -> io:fwrite("ok vacio~n");
                _ -> if 
                       Stats -> SVal = Rest;
                       true -> SVal = binary_to_term(Rest)
                     end,
                     io:fwrite("OK ~p~n",[SVal])
            end;
        <<101,_:32,Stats>> ->
            io:fwrite("OK ~s~n",[Stats]);
        <<101>> ->
            io:fwrite("OK~n");
        <<111>> ->
            io:fwrite("EINVALID~n");
        <<112>> ->
            io:fwrite("ENOTFOUND~n");
        <<113>> ->
            io:fwrite("EBINARY~n");
        <<114>> ->
            io:fwrite("EBIG~n");
        <<115>> ->
            io:fwrite("EUNK~n");
        <<116>> ->
            io:fwrite("EOOM~n");
        _ ->
            io:fwrite("MESSAGE ERROR~n")
    end.