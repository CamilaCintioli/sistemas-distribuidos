-module(rudy).
-export([init/1, request/1]).

%toma un puerto, abre un socket en modo escucha y pasa el socket a handler/1.
%Cuando el request termina, el socket se cierra.
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            %Area de trabajo:
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
    {error, Error} ->
        error
    end.

%cuando el cliente se conecta pasa la conexion a request/1.
%dsp se cierra conexion.
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            %area de trabajo
            spawn(rudy, request, [Client]),
            %request(Client),
            ok;
        {error, Error} ->
          error
    end,
handler(Listen).

%lee el req y la parsea. pasa la request al reply/1. envia la respuesta al cliente.
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            %Area de trabajo:
            try http:parse_request(Str) of
                Request -> gen_tcp:send(Client, reply(Request))
            catch
                _:_  -> gen_tcp:send(Client, http:internal_server_error("Algo salio mal"))
            end;
        {error, Error} ->
            error
    end,
    gen_tcp:close(Client).

%que responde
reply({{get, URI, _}, _, Body}) ->
    case file:read_file("./public" ++ URI) of
        {ok, Binary} -> http:ok(Binary);
        {error, enoent} -> http:not_found("no encontrado");
        {error, _} -> http:internal_server_error("se rompio")
    end;
reply({{post,URI,_},_,Body}) -> http:ok(Body).

