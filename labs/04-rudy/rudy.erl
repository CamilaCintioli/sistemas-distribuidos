-module(rudy).
-export([init/1]).
-import(gen_tcp,[listen/2, accept/1, close/1, recv/2]).
-import(http, [ok/1, parse_request/1]).

%toma un puerto, abre un socket en modo escucha y pasa el socket a handler/1.
%Cuando el request termina, el socket se cierra.
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            %Area de trabajo:
          io:format("init: listen: ~w~n", [Listen]),
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
    {error, Error} ->
      io:format("init: error: ~w~n", [Error]),
        error
    end.

%cuando el cliente se conecta pasa la conexion a request/1.
%dsp se cierra conexion.
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            %area de trabajo
            request(Client),
            ok;
        {error, Error} ->
          io:format("handler: error: ~w~n", [Error]),
          error
    end.

%lee el req y la parsea. pasa la request al reply/1. envia la respuesta al cliente.
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            %Area de trabajo:
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("request: error: ~w~n", [Error]),
            error
    end,
    gen_tcp:close(Client).

%que responde
reply({{get, URI, _}, _, Body}) ->
    http:ok(Body).

% ///---------------------------------------------------------

% gen_tcp:listen(Port, Option): así es como el server abre un socket
% en modo escucha. Pasamos el numero de puerto como argumento y usamos
% la siguiente lista de opciones:
% [{active, false}, {reuseaddr, true}]. Al usar estas opciones
% veremos los bytes como una lista de enteros en lugar de una
% estructura binaria. Tendremos que leer la entrada usando recv/2 en
% lugar de enviárnosla como mensajes. La dirección del puerto deberá
% ser usada una y otra vez.


% gen_tcp:accept(Listen): Así es como aceptamos un request de
% entrada. Si es exitoso, tendremos un canal de comunicación abierto
% con el cliente.


% gen_tcp:recv(Client, 0): Una vez que tengamos conexión con el
% cliente leeremos la entrada y la retornaremos como un string. El
% argumento 0, dice al sistema que lea todo lo posible.


% gen_tcp:send(Client, Reply): Así es como devolvemos la respuesta,
% en forma de string, al cliente.


% gen_tcp:close(Socket): Una vez que terminamos, necesitamos cerrar
% la conexión. Notar también que necesitamos cerrar el socket de
% escucha que abrimos al principio.