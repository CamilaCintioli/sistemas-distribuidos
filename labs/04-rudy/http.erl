-module(http).
-export([parse_request/1, ok/1, get/1, internal_server_error/1, not_found/1]).

message_body(R) ->
    {R, []}.

headers([13,10|R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.

header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

request_uri([32|R0])->
    {[], R0};
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.

request_line([$G, $E, $T, 32 |R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{get, URI, Ver}, R3};
request_line([$P,$O,$S,$T,32|R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{post, URI, Ver}, R3}.


parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

internal_server_error(Body) ->
    "HTTP/1.1 500 Internal Server Error\r\n" ++ "\r\n" ++ Body.

not_found(Body) ->
    "HTTP/1.1 404 Not Found\r\n" ++ "\r\n" ++ Body.

% Para lograr eso
% necesitaremos parsear la URI y serparar el path y el nombre de archivo
% de un posible query o índice. Una vez que tengamos el nombre de archivo
% a mano, necesitamos construir una header de respuesta apropiado
% conteniendo el tamaño, tipo y condiciones de encoding.