# Rudy

### La API de socket


### Estructura de proceso de servidor
thread escuchando socket, cuando recibe llamada spawnea thread para manejarla y seguir escuchando el socket y maximiza disponibilidad del servicio.

### Protocolo HTTP

#### Incrementando el rendimiento
Agregamos un spawn.



-problem! fun import in spawn.
#### Benchmark
problema -> esperar que terminen los requests. 
Usamos otra herramienta para medir la performance, locust.
-corrimos sin spawn
(img sin spwan -- )
Andaba todo joya hasta que pasaron cosas.

-corrimos con spawn
(img con spawn -- semi descrip)
No importa can de usuarios, constante.

#### Parseo de HTTP
-agregamos post!
-si mandamos request muy largo se rompé.

#### Devolviendo archivos

```erlang
reply({{get, URI, _}, _, Body}) ->
    case file:read_file("./public" ++ URI) of
        {ok, Binary} -> http:ok(Binary);
        {error, enoent} -> http:not_found("no encontrado");
        {error, _} -> http:internal_server_error("se rompio")
    end;
```

#### Robustez
-no dejar colgado al client y cerrar socket.
agregamos un try catch en la fun de request para cerrar el socket en caso de errores.
