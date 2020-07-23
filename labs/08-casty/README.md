# Casty

Casty propone como objetivo construir una red de audio streaming.

## Arquitectura

![basic architecture](./readme_imgs/basic_architecture.svg "Basic architecture")


## ICY

## Client

## Proxy

## Testing

Siguiendo las pautas del enunciado, comenzamos a realizar un test usando el programa VLC para dirigirlos al stream iniciado en http://localhost:8080/. Pudimos observar que efectivamente el cliente recibe el request y se lo envía al proxy, que también lo recibe correctamente pero nos encontramos con el problema que la conexión con el servidor no podía ser establecida y luego con que el servidor esperaba un header en el request de ICY pero se obtenía uno de http.

## Dist

Una vez terminadas las pruebas iniciales con nuestros tres modulos base, implementamos el módulo ```dist``` cuyo objetivo es ser un intermediario entre el cliente y el proxy, ya que la solución previa solo podía conectarse un cliente por proxy y convenientemente queremos conectar varios clientes a el proceso dist y que el mismo sea el que se conecta al proxy.

Como podemos ver en el siguiente código, pudimos integrar en el proceso dist el ```multicast``` visto en trabajos previos, y lo usamos al recibir la data del proxy, que es reenviada a los clientes conectados.

```erlang
{data, N, Data} ->
multicast(Clients,{data, N, Data}),
loop(Clients, N+1, Context);
```

También manejamos requests de clientes que quieren conectarse al proxy y agregamos un monitor para poder dejar de enviar data innecesaria a clientes que estan muertos o desconectados.

```erlang
{request, From} ->
Ref = erlang:monitor(process, From),
From ! {reply, N, Context},
loop([{Ref, From}|Clients], N, Context);
```

![dist architecture](./readme_imgs/dist_architecture.svg "Architecture dist")

## Dummy

El dummy client es usado simplemente para realizar unos test ya que el distribuidor necesita un reproductor antes de conectarse al proxy.

## Tree

En la siguiente etapa del trabajo, intentamos construir un árbol distribuidor e introducimos los procesos ```root``` y ```branch```.

### Root

El proceso root es el encargado de conectarse al proxy y esperará a que sus branches se conecten eventualmente. Al igual que el proceso dist, se conectará a la proxy y hará un multicast a sus clientes con la data recibida del mismo.
La manera en que maneja los request de conexión de un cliente es diferente, al root sólo se conectaran dos clientes y si recibe una petición de un tercero, simplemente lo redirige a una de sus ramas. Tuvimos que alterar la rama a la cual es dirigido para no desbalancear el árbol.

``` erlang
loop(Clients, N, Context) ->
    receive
        {data,N,Data} ->
            multicast(Clients, {data, N, Data}),
            loop(Clients,N+1,Context);
        {request, From} ->
            L = length(Clients),
            if
              L < 2 ->
                From ! {reply, N, Context},
                loop([From|Clients], N+1, Context);
              true -> 
                [Left,Right] = Clients,
                From ! {redirect, Left},
                loop([Right,Left],N,Context)
            end
    end.
```

### Branch

El proceso branch es muy similar al root, sólo incluimos un pattern matching más en los mensajes recibidos:

```erlang
{redirect, NewProxy} ->
    connect(NewProxy, Client);
```


![dist architecture](./readme_imgs/tree_architecture.svg "Architecture tree")


## More testing!