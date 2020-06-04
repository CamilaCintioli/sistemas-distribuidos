# Loggy

## Modulo time

El modulo time nos permite realizar el seguimiento de los tiempos lógicos de cada worker.

### Primera implementación


En cuanto a la implementación del modulo time, tomamos los números como una representación de tiempo bastante sencilla para trabajar.

En la primera implementacion del logger se registraban los logs a medida que el logger los recibia, podia darse el caso donde un worker loggeaba recibir un mensaje antes que el remitente pudiera registrar que lo envió.
Todos los mensajes enviados por los workers estaban identificados por lo cual era fácil detectar el orden erroneo en los registros.


### Segunda implementación

En la segunda implementación, el logger continua loggeando los eventos del worker, y ahora inicializa una cola de prioridad donde registra los eventos a loggear si no es seguro loggearlos aún y un reloj.

```erlang
init(Nodes) ->
    Clock = time:clock(Nodes),
    Queue = pqueue:new(),
    loop(Clock, Queue).
```

 Cuando el logger recibe el mensaje de log, se encola el remitente, el mensaje y el tiempo en que fue realizado, hace un update del reloj y si hay mensajes que sean seguros de loggear, imprime todos los mensajes encolados previos a ese tiempo.

```erlang
{log, From, Time, Msg} ->
            Queue2 = pqueue:in(Time, {From, Time, Msg}, Queue),
            Clock2 = time:update(From, Time, Clock),
            case time:safe(Time,Clock2) of
                true ->
                    Queue3 = log_up_to(Time, Queue2),
                    loop(Clock2, Queue3);
                false -> 
                    loop(Clock2, Queue2)
            end;

```

Al momento de realizar el log, se desencolan los mensajes seguros y efectivamente se loggean ordenados por tiempo lógico.

En cuanto al modulo ```time```, implementamos el clock con una lista donde guardamos tuplas con el nombre del nodo y del tiempo actual en ese nodo. Al inicializarlo, los tiempos comienzan en 0 y son actualizados cuando reciben eventos para loggear de un worker. Llevar registro del tiempo actual de un nodo en particular nos permite saber cuando es seguro loggear un evento, y esto lo resolvemos con safe donde es seguro loggear si el tiempo dado es mayor a cada uno de los tiempos.
