# Opty 

El objetivo del ejercicio es implementar un servidor de transacciones utilizando concurrencia optimista. 

## Handler

Comenzamos el ejercicio implementando el handler, ya que implementa la lógica para manejar los pedidos de 
escritura y lectura. 
Para manejar el pedido de lectura, decidimos que en el caso de que se encuentre un registro de escritura en la posicion N del store, 
simplemente devolveríamos el valor escrito al cliente. Tardamos más en completar el caso en donde no se encuentra, 
y nos dimos cuenta que si no ha sido escrito, deberíamos buscar la entrada en el store (con store:lookup) y mandarle a 
la entrada un mensaje de lectura que retorna el valor.

```erlang
{read, Ref, N} ->
            case lists:keysearch(N, 1, Writes) of
                {value, {N, _, Value}} ->
                      Client ! {Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                      Pid = store:lookup(N, Store),
                      Pid ! {read, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes)
            end;

```

No tuvimos dificultades para manejar pedidos de escritura, sólo guardamos un registro de escritura en Writes con el índice
de la entrada en el store, la entrada y el valor a escribir.

```erlang
{write, N, Value} ->
            Entry = store:lookup(N, Store),
            Added = [{N, Entry, Value}|Writes],
            handler(Client, Validator, Store, Reads, Added);
```

## Server

Continuamos completando la implementación del servidor de transacciones haciendo que cuando el cliente pida abrir una
transacción, se devuelva el handler con el que debe interactuar para hacer los pedidos. 

## Validator

Para terminar la implementación del validator, completamos la función update(Writes) de la siguiente manera:

```
update(Writes) -> 
  lists:map(fun({_,Entry,Value}) -> 
    Entry ! {write,Value}
  end,
  Writes).
```

## Performance

### Client

Implementamos un client como es recomendado para realizar unos tests. El cliente puede recibir los mensajes:

- ```{read, N} ```  
- ```{write, N, Value}```
- ```commit```
- ```abort```

### Benchmark

Para realizar pruebas de carga implementamos un benchmark. El start del benchmark recibe el tamaño del store, una cantidad de clientes a iniciar, la cantidad de lecturas y escrituras que cada cliente deber realizar antes de commitear sus operaciones.  

```erlang
benchmark:start(StoreSize, ClientCount, ReadCount, WriteCount)
```

Cada cliente inicializado realizará las escrituras y lecturas en un orden aleatorio en una posición aleatoria del store y hará un commit. Con los datos provistos por el benchmark pudimos llegar a varias conclusiones.  

La manera en que el servidor performa es variante y el tiempo en responder suele aumentar con una mayor cantidad de clientes. Realizamos pruebas con 1000 transacciones donde el tiempo de respusta máximo fue 0,333227 segundos, mientras que pruebas con 5000 transacciones el tiempo de respuesta aumentaba a  13 segundos aproximadamente.  

En cuanto a la tasa de éxito de las transacciones, es muy dependiente del tamaño del store y las operaciones de lectura y escritura que son realizadas. Una de las pruebas que realizamos fue con un tamaño de 5000 en el store y 100 clientes, haciendo 50 lecturas y escrituras tuvimos 84 commits realizados exitosamente y 16 abortados pero realizando la misma prueba con un store de 50, 91 commits fueron abortados.  

## Conclusiones

### ¿Es realista la implementación del store que tenemos?

Esta implementación garantiza la exclusión mutua entre los clientes al momento de hacer `commit` de su transacción. Una optimización posible es restringir la exclusión entrada por entrada o por bloque de entradas. De esta forma, si múltiples clientes operan sobre distintas entradas no se bloquean mutuamente. Esta implementación sería más compleja, por lo cual su implentacion introduciría más casos bordes, reduciendo la confiabilidad.
Esta es una implementacion de un `Store` genérico, que no contempla las particularidades de cada problema. Un `Store` especializado podria contener las optimizaciones pertinentes a la problematica.

### ¿Qué rápido podemos operar sobre el store?

La taza de transacciones esta directamente vinculada con la cantidad de clientes concurrentes sobre el `Store`. Pero, según nuestras pruebas, con 100 clientes realizando 10 operaciones de escritura y 10 operaciones de lectura sobre posiciones aleatorias en un orden aleatorio encontramos una taza promedio de 3000 transacciones por segundo. Al incrementar la cantidad de clientes observamos un descenso de la taza. Con 1000 clientes observamos una taza promedio de 1500 transacciones por segundo.

### ¿Qué es lo que se copia cuando el handler de transacciones arranca?

Al abrir una transaccion el servidor envia al cliente los `Pids` de los procesos `Validator` y `Store`, para que pueda inicializar un `Handler` con estos valores en conjunto con el `Pid` del cliente.

### ¿Dónde corre el handler?

Los clientes al abrir conexión con el servidor inician un proceso vinculado que cumple el rol de `Handler`, como se ve en `handler:start/3`.

### ¿Cuáles son los pros y contras de la estrategia de implementación?

Cuando usas una estrategia con control de concurrencia optimista, todas las transacciones pueden proceder, pero algunas son abortadas cuando son commiteadas. Esto resulta en operaciones relativamente eficientes cuando hay pocos conflictos pero implican repetir trabajo cuando las transacciones son abortadas.
En casos donde existen muchos clientes o muchas entradas, otras implementaciones podrian resultar en mejor rendimiento.
