# Opty 

El objetivo del ejercicio es implementar un servidor de transacciones utilizando concurrencia optimista. 

## Handler

Comenzamos el ejercicio implementando el handler, ya que implementa la lógica para manejar los pedidos de 
escritura y lectura. 
Para manejar el pedido de lectura, decidimos que en el caso de que se encuentre un registro de escritura en la posicion N del store, 
simplemente devolveríamos el valor escrito al cliente. Tardamos más en completar el caso en donde no se encuentra, 
y nos dimos cuenta que si no ha sido escrito, deberíamos buscar la entrada en el store (con store:lookup) y mandarle a 
la entrada un mensaje de lectura que retorna el valor.
```
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

```
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

```benchmark:start(StoreSize, ClientCount, ReadCount, WriteCount)```

Cada cliente inicializado realizará las escrituras y lecturas en un orden aleatorio en una posición aleatoria del store y hará un commit. Con los datos provistos por el benchmark pudimos llegar a varias conclusiones.  

La manera en que el servidor performa es variante y el tiempo en responder suele aumentar con una mayor cantidad de clientes. Realizamos pruebas con 1000 transacciones donde el tiempo de respusta máximo fue 0,333227 segundos, mientras que pruebas con 5000 transacciones el tiempo de respuesta aumentaba a  13 segundos aproximadamente.  

En cuanto a la tasa de éxito de las transacciones, es muy dependiente del tamaño del store y las operaciones de lectura y escritura que son realizadas. Una de las pruebas que realizamos fue con un tamaño de 5000 en el store y 100 clientes, haciendo 50 lecturas y escrituras tuvimos 84 commits realizados exitosamente y 16 abortados pero realizando la misma prueba con un store de 50, 91 commits fueron abortados.  
 

