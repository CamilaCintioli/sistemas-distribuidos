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

Hicimos un client como es recomendado para realizar unos tests. El cliente puede recibir los mensajes:
- {read, N} 
- {write, N, Value}
- commit
- abort
