# Detector

## Testeando con dos nodos

### En un mismo host
Levantamos dos nodos en el mismo host, silver y gold. Pudimos levantar exitosamente el producer en uno y el consumer en otro y realizar las pruebas sin problemas. Hasta el momento, todo funcionaba como era esperado.  

Probamos crashear el producer y obtuvimos:  
```{producer,'silver@host'} died; {badarith,[{producer,producer,3,[{file,'producer.erl'},{line,23}]}]}```  

Y probamos matar el nodo silver donde se estaba corriendo el producer obteniendo:  
```{producer,'silver@host'} died; noconnection```   

Ambos mensajes se dan porque el monitor observando al proceso del producer le avisá al consumer si el proceso observado, en este caso el productor, se cae , enviando el mensaje 'DOWN' e información de por qué el proceso murió. Esto puede verse en el siguiente caso dentro de la función consume.

```erl 
{'DOWN', Monitor, process, Object, Info} ->
        io:format("~w died; ~w~n", [Object, Info]),
        consume(M, Monitor);
```

En el caso en el que el producer crashea, se puede ver que la rázon dada es la de badarith al ejecutarse el 42/0 en la línea 23.

### En diferentes hosts
Arrancamos un nodo silver y un nodo gold en máquinas diferentes, utilizando la misma cookie y logramos comunicar el producer y el consumer.  
Comenzamos las pruebas mandándole el mensaje stop al producer y obtuvimos esto en el nodo con el consumidor:
```{producer,'silver@host'} died; normal```.  
Al momento de realizar más pruebas como matar al nodo y desconectar el cable de red de la máquina tuvimos un problema dónde no podíamos lograr la comunicación entre los nodos y obtuvimos:   
```{producer,'silver@host'} died; noconnection```
