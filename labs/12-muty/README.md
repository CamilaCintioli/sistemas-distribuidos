# Muty

## Testing lock1

### Test1 (lock1, 1000, 1000)

sleep 1000, work 1000

```bash
John:   11  locks taken, average of 763.25 ms, 0 deadlock
Ringo:  10  locks taken, average of 904.06 ms, 0 deadlock
Paul:   9   locks taken, average of 947.97 ms, 0 deadlock
George: 11  locks taken, average of 683.06 ms, 0 deadlock
```

### Test2 (lock1, 1000, 5000)

```bash
John:   3   locks taken, average of 2473.72 ms, 4 deadlock
Ringo:  5   locks taken, average of 2625.31 ms, 3 deadlock
Paul:   4   locks taken, average of 2884.35 ms, 2 deadlock
George: 5   locks taken, average of 2187.65 ms, 3 deadlock
```

### Test3 (lock1, 5000, 1000)

```bash
John:   14  locks taken, average of 150.94 ms, 0 deadlock
Ringo:  13  locks taken, average of 200.98 ms, 0 deadlock
Paul:   12  locks taken, average of 192.93 ms, 0 deadlock
George: 14  locks taken, average of 232.94 ms, 0 deadlock
```

## Testing lock2

### Test1 (lock2, 1000, 1000)

Probamos con sleep 1000, work 1000 - notamos que el worker John tiene el lock 1 con la prioridad mas alta y esto genero los siguientes resultados:

```bash
John:   35  locks taken, average of 362.39 ms, 0 deadlock
Ringo:  30  locks taken, average of 456.63 ms, 0 deadlock
Paul:   26  locks taken, average of 646.77 ms, 0 deadlock
George: 27  locks taken, average of 742.90 ms, 0 deadlock
```

### Test2 (lock2, 1000, 5000)

```bash
John:   16  locks taken, average of 1817.26 ms, 0 deadlock
Ringo:  13  locks taken, average of 2405.26 ms, 1 deadlock
Paul:   12  locks taken, average of 2376.12 ms, 1 deadlock
George: 8   locks taken, average of 2167.60 ms, 6 deadlock
```

### Test3 (lock2,5000,1000)

```bash
John:   16  locks taken, average of 163.65 ms, 0 deadlock
Ringo:  14  locks taken, average of 210.79 ms, 0 deadlock
Paul:   13  locks taken, average of 210.36 ms, 0 deadlock
George: 17  locks taken, average of 293.72 ms, 0 deadlock
```

## Testing lock3

### Test1 (lock3, 1000, 1000)

```bash
John:   21  locks taken, average of 939.21 ms, 0 deadlock
Ringo:  20  locks taken, average of 956.72 ms, 0 deadlock
Paul:   18  locks taken, average of 1029.32 ms, 0 deadlock
George: 20  locks taken, average of 1027.79 ms, 0 deadlock
```

### Test2 (lock3, 1000, 5000)

```bash
John:   5   locks taken, average of 2632.97 ms, 6 deadlock
Ringo:  6   locks taken, average of 2600.39 ms, 5 deadlock
Paul:   6   locks taken, average of 2411.23 ms, 6 deadlock
George: 6   locks taken, average of 1865.94 ms, 6 deadlock
```

### Test3 (lock3, 5000, 1000)

```bash
John:   16  locks taken, average of 220.16 ms, 0 deadlock
Ringo:  14  locks taken, average of 283.58 ms, 0 deadlock
Paul:   14  locks taken, average of 195.87 ms, 0 deadlock
George: 18  locks taken, average of 327.62 ms, 0 deadlock
```

## Conclusiones

- A menor tiempo de reposo, mayor cantidad de intentos de obtener el recurso compartido.
- A mayor tiempo de trabajo, existen mas chances de producirse un `deadlock`.
- `lock2` previene casos de `deadlock` al desempatar la obtención del recurso compartido en función de una prioridad arbitraria asignada a cada consumidor.
- `lock3` permite una distribución mas equitativa del recurso compartido, ya que tiene en consideración la cantidad de veces que lo fué asignado a cada consumidor.
