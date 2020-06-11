# Muty

## Testing lock1

### Test1 (lock1, 1000, 1000)

sleep 1000, work 1000

```bash
John: 11 locks taken, average of 763.2594545454546 ms, 0 deadlock
Ringo: 10 locks taken, average of 904.0633 ms, 0 deadlock
Paul: 9 locks taken, average of 947.9736666666666 ms, 0 deadlock
George: 11 locks taken, average of 683.065909090909 ms, 0 deadlock
```

sleep 1000, work 5000

```bash
John: 3 locks taken, average of 2473.7273333333333 ms, 4 deadlock
Ringo: 5 locks taken, average of 2625.318 ms, 3 deadlock
Paul: 4 locks taken, average of 2884.352 ms, 2 deadlmock
George: 5 locks taken, average of 2187.6564 ms, 3 deadlock
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

## Testing lock3

### Test1 (lock3, )

500,6000
George: 7 locks taken, average of 2291.775714285714 ms, 9 deadlock
John: 7 locks taken, average of 3265.771285714286 ms, 8 deadlock
Paul: 7 locks taken, average of 3013.0517142857143 ms, 9 deadlock
Ringo: 7 locks taken, average of 2816.5371428571425 ms, 10 deadlock