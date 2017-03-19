# sets

Benchmarks for set data structures: hash maps, FSA's, etc.

## Running

For all benchmarks:

    $ stack bench :space :time

For just space:

    $ stack bench :space

For just time:

    $ stack bench :time


<!-- RESULTS -->

## Insert Int (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Set|560.8 ns|17.99 μs|226.3 μs|7.468 ms|
|Data.HashSet|708.5 ns|10.07 μs|170.2 μs|7.551 ms|
|Data.IntSet|89.72 ns|1.115 μs|20.08 μs|0.456 ms|

## Intersection (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|1600 ns|17.16 μs|303.4 μs|2.557 ms|28.62 ms|214.3 ms|
|Data.HashSet|652.1 ns|4.390 μs|72.35 μs|0.961 ms|19.75 ms|235.1 ms|
|Data.IntSet|129.1 ns|0.958 μs|12.51 μs|0.258 ms|3.963 ms|39.44 ms|

## Member Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|36.61 ns|37.87 ns|50.35 ns|75.33 ns|95.42 ns|101.2 ns|
|Data.HashSet|67.57 ns|79.16 ns|43.81 ns|75.46 ns|115.9 ns|112.7 ns|
|Data.IntSet|38.12 ns|60.88 ns|76.85 ns|88.57 ns|107.6 ns|99.76 ns|

## Member String (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|275.3 ns|386.9 ns|445.1 ns|722.7 ns|841.2 ns|909.7 ns|
|Data.HashSet|320.5 ns|369.1 ns|316.3 ns|352.8 ns|426.8 ns|473.2 ns|
|Data.DAWG.Packed|994.1 ns|1379 ns|1321 ns|1739 ns|1510 ns|61.00 ns|

## FromList String (Monotonic)

|Name|10000|
|---|---|
|Data.Set|12.88 ms|
|Data.HashSet|7.692 ms|
|Data.DAWG.Packed|7.835 ms|

## FromList String (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Set|2.937 μs|45.42 μs|0.790 ms|20.70 ms|
|Data.HashSet|3.559 μs|47.80 μs|0.677 ms|19.04 ms|
|Data.DAWG.Packed|125.7 μs|2175 μs|30.85 ms|423.2 ms|

