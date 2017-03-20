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
|Data.Set|557.3 ns|17.21 μs|329.0 μs|7.569 ms|
|Data.HashSet|1270 ns|14.56 μs|213.8 μs|7.279 ms|
|Data.IntSet|106.0 ns|1.170 μs|40.78 μs|0.491 ms|

## Intersection (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|1685 ns|18.46 μs|218.9 μs|3.357 ms|29.93 ms|241.3 ms|
|Data.HashSet|640.3 ns|6.534 μs|94.70 μs|0.854 ms|21.01 ms|267.1 ms|
|Data.IntSet|167.8 ns|0.947 μs|18.38 μs|0.256 ms|4.604 ms|42.46 ms|

## Member Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|40.38 ns|37.35 ns|47.81 ns|70.97 ns|90.09 ns|117.4 ns|
|Data.HashSet|124.5 ns|90.98 ns|49.75 ns|56.03 ns|106.6 ns|138.7 ns|
|Data.IntSet|45.48 ns|55.69 ns|67.94 ns|73.28 ns|88.82 ns|136.2 ns|

## Member Int (Randomized, false positive rate 0.1)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|30.03 ns|54.31 ns|47.46 ns|74.55 ns|110.1 ns|93.38 ns|
|Data.HashSet|77.05 ns|73.74 ns|58.66 ns|52.13 ns|109.2 ns|104.3 ns|
|Data.IntSet|44.29 ns|57.61 ns|102.8 ns|85.83 ns|79.70 ns|100.9 ns|
|Data.BloomFilter|326.7 ns|214.6 ns|247.4 ns|183.4 ns|219.0 ns|224.8 ns|

## Member String (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|260.0 ns|336.3 ns|625.2 ns|674.7 ns|744.7 ns|1053 ns|
|Data.HashSet|280.0 ns|347.6 ns|420.2 ns|404.1 ns|457.4 ns|421.2 ns|
|Data.DAWG.Packed|1326 ns|1896 ns|1212 ns|1592 ns|1373 ns|76.12 ns|

## Member String (Randomized, false positive rate 0.1)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Set|252.0 ns|404.0 ns|341.0 ns|670.3 ns|1079 ns|938.6 ns|
|Data.HashSet|530.4 ns|344.0 ns|352.6 ns|406.8 ns|409.9 ns|464.7 ns|
|Data.DAWG.Packed|1627 ns|1591 ns|1447 ns|1597 ns|1192 ns|75.21 ns|
|Data.BloomFilter|858.9 ns|911.3 ns|699.7 ns|1110 ns|821.2 ns|804.4 ns|

## FromList String (Monotonic)

|Name|10000|
|---|---|
|Data.Set|14.40 ms|
|Data.HashSet|11.22 ms|
|Data.DAWG.Packed|7.570 ms|

## FromList String (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Set|3.238 μs|49.43 μs|0.847 ms|21.40 ms|
|Data.HashSet|3.524 μs|44.44 μs|0.575 ms|17.99 ms|
|Data.DAWG.Packed|117.5 μs|2111 μs|31.65 ms|411.0 ms|

