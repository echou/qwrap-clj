# qwrap-clj

Turn clojure code to KDB+/Q code.

## Leiningen / Boot

```clojure
[qwrap-clj "0.1.0"]
```

## Usage

```clojure
(require ['qwrap.core :refer :all])
```
It provides many `q-*` macros similar to clojure counter-parts. The two entrypoints are:

* `q` - convert all the `q-*` constructs to one KDB+/Q code string.
* `run-q` - connect to a Q server and execute `q`-converted string to the server. The result is converted to clojure data structures.

For example,

```clojure
(q (q-let [a [1 2 3]
           b [:a :b :c]]
      (q-table {a a} {b b})))
```
Generates the following Q code:

```q
({a:(1;2;3);b:(`a;`b;`c);([a:a] b:b)}[])
```

And 

```clojure
(run-q ["" 6001] (q- ...))
```

sends q code to Q server on `localhost:6001`. (You starts Q process with `q -p 6001`)

## Q Functions

### q-def

Define a Q variable.

```clojure
(q-def a [1 2 3])
```

### q-let

Local scope.

```clojure
(q-let [a [1 2 3]
        b [:a :b :c]]
  (q-concat a b))
```

### q-defn

Define a Q function.

```clojure
(q-defn my-concat [a b]
  (q-concat a b))
```

### q-if

If-else expression.

```clojure
(q-if (= a b) (+ a 1) (- a 1))
```

### q-cond
Multiple conditions.

```clojure
(q-cond
  (= a b) a
  :else b)
```

### q-get / q-get-in

List/Dict/Table lookup.
```clojure
(q-get list 1)
(q-get list [1 2 3])    ;-> [ list[1] list[2] list[3] ]
(q-get-in list [1 2 3]) ;-> list[1][2][3]
```

### q-assoc / q-assoc-in

Associate value to multiple keys (or a key path).

```clojure
(q-def L [[1 2] [10 20] [-10 -20])
(q-assoc L [1 2] [100 200])      ;-> [[1 2] [100 200] [100 200]]
(q-assoc-in L [1 1] 200) ; -> [ [1 2] [10 200] [-10 -20] ]
```

### q-file

Generate a Q file construct

```clojure
(q-file "abc.csv") ; -> `:abc.csv
```

### q-load-csv

Load csv file.

```clojure
; (q-load-csv csv-file types)

(q-load-csv (q-file "abc.csv") [:double :int :date :symbol])
```

### q-save-table

Save table to a file or a splayed directory.

```clojure
(q-save-table tbl (q-file "abc.csv"))  ; save to a file
(q-save-table tbl (q-file "abc/def/")
              :sympath (q-file "abc/")
              :splayed? true
              :compress? true)         ; save to a splayed directory and encode symbols.
                                       ; -> (`:abc/def/;17;2;6) set .Q.en[`:abc/] tbl
```