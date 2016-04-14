# qwrap-clj

Turn clojure code to KDB+/Q code.

## Leiningen / Boot

Not deployed to clojars yet. You should build it yourself:

The dependency:
```clojure
[qwrap-clj "0.1.0-SNAPSHOT"]
```

To build it:
```sh
lein jar

# or
boot build
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
(q-def L [1])
; -> (enlist 1)

(q-def L [1 2 3])           
; -> (1;2;3) 

(q-def D {a [1 2 3]
          b [:a :b :c]})
; -> ((1;2;3) ! {`a;`b;`c})
```

Note: a literal vector is converted to Q list. a literal map is converted to Q dict.

### q-let

Define in local scope.

```clojure
(q-let [a [1 2 3]
        b [:a :b :c]]
  (q-concat a b))
  
; -> ({a:(1;2;3); b:(`a;`b;`c); (a , b)}[])  
```

### q-defn

Define a Q function.

```clojure
(q-defn my-concat [a b]
  (q-concat a b))
; -> my-concat: {[a b] (a , b)}  
  
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

List/Dict/Table lookup. The naming convention `*-in` means lookup in depth (the same as in clojure)
```clojure
(q-get list 1)          ; normal get, -> list[1]
(q-get list [1 2 3])    ; this is item-wise get, -> [ list[1] list[2] list[3] ]
(q-get-in list [1 2 3]) ; in depth get, -> list[1][2][3]
```

### q-assoc / q-assoc-in

Associate value to multiple keys (or a key path).

```clojure
(q-def L [[1 2] [10 20] [-10 -20])
(q-assoc L [1] [100 200])        ; normal set -> [[1 2] [100 200] [-10 -20]]
(q-assoc L [1 2] [100 200])      ; item-wise set -> [[1 2] [100 200] [100 200]]
(q-assoc-in L [1 1] 200)         ; in depth set -> [ [1 2] [10 200] [-10 -20] ]
```

### q-update /q-update-in

Update a variable.

```clojure
(q-update L [0 1] + 100)     ; item-wise update with + -> [[101 102] [110 120] [-10 -20]]
(q-update-in L [0 1] + 100)  ; in depth update with + -> [[1 102] [10 20] [-10 -20]]
```

### q-file

Q file construct.

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
(q-save-table tbl                        ; table variable
              (q-file "abc/def/")        ; target file or directory
              :splayed? true             ; splayed storage mode?
              :sympath (q-file "abc/")   ; symbol encoding directory (splayed mode only)
              :compress? true)           ; use compression?

; The above example saves to a splayed directory and encode symbols.
; -> (`:abc/def/;17;2;6) set .Q.en[`:abc/] tbl
```
