# qwrap-clj

Turn clojure code to KDB+/Q code.

## Leiningen / Boot

```clojure
[qwrap-clj "0.1.0"]
```

## Usage

```clojure
(require ['qwrap.q :refer :all])
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
```


