(ns qwrap.core
  (:require [clojure.string :as s]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]])
  (:import (clojure.lang IPersistentCollection Keyword Symbol LazySeq PersistentVector)
           (java.time LocalDate)
           (kx c c$Month c$Dict c$Flip)
           (java.sql Date)
           (java.time.format DateTimeFormatter)))

(defprotocol ToStr
  (to-str [this]))

(defprotocol ToQ
  (to-q [this]))


(def ^:private date-format (DateTimeFormatter/ofPattern "yyyy.MM.dd"))

(defn map-join [sep f & xs]
  (s/join sep (apply map f xs)))


(defn list-to-q [values must]
  (case (count values)
    0 "()"
    1 (if must
        (str "(enlist " (to-q (first values)) ")")
        (to-q (first values)))
    (str \( (map-join ";" to-q values) \))))


(extend-protocol ToStr
  nil
  (to-str [_] "")
  String
  (to-str [this] this)
  Keyword
  (to-str [this] (name this))
  Symbol
  (to-str [this] (name this))
  LazySeq
  (to-str [this] (str (seq this)))
  Object
  (to-str [this] (str this)))

;;;;;;;;

(extend-protocol ToQ
  nil
  (to-q [_] "0N")                                           ; TODO typed null
  Boolean
  (to-q [b] (if b "1b" "0b"))
  Keyword
  (to-q [k] (str \` (name k)))
  Symbol
  (to-q [k] (str (name k)))                                 ; keep the same
  String
  (to-q [s] (str \" s \"))                                  ; TODO escape double quote
  PersistentVector
  (to-q [s] (list-to-q s false))
  LocalDate
  (to-q [d] (.format d date-format))
  Number
  (to-q [n] n))

;;;;;;;;; kx/c response to clojure data structures

(defrecord qDict [key val])

(defn- array? [v]
  (-> v (class) (.isArray)))

(defn convert-array [v]
  (let [cc (.getComponentType (class v))]
    ;(println "array element type" (str cc))
    (case (str cc)
      "char" (String. v)
      (vec v))))

(defn convert-number [v]
  (condp = (class v)
    Long (when (not= Long/MIN_VALUE v) v)
    Integer (when (not= Integer/MIN_VALUE v) v)
    v))

(defn from-q
  "convert kx/c response to clojure data"
  [v]
  (let [f (fn [v]
            (cond
              (nil? v)
              nil

              (array? v)
              (convert-array v)

              (instance? c$Dict v)
              (->qDict (.x v) (.y v))

              (instance? c$Flip v)
              (zipmap (map keyword (seq (.x v)))            ; TODO keywordize
                      (map vec (seq (.y v))))

              (instance? java.sql.Date v)
              (.toLocalDate v)

              (instance? c$Month v)
              (if (#{2147483647 -2147483647} (.i v))        ; nil
                nil (str v))

              (number? v)
              (convert-number v)

              :else
              v))]

    (when (some? v)
      (walk/prewalk f v))))

;;;;;;;;;

; reference to a Q variable
(defrecord QRef [v]
  ToQ
  (to-q [_] (str \` (to-q v))))

; raw Q code
(defrecord QRaw [text]
  ToQ
  (to-q [_] text))

; Q List construct
(defrecord QList [values]
  ToQ
  (to-q [_] (list-to-q values false)))

; Q List (enlist wrapped when single element)
(defrecord QMustList [values]
  ToQ
  (to-q [_] (list-to-q values true)))

; Q Dict construct
(defrecord QDict [ks vs]
  ToQ
  (to-q [_]
    (str (to-q ks)
         " ! "
         (to-q vs))))

; Q Table construct
(defrecord QTable [kcols cols]
  ToQ
  (to-q [_]
    (str "([" (map-join ";" (fn [[k v]]
                              (str (to-q k) \: (to-q v))) kcols)
         "] " (map-join ";" (fn [[k v]]
                              (str (to-q k) \: (to-q v))) cols)
         ")")))


; List Indexing (width or depth)
(defrecord QIndex [v depth?]
  ToQ
  (to-q [_]
    (str "["
         (if (seq v)
           (map-join (if depth? ";" " ")
                     (fn [x]
                       (if (and depth? (nil? x))
                         ""
                         (to-q x)))
                     v)
           "::")
         "]")))

; Q Symbol: to express a Q variable
(defrecord QSym [s]
  ToQ
  (to-q [_] (to-str s)))

; Q Filename (`:xxx form)
(defrecord Filename [file]
  ToQ
  (to-q [_] (str "`:" (to-str file))))


;;;;;;




(defn to-sequential
  "构造一个seq. " [val]
  (cond
    (nil? val) []
    (map? val) [val]
    (coll? val) val
    :else [val]))


(defn to-sym
  "generate a symbol"
  [s]
  `(->QSym ~(to-str s)))

(defn maybe-sym
  "used in macro. convert a clojure symbol to QSym." [s]
  (if (or (string? s) (symbol? s))
    (to-sym s)
    s))

(defn map-sym [xs]
  (map maybe-sym xs))


(defn normalize-expr
  "Used in macro. take care of symbol, vector, and map. convert to their Q constructs."
  [expr & {:keys [as-vector? must-list?]}]
  (cond
    (nil? expr) nil
    (symbol? expr) (to-sym expr)
    (vector? expr) (let [v (mapv normalize-expr expr)]
                     (if as-vector?
                       v
                       (if must-list?
                         `(->QMustList ~v)
                         `(->QList ~v))))
    (map? expr) `(q-dict ~expr)
    ;(sequential? expr) (map normalize-expr expr)
    :else expr))

(defn type-id
  "maps a keyword-ish type to Q type abbreviation char"
  [t & [lower?]]
  (let [c (case t
            (:int :long :i :j) "J"
            (:double :float :real :f :r) "F"
            (:sym :symbol :s) "S"
            (:date :d) "D"
            (:time :t) "T"
            (:datetime :z) "Z"
            (:month :m) "M"
            (:minute :u) "U"
            (:timestamp :p) "P"
            (:bool :b) "B"
            nil)]
    (if c
      (if lower? (.toLowerCase c) c)
      t)))


;;;;;;;;;;;;;

;;; "A Q function. like (fn)"
(defrecord QFN [args body]
  ToQ
  (to-q [_]
    (str "{"
         (if (seq args)
           (str "[" (map-join ";" to-str args) "] "))
         (map-join ";" to-q body) "}")))

; 类型转换: to 为类型（:int :symbol :date ...）
(defrecord Cast [val to]
  ToQ
  (to-q [_] (str \` (to-str to) \$ (to-q val))))

;"Q Variable definition. like (def a ...)"
(defrecord QDef [sym expr]
  ToQ
  (to-q [_] (str (to-q sym) ":" (to-q expr))))

;"Function call. generates (xxx [arg1;arg2;...])"
(defrecord QCall [fun args adverb]
  ToQ
  (to-q [_]
    (let [fs (str (to-q fun) (if adverb (to-q adverb)))
          fs (if (> (.indexOf fs " ") -1)
               (str \( fs \))
               fs)]
      (str \(
           fs
           " "
           \[
           (map-join ";" to-q args)
           \]
           \)))))

(defrecord QAdverb [adverb]
  ToQ
  (to-q [_]
    (to-str adverb)))

(defrecord QInfix [op a b a2 b2 adverb]
  ToQ
  (to-q [_]
    (str \(
         (to-q a)
         (when a2 (str " " (to-q a2)))
         " "
         (to-q op)
         (if adverb (to-q adverb))
         " "
         (to-q b)
         (when b2 (str " " (to-q b2)))
         \))))


(defrecord QMultiOp [op values]
  ToQ
  (to-q [_]
    (str \(
         (map-join (str " " (to-q op) " ")
                   to-q values)
         \))))

;;;;;;;;;; constructs

(defn q-raw [text]
  (->QRaw text))

(defmacro q-call0 [& {:keys [fun args adverb]}]
  `(map->QCall {:fun    ~(normalize-expr fun)
                :args   [~@(normalize-expr (vec args) :as-vector? true)]
                :adverb ~(maybe-sym adverb)}))

(defmacro q-call [fun & args]
  `(q-call0 :fun ~fun
            :args ~args))

(defmacro q-each []
  `(->QAdverb "/"))

(defmacro q-infix0 [& {:keys [a a2 op b b2 adverb]}]
  `(map->QInfix {:a      ~(normalize-expr a)
                 :a2     ~(normalize-expr a2)
                 :b      ~(normalize-expr b)
                 :b2     ~(normalize-expr b2)
                 :op     ~(maybe-sym op)
                 :adverb ~adverb}))

(defmacro q-infix
  ([a op b] `(q-infix ~a nil ~op ~b))
  ([a a2 op b] `(map->QInfix {:op ~(maybe-sym op)
                              :a  ~(normalize-expr a)
                              :a2 ~(normalize-expr a2)
                              :b  ~(normalize-expr b)}))
  ([a a2 op b b2] `(map->QInfix {:op ~(maybe-sym op)
                                 :a  ~(normalize-expr a)
                                 :a2 ~(normalize-expr a2)
                                 :b  ~(normalize-expr b)
                                 :b2 ~(normalize-expr b2)})))

(defmacro q-list [& values]
  `(->QList ~(normalize-expr (vec values)
                             :as-vector? true)))

(defmacro q-file [f]
  `(->Filename ~(maybe-sym f)))

(defn to-filename [v]
  (cond
    (string? v) `(q-file ~v)
    :else (maybe-sym v)))



;;;;;;;;;;;;;;;;

(defmacro q-def [v expr]
  `(->QDef ~(to-sym v) ~(normalize-expr expr :must-list? true)))

(defmacro q-fn [args & body]
  `(->QFN [~@(map to-sym args)]
          [~@(map-sym body)]))

(defmacro q-defn [name args & body]
  `(q-def ~name (q-fn ~args ~@body)))



(defmacro q-plus
  "相加"
  [a b]
  `(q-call ~(to-sym "+") ~a ~b))

(defmacro q-minus [a b]
  `(q-call ~(to-sym "-") ~a ~b))

(defmacro q-mul [a b]
  `(q-call ~(to-sym "*") ~a ~b))

(defmacro q-div [a b]
  `(q-call ~(to-sym "%") ~a ~b))


(defmacro q-flatten [args]
  `(q-call (q-each ~'raze) ~args))

(defmacro q-take
  ([n xs] `(q-take ~n nil ~xs))
  ([n n2 xs] `(q-infix ~n ~n2 ~(to-sym "#") ~xs)))

(defmacro q-range [n]
  `(q-call ~'til ~n))

(defmacro q-str [& coll]
  `(q-call ~'string [~@coll]))

(defmacro q-count [v]
  `(q-call ~'count ~v))

(defmacro q-join [sep coll]
  `(q-infix ~(to-str sep) ~'sv (q-str ~coll)))

(defmacro q-println [s]
  `(q-call ~(to-sym "-1") (q-join " " ~s)))

(defmacro q-split [s sep]
  `(map->QInfix {:op ~(to-sym "vs")
                 :a  ~(to-str sep)
                 :b  ~(maybe-sym s)}))


(defmacro q-if [test true-exp false-exp]
  `(q-call ~(to-sym "$")
           ~test
           ~true-exp
           ~false-exp))

(defmacro q-let [bindings & body]
  `(q-call (q-fn []
                 ~@(for [[v expr] (partition 2 bindings)]
                     `(q-def ~v ~expr))
                 ~@(map-sym body))))

(defmacro q-cond [& clauses]
  (when clauses
    (list `q-if
          (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                     "cond requires an even number of forms")))
          (cons `q-cond (next (next clauses))))))



(defmacro q-eq [a b]
  `(q-infix ~a ~'= ~b))


(defmacro q-match [a b]
  `(q-infix ~a ~(symbol "~") ~b))

(defmacro q-find [d v]
  `(q-infix ~d ~(symbol "?") ~v))

(defmacro q-rand [n max]
  `(q-infix ~n ~(symbol "?") ~max))

(defmacro q-guid []
  `(q-infix -1 ~(symbol "?") (q-raw "0Ng")))

(defmacro q-fill [d v]
  `(q-infix ~v ~(symbol "^") ~d))

(defmacro q-today []
  `(q-raw ".z.d"))

(defmacro q-first [d]
  `(q-call ~'first ~d))

(defmacro q-last [d]
  `(q-call ~'last ~d))

(defmacro q-sum [d]
  `(q-call ~'sum ~d))



;;;;;;;;;;;;; $
; lowercase
(defmacro q-cast [src type]
  `(q-infix ~(type-id type true) ~(to-sym "$") ~src))
; uppercase
(defmacro q-strparse [s type]
  `(q-infix ~(type-id type) ~(to-sym "$") ~s))
; pad
(defmacro q-right-pad [s n]
  `(q-infix ~n ~(to-sym "$") ~s))

(defmacro q-left-pad [s n]
  `(q-infix ~(- n) ~(to-sym "$") ~s))


;;;;;;;;;;;;;; "."
;
(defmacro q-get [d idx]
  `(q-infix ~d ~(symbol "@") ~idx))

(defmacro q-get-in [coll ks]
  #_`(q-infix ~coll ~(to-sym ".") (->QMustList [~@ks]))
  `(q-infix ~coll ~(to-sym " ")
            ~(if (vector? ks) `(->QIndex ~ks true) (maybe-sym ks))))

(defmacro q-dict [m]
  `(->QDict (q-list ~@(map first m))
            (q-list ~@(map second m))))

(defmacro q-table [& [kcols cols]]
  (let [[kcols cols] (if-not cols [nil kcols]
                                  [kcols cols])]
    `(->QTable [~@(map (fn [[k v]]
                         [(maybe-sym k)
                          (normalize-expr v :must-list? true)]) kcols)]
               [~@(map (fn [[k v]]
                         [(maybe-sym k)
                          (normalize-expr v :must-list? true)]) cols)])))

(defmacro q-select-keys [dct ks]
  `(q-infix ~(normalize-expr ks :must-list? true)
            ~(to-sym "#")
            ~dct))

(defmacro q-flip [v]
  `(q-call ~'flip ~v))

(defmacro q-assoc [v ks expr]
  `(q-infix ~(to-sym v)
            ~(if (vector? ks) `(->QIndex ~ks false) (maybe-sym ks))
            ~(to-sym ":")
            ~expr))

(defmacro q-assoc-in [v ks expr]
  `(q-infix ~(to-sym v)
            ~(if (vector? ks) `(->QIndex ~ks true) (maybe-sym ks))
            ~(to-sym ":")
            ~expr))

(defmacro q-update
  ([v op expr]
   `(q-infix ~(to-sym v) ~(to-sym (str (to-str op) ":")) ~expr))
  ([v ks op expr]
   `(q-infix ~(to-sym v)
             ~(if (vector? ks) `(->QIndex ~ks false) (maybe-sym ks))
             ~(to-sym (str (to-str op) ":"))
             ~expr)))

(defmacro q-update-in [v ks op expr]
  `(q-infix ~(to-sym v)
            ~(if (vector? ks) `(->QIndex ~ks true) (maybe-sym ks))
            ~(to-sym (str (to-str op) ":"))
            ~expr))


(defmacro q-concat [l1 l2]
  `(q-infix ~l1 ~(to-sym ",") ~l2))

(defmacro q-zip
  ([l1 l2] `(q-infix0 :a ~l1
                      :b ~l2
                      :op ","
                      :adverb "'"))
  ([l1 l2 l3] `(q-interleave (q-interleave ~l1 ~l2) ~l3)))

(defmacro q-reverse [l]
  `(q-call ~'reverse ~l))

(defmacro q-reverse-each [l]
  `(q-call0 :fun ~'reverse
            :args [~l]
            :adverb " each"))

(defrecord SaveTable [table filename sympath compress?]
  ToQ
  (to-q [_]
    (let [f       (to-q filename)
          sympath (to-q sympath)
          splay?  (.endsWith f "/")
          dest    (if compress?
                    (str "(" f ";17;2;6)")
                    (to-q filename))
          tbl     (if splay?
                    (str ".Q.en[" sympath "] " (to-q table))
                    (to-q table))]

      (str dest " set " tbl))))



(defmacro q-load-csv [f types]
  `(q-infix [(map-join "" #(type-id % true) ~types)
             (q-raw "enlist csv")]
            ~(to-sym "0:")
            ~(to-filename f)))

; (file;17;2;6)
(defmacro q-save-table [table f & {:keys [sympath compress? splayed?]}]
  `(q-infix [~(to-filename f)
             ~@(if compress? [17 2 6])]
            ~'set
            ~@(if splayed?
                [`(q-call ~(to-sym ".Q.en") ~(to-filename (or sympath f)))
                 table]
                [table])))

(defmacro q-rename-cols [cols tr]
  `(q-infix ~cols
            ~'xcol
            ~tr))

(defmacro q-shutdown []
  `(q-raw "\\\\"))

(defmacro q-gc []
  `(q-raw ".Q.gc[]"))

(defmacro q-purge-var [v & [ns]]
  `(q-raw (str "delete " ~(to-str v) " from `" ~(to-str (or ns ".")))))

(def TAGS #_{'def        'q-def
             'defn       'q-defn
             'fn         'q-fn
             'list       'q-list
             'vector     'q-list
             'flatten    'q-flatten
             'println    'q-println
             'take       'q-take
             'range      'q-range
             ;'str     'q-str
             'join       'q-join
             'split      'q-split
             'if         'q-if
             'let        'q-let
             'cond       'q-cond
             'table      'q-table
             'update!    'q-update
             'update-in! 'q-update-in}

  {'+    'q-plus
   '-    'q-minus
   '*    'q-mul
   '/    'q-div
   '=    'q-eq
   '===  'q-match
   'not= 'q-not-match})

(defn normalize-q [body]
  (clojure.walk/prewalk
    (fn [obj]
      (cond
        (list? obj) (let [tag (first obj)]
                      (cons (or (TAGS tag)
                                tag)
                            (rest obj)))
        ;(vector? obj) (list* (into ['q-list] obj))
        :else obj))
    (map-sym body)))



(defmacro q [& body]
  `(s/join ";" (filter some? (map #(if (satisfies? ToQ %) (to-q %) nil) [~@(normalize-q body)]))))

(defmacro print-q [& body]
  `(println (q ~@body)))

(defmacro run-q [conn-str & body]
  `(with-open [conn# (c. (first ~conn-str) (second ~conn-str))]
     (let [q# (q ~@body)]
       (println "run-q:" q#)
       (from-q (.k conn# q#)))))