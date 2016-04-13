(def +project+ 'qwrap-clj)
(def +version+ "0.1.0-SNAPSHOT")

(merge-env!
  :dependencies '[[org.clojure/clojure "1.8.0"]])

(task-options! 
      pom {:project +project+ 
           :version +version+})

(deftask build []
      (comp (pom)
            (javac)
            (jar)
            (install)))
