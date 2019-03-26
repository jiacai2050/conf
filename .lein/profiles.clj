{:user {:plugins [[lein-try "0.4.3"]
                  [com.gfredericks/lein-shorthand "0.4.1"]
                  [venantius/ultra "0.5.4"]]
        :dependencies [[alembic "0.3.2"]]
        :shorthand {. {pp clojure.pprint/pprint
                       distill alembic.still/distill
                       doc clojure.repl/doc
                       source clojure.repl/source
                       lein alembic.still/lein
                       javadoc clojure.java.javadoc/javadoc
                       join clojure.string/join}}
        ;; :ultra {:repl false}
        ;; :repositories [["clojars" {:url "https://mirrors.ustc.edu.cn/clojars"}]]
        }}
