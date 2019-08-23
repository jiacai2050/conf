{:user {:plugins [[lein-try "0.4.3"]
                  [com.gfredericks/lein-shorthand "0.4.1"]
                  ;; [venantius/ultra "0.6.0"]
                  [jonase/eastwood "0.3.5"]
                  ]
        :dependencies [[alembic "0.3.2"]
                       [vvvvalvalval/scope-capture "0.3.2"]
                       ]
        :shorthand {. {pp clojure.pprint/pprint
                       distill alembic.still/distill
                       doc clojure.repl/doc
                       source clojure.repl/source
                       lein alembic.still/lein
                       javadoc clojure.java.javadoc/javadoc
                       join clojure.string/join
                       spy sc.api/spy
                       letsc sc.api/letsc
                       defsc sc.api/defsc
                       ep-repl sc.repl/ep-repl
                       }}
        :aliases {"lint" ["eastwood"]}
        ;; :ultra {:repl false}
        :repositories {"clojars" "https://mirrors.tuna.tsinghua.edu.cn/clojars"
                       "central" "https://mirrors.huaweicloud.com/repository/maven/"
                       "tencent" "https://mirrors.cloud.tencent.com/nexus/repository/maven-public"}
        }}
