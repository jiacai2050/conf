{:user {:plugins [[lein-try "0.4.3"]
                  [com.gfredericks/lein-shorthand "0.4.1"]
                  ;; [venantius/ultra "0.6.0"]
                  [jonase/eastwood "0.3.5"]]

        :dependencies [[alembic "0.3.2"]
                       [vvvvalvalval/scope-capture "0.3.2"]]
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
        :repositories [["tencent" "https://mirrors.cloud.tencent.com/nexus/repository/maven-public"]]
        :plugin-repositories ^:replace [["tsing-clojars-pl" "https://mirrors.tuna.tsinghua.edu.cn/clojars"
                                         "hw-central-pl" "https://mirrors.huaweicloud.com/repository/maven/"]]
        :mirrors {"central" {:name "hw-central"
                             :url "https://mirrors.huaweicloud.com/repository/maven/"}
                  #"clojars" {:name "tsinghua-clojars"
                              :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"}}}}
