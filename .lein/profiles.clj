{:user {:plugins [[lein-try "0.4.3"]
                  [com.gfredericks/lein-shorthand "0.4.1"]
                  ]
        :dependencies [[alembic "0.3.2"]
                       ]
        :shorthand {. {pp clojure.pprint/pprint
                       distill alembic.still/distill
                       lein alembic.still/lein
                       join clojure.string/join}
                    }
;;        :repositories [["clojars" {:url "http://mirrors.ustc.edu.cn/clojars"}]]
               }}
