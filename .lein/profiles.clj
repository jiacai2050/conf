{:user {:plugins [[lein-try "0.4.3"]
                  [com.gfredericks/lein-shorthand "0.4.1"]
                  ;; [venantius/ultra "0.6.0"]
                  [jonase/eastwood "0.3.5"]
                  [lein-ancient "0.7.0"]
                  ]

        :dependencies [[alembic "0.3.2"]
                       [vvvvalvalval/scope-capture "0.3.2"]
                       [spieden/spyscope "0.1.7"]
                       ;; https://github.com/jiacai2050/fs
                       [org.clojars.jiacai/fs "1.4.6"]
                       ;; https://github.com/magnars/java-time-literals
                       ;; [java-time-literals "2018-04-06"]
                       [clj-kondo "2020.06.21"]]
        :injections [(require 'spyscope.core)
                     ;; (require 'java-time-literals.core)
                     ]

        :aliases {"lint" ["eastwood"]
                  "lint2" ["run" "-m" "clj-kondo.main" "--lint" "src"]}
        ;; :ultra {:repl false}
        :repositories [["tencent" "https://mirrors.cloud.tencent.com/nexus/repository/maven-public"]]
        :plugin-repositories ^:replace [["tsing-clojars-pl" "https://mirrors.tuna.tsinghua.edu.cn/clojars"
                                         "hw-central-pl" "https://mirrors.huaweicloud.com/repository/maven/"]]
        :mirrors {"central" {:name "hw-central"
                             :url "https://mirrors.huaweicloud.com/repository/maven/"}
                  #"clojars" {:name "tsinghua-clojars"
                              :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"}}

        :shorthand {. {pp clojure.pprint/pprint
                       source clojure.repl/source

                       distill alembic.still/distill
                       lein alembic.still/lein

                       doc clojure.repl/doc
                       javadoc clojure.java.javadoc/javadoc

                       join clojure.string/join
                       trim clojure.string/trim
                       reverse clojure.string/reverse
                       replace clojure.string/replace
                       index-of clojure.string/index-of
                       lower clojure.string/lower-case
                       upper clojure.string/upper-case
                       cap clojure.string/capitalize
                       split clojure.string/split
                       blank? clojure.string/blank?
                       contains? clojure.string/includes?
                       starts-with? clojure.string/starts-with?
                       ends-with? clojure.string/ends-with?

                       sh clojure.java.shell/sh
                       with-sh-dir clojure.java.shell/with-sh-dir
                       with-sh-env clojure.java.shell/with-sh-env

                       expand-home me.raynes.fs/expand-home
                       home me.raynes.fs/home
                       ls me.raynes.fs/list-dir
                       basename me.raynes.fs/base-name
                       extension me.raynes.fs/extension
                       rmr me.raynes.fs/delete-dir
                       touch me.raynes.fs/touch
                       glob me.raynes.fs/glob
                       cp+ me.raynes.fs/copy+
                       walk me.raynes.fs/walk
                       tmp-file me.raynes.fs/temp-file
                       tmp-dir me.raynes.fs/temp-dir
                       mv me.raynes.fs/move


                       as-file clojure.java.io/as-file
                       mkdir clojure.java.io/make-parents
                       cp clojure.java.io/copy
                       rm clojure.java.io/delete-file
                       is clojure.java.io/input-stream
                       os clojure.java.io/output-stream
                       reader clojure.java.io/reader
                       writer clojure.java.io/writer

                       ^:lazy spy sc.api/spy
                       ^:lazy letsc sc.api/letsc
                       ^:lazy defsc sc.api/defsc
                       ^:lazy ep-repl sc.repl/ep-repl
                       }}}}
