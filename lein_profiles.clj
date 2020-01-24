{:user {:plugins [[org.clojure/clojure "1.10.1"]
                  [cider/cider-nrepl "0.21.1"]
                  [lein-midje "3.2.1"]
                  [lein-cloverage "1.1.2"]
                  [lein-ancient "0.6.15"]
                  [cider/cider-nrepl "0.22.0"]
                  [lein-cljfmt "0.6.6"]]}
 :repl {:repl-options {:init (clojure.core.server/start-server {:accept 'clojure.core.server/io-prepl
                                                                :address "localhost"
                                                                :port 55555
                                                                :name "lein"})}}}
