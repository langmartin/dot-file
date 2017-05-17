{:user
 {:plugins [[cider/cider-nrepl "0.14.0"]
            [lein-ancient "0.6.10" :exclusions [org.clojure/clojure]]

            ;; 1. jack in to a jvm repl
            ;; 2. (require '[figwheel-sidecar.repl-api :refer :all])
            ;; 3. (start-figwheel!)
            ;; 4. (cljs-repl)
            [lein-figwheel "0.5.10" :exclusions [org.clojure/clojure]]

            [jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
            [venantius/yagni "0.1.4"]
            [lein-kibit "0.1.5"]
            [lein-bikeshed "0.4.1"]
            ;; [lein-cloverage "1.0.7"]
            ]

  :dependencies [[org.clojure/tools.trace "0.7.9"]
                 [pjstadig/humane-test-output "0.8.1"]
                 [org.clojure/tools.namespace "0.2.11"]
                 ;; need these for figwheel
                 [figwheel-sidecar "0.5.10"]
                 [com.cemerick/piggieback "0.2.1"]
                 ]

  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]}

 :repl
 {:repl-options {:init (set! *print-length* 50)
                 :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 }
  }}
