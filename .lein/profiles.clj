{:user
 {:plugins [;; [cider/cider-nrepl "0.24.0"]
            [lein-ancient "0.7.0" :exclusions [org.clojure/clojure]]

            ;; 1. jack in to a jvm repl
            ;; 2. (require '[figwheel-sidecar.repl-api :refer :all])
            ;; 3. (start-figwheel!)
            ;; 4. (cljs-repl)
            ;; [lein-figwheel "0.5.19" :exclusions [org.clojure/clojure]]

            [jonase/eastwood "1.4.2" :exclusions [org.clojure/clojure]]
            [venantius/yagni "0.1.7"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            ;; [lein-cloverage "1.0.7"]
            ]

  :dependencies [
                 [org.clojure/clojure "1.11.1"]
                 ;; [org.clojure/tools.trace "0.7.10"]
                 ;; [pjstadig/humane-test-output "0.10.0"]
                 ;; [org.clojure/tools.namespace "1.0.0"]
                 ;; need these for figwheel
                 ;; [figwheel-sidecar "0.5.19"]
                 [cider/piggieback "0.5.3"]
                 ]

  ;; :injections [(require 'pjstadig.humane-test-output)
  ;;              (pjstadig.humane-test-output/activate!)]
  }

 :repl
 {:repl-options {:init (set! *print-length* 50)
                 :nrepl-middleware [cider.piggieback/wrap-cljs-repl]
                 }
  }}
