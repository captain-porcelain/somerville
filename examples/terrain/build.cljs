(require '[lumo.build.api :as b])

(b/build "."
         {:main 'core
          :output-to "app.js"
          :optimizations :advanced})
