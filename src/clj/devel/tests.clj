(ns devel.tests
  (:require
    [clojure.string :as s]
    [clojure.java.shell :as shell]
    [clojure.java.io :as io]
    [clojure.test :as ctest]
    [pjstadig.humane-test-output :as hto]))

(hto/activate!)

(def last-test-run (atom 0))
(def running (atom false))

(defn to-ns
  "String replacements to get namespace from filename."
  [filepath]
  (s/replace (s/replace (s/replace (s/replace filepath "test/" "") "/" ".") ".clj" "") "_" "-"))

(defn find-test-files
  "Use the shell to list all files in test folder."
  []
  (filter #(.contains % ".clj") (s/split (:out (shell/sh "find" "test")) #"\n")))

(defn filemap
  "Create a map for each filepath remembering when it was last modified."
  [filepath]
  (let [f (io/file filepath)]
    {:filepath filepath
     :namespace (symbol (to-ns filepath))
     :file f
     :last-modified (.lastModified f)}))

(defn find-tests
  "Find all tests and get meta information."
  []
  (map filemap (find-test-files)))

(defn load-namespace
  "Require and/or refresh namespaces."
  [n]
  (require n :reload-all))

(defn run
  "Run either all tests or only the named ones."
  ([]
   (let [tests (find-tests)
         namespaces (map :namespace tests)]
     (dorun (map load-namespace namespaces))
     (apply ctest/run-tests namespaces)))
  ([namespaces]
   (apply ctest/run-tests namespaces)))

(defn run-changed
  "Run all tests initially then watch the files and rerun when changes occur."
  []
  (let [tests (filter #(< @last-test-run (:last-modified %)) (find-tests))
        namespaces (map :namespace tests)]
    (dorun (println (str "Changed namespace:\n\t" (s/join "\n\t" namespaces))))
    (dorun (map load-namespace namespaces))
    (reset! last-test-run (System/currentTimeMillis))
    (apply ctest/run-tests namespaces)))

(defn watch
  "Run all tests initially then watch the files and rerun when changes occur."
  []
  (reset! running true)
  (future (while @running (run-changed) (Thread/sleep 1000))))

(defn stop-watching
  []
  (reset! running false))

