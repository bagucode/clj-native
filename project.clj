(defproject clj-native "0.5.0-SNAPSHOT"
  :description "Simplify usage of native libs from Clojure. Uses JNA."
  :dependencies [[org.clojure/clojure "1.1.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 ;; Could not find a repo that had the latest jna version
                 ;; so this is a local install (put on clojars?)
                 [com.sun.jna/jna "3.2.4"]]
  :dev-dependencies [[leiningen/lein-swank "1.0.0-SNAPSHOT"]])
