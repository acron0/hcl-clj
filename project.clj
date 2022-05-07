(defproject org.clojars.acron0/hcl-clj "0.1.1-SNAPSHOT"
  :description "Convert HashiCorp HCL to Clojure hash map"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.bertramlabs.plugins/hcl4j "0.4.0"]]
  :profiles {}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]
                 ["snapshots" {:url "https://clojars.org/repo"
                               :creds :gpg}]])
