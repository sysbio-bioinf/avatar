(def version
  (let [version-file "src/clojure/avatar/version.clj"
        version-fn (try
                     ; works in leiningen
                     (load-file version-file)
                     (resolve 'avatar.version/current-version)
                     (catch java.io.FileNotFoundException e
                       ; workaround for CCW (version number is not needed anyway)
                       (constantly "0.8.15-UNDEFINED")))]
    (version-fn)))


(defproject avatar version
  :license {:name "Eclipse Public License v2.0"
            :url "http://www.eclipse.org/legal/epl-v20.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.apache.poi/poi "3.17"]
                 [com.monitorjbl/xlsx-streamer "1.2.1"]
                 [org.clojure/data.int-map "0.2.4"]
                 [com.stuartsierra/component "0.3.1"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.18"]
                 [frost "0.5.4"]
                 ; newer snappy
                 [org.xerial.snappy/snappy-java "1.1.7.2"]
                 [org.uma.jmetal/jmetal-core "5.3"
                  :exclusions [org.knowm.xchart/xchart]]
                 [org.uma.jmetal/jmetal-algorithm "5.3"]
                 [clj-jfx "0.1.25"]
                 [clj-http "3.9.1"]
                 [org.clojure/data.json "0.2.6"]
                 [com.climate/claypoole "1.1.4"]
                 ; SVG
                 [xerces/xerces "2.4.0"]
                 [org.apache.xmlgraphics/batik-transcoder "1.13"
                  :exclusions [xerces/xercesImpl
                               org.python/jython
                               org.mozilla/rhino]]
                 [org.apache.xmlgraphics/batik-codec "1.13"]
                 [org.apache.xmlgraphics/batik-anim "1.13"]
                 [org.apache.xmlgraphics/batik-svggen "1.13"] ; to convert fonts
                 [org.apache.xmlgraphics/xmlgraphics-commons "2.4"]
                 [org.apache.xmlgraphics/fop "2.7" :exclusions [commons-logging]]
                 ; decompress tar archives
                 [org.apache.commons/commons-compress "1.13"]
                 [commons-io "2.4"]
                 ; data validation
                 [prismatic/schema "1.1.7"]]

  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [org.clojure/test.check "1.0.0"]
                                  [com.gfredericks/test.chuck "0.2.10"]
                                  [clj-debug "0.7.6"]
                                  [criterium "0.4.4"]
                                  [org.apache.xmlgraphics/batik-swing "1.10"] ; only for swing canvas
                                  [org.apache.commons/commons-math3 "3.6.1"]
                                  #_[com.clojure-goes-fast/clj-memory-meter "0.1.0"]]
                   :source-paths ["dev"]
                   :jvm-opts ^:replace ["-XX:-OmitStackTraceInFastThrow" "-XX:+UseG1GC"
                                        "--add-modules" "java.xml"]}
             :debug {:dependencies [[clj-debug "0.7.6"]]}
             :uberjar {:aot :all
                       :omit-source true
                       ;:plugins [[jfx-eval "0.1.0-SNAPSHOT"]]
                       :jvm-opts ["-Dclojure.compiler.elide-meta=[:doc]"
                                  "-Dclojure.compiler.direct-linking=true"]
                       :uberjar-exclusions [#"clj_jfx.*\.clj", #"lic.*\.clj"]
                       :main avatar.Main}
             :run {:main avatar.Main
                   :aot [avatar.main, avatar.gui-application]
                   :jvm-opts ^:replace ["-XX:-OmitStackTraceInFastThrow" "-XX:+UseG1GC"
                                        "--add-modules" "java.xml"]}}


  :java-cmd "/usr/bin/java"
  :jvm-opts ["--add-modules" "java.xml"]

  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]

  :jar-name ~(format "avatar-lib-%s.jar" version)
  :uberjar-name ~(format "avatar-%s.jar" version)

  :uberjar-exclusions [#"cec2005CompetitionResources" #"cec2015Comp" #"MOEAD_Weights" #"mombi2-weights" #"^pareto_fronts" #"tspInstances" #"eil101.tsp" #"module-info.class"]

  :repl-options {:init-ns dev}

  :plugins [[lein-licenses-file "0.1.0"]]

  :licenses-file/synonyms "config/synonyms.edn"
  :licenses-file/overrides "config/overrides.edn"

  :aliases {"extractlicenses" ["with-profile" "uberjar" "licenses-file" "resources/library-licenses.edn"]
            "release-uberjar" ["with-profile" "uberjar" "do" "clean," "uberjar"]
            "debug-uberjar" ["with-profile" "uberjar,debug" "do" "clean," "uberjar"]
            "build" ["do" "extractlicenses," "release-uberjar"]
            "run" ["with-profile" "run" "do" "clean," "run"]})
