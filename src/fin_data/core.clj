(ns fin-data.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [sys-loader.core :as sys]
            [fin-data.ddl :as ddl])
  (:gen-class))

(def cli-options
  [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ["-s" "--server"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["fin-data tool"
        ""
        "Usage: cmd [options]"
        ""
        "Commands:"
        "server      - run server"
        ""
        "Options:"
        options-summary
        ""]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (#{"server"} (first arguments))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))


(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [action _options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "server" (do
                   (sys/-main args)
                   (log/debug "fin-data server init complete"))))))


(defn init
  "The sys-module initialization fn. This configures the DB schema."
  [state]
  (let [migrate (-> :sys/migrations state)]
    (ddl/exec-ddl migrate)
    (log/info "fin-data module init complete")))


(comment
  *e
  (sys/-main [])
  @sys/sys-state
  (clojure.pprint/pprint @sys/sys-state)

  ;;
      )