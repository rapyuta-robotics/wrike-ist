(ns wrike-ist.core
  (:require ["@actions/core" :as core]
            ["@actions/github" :as github]
            [wrike-ist.wrike :as wrike]
            [wrike-ist.azure :as azure]))

(defn find-links
  [text]
  (not-empty (re-seq #"\bhttps://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+\b" text)))
;; (defn find-links
;;   [text]
;;   (let [wrike-pattern #"\bhttps://www\.wrike\.com/open\.htm\?id=\d+\b"
;;         azure-pattern #"\bhttps://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+\b"
;;         combined-pattern (re-pattern (str wrike-pattern "|" azure-pattern))]
;;     (not-empty (re-seq combined-pattern text))))

(defn extract-details
  [pr-obj]
  (when-let [body (.-body pr-obj)]
    (when-let [links (find-links body)]
      (let [state (cond
                    ^boolean (.-merged pr-obj) :merged
                    (= (.-state pr-obj) "closed") :closed
                    ^boolean (.-draft pr-obj) :draft
                    :else :open)
            url ^String (.-html_url pr-obj)
            title ^String (.-title pr-obj)
            id ^long (.-number pr-obj)
            target-branch ^String (-> pr-obj ^String (.-base) ^String (.-ref))
            repository-name ^String (-> pr-obj ^String (.-head) ^String (.-repo) ^String (.-name))]
        (map
         (fn [permalink]
           {:state state
            :permalink permalink
            :pr-url url
            :id id
            :title title
            :target-branch target-branch
            :repository-name repository-name})
         links)))))

(defn main
  []
  (let [payload (.-payload (.-context github))]
    (if-let [pr (.-pull_request payload)]
      (loop [links (extract-details pr)]
        (when-let [{:keys [state] :as details} (first links)]
          (-> (case state
                :draft
                (azure/link-pr details)

                :open
                (azure/link-pr details)

                ;; else ignore
                (js/Promise.resolve))
              (.catch #(core/setFailed (.-message %))))
          (recur (rest links))))
      (js/console.log "No pull_request in payload"))))
