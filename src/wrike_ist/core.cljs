(ns wrike-ist.core
  (:require ["@actions/core" :as core]
            ["@actions/github" :as github]
            [wrike-ist.wrike :as wrike]
            [wrike-ist.azure :as azure]))

;; (defn find-links
;;   [text]
;;   (not-empty (re-seq #"\bhttps://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+\b" text)))
(defn find-links
  [text]
  (let [wrike-pattern #"https://www\.wrike\.com/open\.htm\?id=\d+"
        azure-pattern #"https://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+"]
    (let [wrike-matches (re-seq wrike-pattern text)
          azure-matches (re-seq azure-pattern text)]
      (do
        ;; Print debug information
        (js/console.log "Wrike Pattern:" wrike-pattern)
        (js/console.log "Azure Pattern:" azure-pattern)
        (js/console.log "Text to Search:" text)
        (js/console.log "Wrike Matches Found:" wrike-matches)
        (js/console.log "Azure Matches Found:" azure-matches)
        
        ;; Combine results
        (cond
          (seq wrike-matches) wrike-matches
          (seq azure-matches) azure-matches
          :else (js/console.log "No matching links found"))))))

;; Example usage
(find-links "Here are some links: https://www.wrike.com/open.htm?id=123456 and https://dev.azure.com/organization/project/_workitems/edit/98765")


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

(defn find-link-type
  [url]
  (cond
    (re-find #"https://www\.wrike\.com/open\.htm\?id=\d+" url) :wrike
    (re-find #"https://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+" url) :azure
    :else :unknown))

(defn main
  []
  (let [payload (.-payload (.-context github))]
    (if-let [pr (.-pull_request payload)]
      (loop [links (extract-details pr)]
        (when-let [{:keys [state pr-url] :as details} (first links)]
          (let [link-type (find-link-type pr-url)]
            (-> (case state
                  :draft
                  (case link-type
                    :wrike (wrike/link-pr details)
                    :azure (azure/link-pr details)
                    (js/Promise.resolve))

                  :open
                  (case link-type
                    :wrike (wrike/link-pr details)
                    :azure (azure/link-pr details)
                    (js/Promise.resolve))

                  ;; else ignore
                  (js/Promise.resolve))
              (.catch #(core/setFailed (.-message %)))))
          (recur (rest links))))
      (js/console.log "No pull_request in payload"))))
