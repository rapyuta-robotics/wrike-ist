(ns wrike-ist.core
  (:require ["@actions/core" :as core]
            ["@actions/github" :as github]
            [wrike-ist.wrike :as wrike]
            [wrike-ist.azure :as azure]))

(defn find-links
  [text]
  (not-empty (re-seq #"\bhttps://www\.wrike\.com/open\.htm\?id=\d+\b|\bhttps://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+\b" text)))

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
    (re-find #"\bhttps://www\.wrike\.com/open\.htm\?id=\d+\b" url) :wrike
    (re-find #"\bhttps://dev\.azure\.com/[^/]+/[^/]+/_workitems/edit/\d+\b" url) :azure
    :else :unknown))

(defn main
  []
  (let [payload (.-payload (.-context github))]
    (if-let [pr (.-pull_request payload)]
      (loop [links (extract-details pr)]
        (when-let [{:keys [state pr-url permalink] :as details} (first links)]
          (let [link-type (find-link-type (first permalink))]
            (-> (case state
                  :draft
                  (case link-type
                    :wrike (wrike/link-pr details)
                    :azure (azure/link-pr details)
                    :unknown (js/console.log (str "Unknown link type: " permalink))
                    (js/Promise.resolve))

                  :open
                  (case link-type
                    :wrike (wrike/link-pr details)
                    :azure (azure/link-pr details)
                    :unknown (js/console.log (str "Unknown link type: " permalink))
                    (js/Promise.resolve))

                  ;; else ignore
                  (js/Promise.resolve))
              (.catch #(core/setFailed (.-message %)))))
          (recur (rest links))))
      (js/console.log "No pull_request in payload"))))
