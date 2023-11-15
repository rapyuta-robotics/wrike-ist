(ns wrike-ist.core
  (:require ["@actions/core" :as core]
            ["@actions/github" :as github]
            [wrike-ist.wrike :as wrike]))

(defn find-links
  [text]
  (not-empty (re-seq #"\bhttps://www\.wrike\.com/open\.htm\?id=\d+\b" text)))

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

(defn main []
  (let [payload (.-payload (.-context github))]
    (if-let [pr (.-pull_request payload)]
      (loop [links (extract-details pr)]
        (when-let [{:keys [state] :as details} (first links)]
          (let [link-pr-promise (wrike/link-pr details)]
            (-> (case state
                  :draft
                  link-pr-promise

                  :open
                  (js/Promise.all
                   [link-pr-promise
                    (wrike/progress-task details (core/getInput "opened"))])

                  :merged
                  (wrike/complete-task details (core/getInput "merged"))

                  :closed
                  (wrike/cancel-task details (core/getInput "closed"))

                  ;; else ignore
                  (js/Promise.resolve))
                (.then
                 (fn [result]
                   (.log js/console (str "Result of link-pr: " result)))
                 (.catch #(core/setFailed (.-message %)))))
            (recur (rest links)))))
        (js/console.log "No pull_request in payload"))))
