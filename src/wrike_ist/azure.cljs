(ns wrike-ist.azure
  (:require [httpurr.client.node :as http]
            [clojure.string :as str]))
(defn- azure-token []
  (some-> js/process .-env .-AZURE_TOKEN .trim))

(defn- headers []
  {:Authorization (str "Basic " (js/btoa (str ":" (azure-token))))
   :Content-Type "application/json"})

(defn parse-body
   [response]
   (js->clj (js/JSON.parse (:body response))))

(defn link-html
   [{:keys [id pr-url target-branch repository-name title]}]
   (if (empty? title)
     (str "Pull request for " target-branch ": " "<a href=\"" pr-url "\">" " (#" id ")</a>")
     (str "Pull request for " repository-name " on branch " target-branch ": " "<a href=\"" pr-url "\">" title " (#" id ")</a>")))

(defn find-task
  [url]
  (let [pattern #"https://dev\.azure\.com/([^/]+)/([^/]+)/_workitems/edit/(\d+)"
        matches (re-seq pattern url)]
    (js/Promise.
     (fn [resolve reject]
       (if (seq matches)
         (let [[_ organization project task-id] (first matches)]
           (resolve {:organization organization
                     :project project
                     :task-id (js/parseInt task-id)}))
         (reject (js/Error. "No task ID found")))))))

(defn link-pr
  [{:keys [pr-url permalink] :as details}]
  (.then
   (find-task permalink)
   (fn [{:keys [organization project task-id]}]
       (js/console.log (str "link-pr: Found organization:" organization " project:" project " task-id:" task-id))
       (js/console.log (str "headers:" (headers)))
       (let [uri (str "https://dev.azure.com/" organization "/" project "/_apis/wit/workitems/" task-id "/comments?api-version=7.0-preview.3")]
        ;;  (js/console.log (str "uri:" uri))
         (-> (http/get uri {:headers (headers)})
             (.then (fn find-existing-link [response]
                    ;;   (js/console.log (str "find-existing-link: response:" response))
                      (reduce
                       (fn [ok comment]
                         (if (.includes (get comment "text") pr-url)
                           (reduced (js/Promise.reject :present))
                           ok))
                       (js/Promise.resolve)
                       (get (parse-body response) "comments"))))
             (.then (fn add-link-comment [& _]
                      (let [comment-text (link-html details)
                            params (clj->js {:text comment-text})]
                        (js/console.log (str "add-link-comment: params:" (js/JSON.stringify params)))
                        (-> (http/post uri {:headers (headers)
                                        :body (js/JSON.stringify params)})
                                        (.then (fn [response]
                                                (js/console.log (str "add-link-comment: response:" response)))))
                                        )))
             (.then #(js/console.log "PR link sent to task"))
             (.catch #(if (= % :present)
                        (js/console.log "PR link already in comments")
                        (js/Promise.reject %))))))))
