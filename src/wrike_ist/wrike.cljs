(ns wrike-ist.wrike
  (:require [httpurr.client.node :as http]
            [clojure.string :as str]))

(defn- wrike-token
  []
  (some-> js/process .-env .-WRIKE_TOKEN .trim))

(defn- headers
  []
  {:Authorization (str "bearer " (wrike-token))
   :Content-Type "application/json"})

(def folder-names "98_issues 02_sootballs_releases")

(def link-badge
  (str "<span "
       "style=\"background-color: rgb(255,204,128); color: rgb(25,25,25);\" "
       "contenteditable=\"false\">"
       "Pull request for "
       "</span> "))

(defn link-html
  [{:keys [id pr-url target-branch repository-name title]}]
  (if (empty? title)
    (str link-badge target-branch ": " "<a href=\"" pr-url "\">" " (#" id ")</a>")
    (str link-badge repository-name " on branch " target-branch ": " "<a href=\"" pr-url "\">" title " (#" id ")</a>")))

(defn parse-body
  [response]
  (js->clj (js/JSON.parse (:body response))))

(defn find-task
  [permalink]
  (let [uri (str "https://www.wrike.com/api/v4/tasks?fields=[parentIds]"
                 "&permalink=" (js/encodeURIComponent permalink))]
    (.then
     (http/get uri {:headers (headers)})
     (fn [response]
       (if-let [task (get-in (parse-body response) ["data" 0])]
         (do
           (.info js/console "find-task: Task found")
           (js/Promise.resolve task))
         (do
           (.error js/console "find-task: Task not found")
           (js/Promise.reject (js/Error. "find-task: Task not found"))))))))

(defn get-folder-id
  [folder-names]
  (let [uri (str "https://www.wrike.com/api/v4/folders")]
    (-> (http/get uri {:headers (headers)})
        (.then parse-body)
        (.then (fn [{folders "data"}]
                 (let [folder-names (clojure.string/split folder-names #"\s+")
                       matching-folders (filter #(contains? folder-names (:title %)) folders)]
                   (if (seq matching-folders)
                     (map :id matching-folders)
                     [])))))))

(defn fetch-wrike-task [task-id]
  (let [task-url (str "https://www.wrike.com/api/v4/tasks/" task-id)
        headers {:headers (headers)}
        response (-> (http/get task-url headers)
                     parse-body)]
    (if (= 200 (:status response))
      response
      nil)))

(defn is-wrike-task-in-folder? [permalink folder-id]
  (find-task permalink)
  (fn [{:strs [task-id]}]
    (let [uri (str "https://www.wrike.com/api/v4/tasks/" task-id)]
      (-> (http/get uri {:headers (headers)})
          (.then parse-body)
          (.then (fn [task]
                  (if (contains? (:folders task) folder-id)
                    (do
                        (.log js/console (str  "is-wrike-task-in-folder?: Task is in the folder or an inherited folder: true"))
                        true)
                    (if-let [parent-id (:parentIds task)]
                        (do
                          (.log js/console (str  "is-wrike-task-in-folder?: Task is in the folder or an inherited folder: true"))
                          (some #(contains? (:folders (fetch-wrike-task %)) folder-id) parent-id))
                        (do
                          (.log js/console (str  "is-wrike-task-in-folder?: Task is not in the folder or an inherited folder: false"))
                          false)))))))))


(defn check-valid-task
  [{:keys [permalink target-branch folder-names]}]
  (when (and target-branch (str/starts-with? target-branch "ayush/"))
    (let [folder-ids (get-folder-id folder-names)]
      (if (seq folder-ids)
        (if (is-wrike-task-in-folder? permalink (first folder-ids))
          (do
            (.info js/console "check-valid-task: Task is in the folder or an inherited folder: true")
            (js/Promise.resolve permalink))
          (do
            (.error js/console "check-valid-task: Task not found")
            (js/Promise.reject (js/Error. "Task not found"))))
        (do
          (.error js/console "check-valid-task: No matching folder found")
          (js/Promise.reject (js/Error. "check-valid-task: No matching folder found")))))))

(defn link-pr
  [{:keys [pr-url permalink] :as details}]
  (.then
   (find-task permalink)
   (fn [{:strs [id]}]
     (let [uri (str "https://www.wrike.com/api/v4/tasks/" id "/comments")]
       (-> (http/get uri {:headers (headers)})
            
           (.then (fn find-existing-link [response]
                    (reduce
                     (fn [ok comment]
                       (if (.includes (get comment "text") pr-url)
                         (reduced (js/Promise.reject :present))
                         ok))
                     (js/Promise.resolve)
                     (get (parse-body response) "data"))))
           (.then (fn add-link-comment [& _]
                    (let [comment-text (link-html details)
                          params (clj->js {:text comment-text
                                           :plainText false})]
                      (http/post uri {:headers (headers)
                                      :body (js/JSON.stringify params)}))))
           (.then #(.log js/console (str  "link-pr: PR link sent to task")))
           (.catch #(if (= % :present)
                      (.log js/console (str  "link-pr: PR link already in comments"))
                      (js/Promise.reject %))))))))

(defn folder-statuses
  [folder-id]
  (let [uri (str "https://www.wrike.com/api/v4/folders/" folder-id)]
    (-> (http/get uri {:headers (headers)})
        (.then parse-body)
        (.then (fn find-workflow
                 [{[{id "workflowId"}] "data"}]
                 (let [uri "https://www.wrike.com/api/v4/workflows"]
                   (-> (http/get uri {:headers (headers)})
                       (.then parse-body)
                       (.then (fn [{workflows "data"}]
                                (->> workflows
                                     (filter #(= (get % "id") id))
                                     first)))))))
        (.then (fn [{statuses "customStatuses"}]
                 (filter #(= (get % "hidden") false) statuses))))))

(defn find-status
  [statuses {:keys [wanted-status wanted-group]}]
  (reduce
   (fn [{current-group "group" :as candidate} {:strs [name group] :as status}]
     (if (= name wanted-status)
       ;; if an exact name match is found, that's it
       (reduced status)
       ;; if wanted-group isn't set then only exact name matches are acceptable
       (when wanted-group
         (cond
           ;; if the current candidate is already in the right group use it
           (= current-group wanted-group)
           candidate

           ;; else if the new status is in the right group use that
           (= group wanted-group)
           status))))
   nil
   statuses))

(defn next-status
  ;; {:keys [wanted-status wanted-group] :as opts}
  [folder-id opts]
  (.then
   (folder-statuses folder-id)
   (fn [statuses]
     (if-let [match (find-status statuses opts)]
       match
       (js/Promise.reject (str "next-status: No appropriate status found" opts))))))

(defn update-task-status
  [{task-id "id" [folder-id] "parentIds"} wanted]
  (.then
   (next-status folder-id wanted)
   (fn [{:strs [id]}]
     (let [uri (str "https://www.wrike.com/api/v4/tasks/" task-id)
           params (clj->js {:customStatus id})]
       (http/put uri {:headers (headers)
                      :body (js/JSON.stringify params)})))))

(defn progress-task
  [{:keys [permalink]} wanted-status]
  (when (not-empty wanted-status)
    (.then
     (find-task permalink)
     #(update-task-status % {:wanted-status wanted-status}))))

(defn complete-task
  [{:keys [permalink]} wanted-status]
  (if-not (= "-" wanted-status)
    (.then
     (find-task permalink)
     #(update-task-status % {:wanted-status wanted-status
                             :wanted-group "Completed"}))
    (.log js/console (str  "complete-task: Skipping `merged` transition because it's set to \"-\""))))

(defn cancel-task
  [{:keys [permalink]} wanted-status]
  (if-not (= "-" wanted-status)
    (.then
     (find-task permalink)
     #(update-task-status % {:wanted-status wanted-status
                             :wanted-group "Cancelled"}))
    (.log js/console (str  "cancel-task: Skipping `closed` transition because it's set to \"-\""))))
