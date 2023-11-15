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

 (def folder-names ["98_issues" "02_sootballs_releases"])

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
   (let [uri (str "https://www.wrike.com/api/v4/tasks?fields=['parentIds', 'superParentIds']"
                  "&permalink=" (js/encodeURIComponent permalink))]
     (.then
      (http/get uri {:headers (headers)})
      (fn [response]
        (if-let [task (get-in (parse-body response) ["data" 0])]
          (do
            (.info js/console "")
            (js/Promise.resolve task))
          (do
            (.error js/console "find-task: Task not found")
            (js/Promise.reject (js/Error. "find-task: Task not found"))))))))

(defn fetch-folder-details [folder-id folder-names]
  (let [uri (str "https://www.wrike.com/api/v4/folders/" folder-id)]
    (-> (http/get uri {:headers (headers)})
        (.then parse-body)
        (.then (fn [response]
                 (let [title (-> response
                                 (get-in ["data" 0 "title"]))]
                   ()
                  ;;  (.info js/console (str "Folder ID: " folder-id " Title: " title))
                   (let [lowercase-folder-names (map clojure.string/lower-case folder-names)
                         parent-ids (-> response
                                        (get-in ["data" 0 "parentIds"])
                                        (or []))]
                    ;;  (.info js/console (str "Parent IDs: " parent-ids)) 
                     (if (and #_{:clj-kondo/ignore [:not-empty?]}
                          (not (empty? title))
                              (some #(str/starts-with? title %) lowercase-folder-names))
                       (do
                         (.info js/console (str "Match found for folder: " title))
                         (js/Promise.resolve true))
                       (js/Promise.all
                        (for [parent-id parent-ids]
                          (fetch-folder-details parent-id folder-names)))))))))))

(defn is-wrike-task-in-folder? [permalink]
  (.then
   (find-task permalink)
   (fn [{:strs [id parentIds superParentIds title]}]
     (.info js/console (str "is-wrike-task-in-folder?: Task Name: " title " id: " id " parentIds: " parentIds " superParentIds: " superParentIds))
     (.info js/console (str "is-wrike-task-in-folder?: Folder names to match: " folder-names))
     (let [all-parent-ids (concat parentIds superParentIds)
           matching-folders-promises (for [parent-id all-parent-ids
                                            :let [folder-details-promise (fetch-folder-details parent-id folder-names)]]
                                        folder-details-promise)]
       (js/Promise.all matching-folders-promises)
       (.then
        (js/Promise.all matching-folders-promises)
        (fn [matching-folders]
          (if (some true? matching-folders)
            (do
              (.info js/console (str "is-wrike-task-in-folder?: Matching folders found"))
              true)
            (do
              (.info js/console "is-wrike-task-in-folder?: Task is not a valid bug ticket of part of a planned release")
              false))))))))


(defn check-valid-task
  [permalink target-branch]
  (js/Promise.
   (fn [resolve reject]
     (if (and target-branch (str/starts-with? target-branch "release"))
       (do
         (.info js/console (str "check-valid-task: PR is targeted to release branch, checking if task is a valid bug ticket or part of planned releases"))
         (let [task-in-folder-promise (is-wrike-task-in-folder? permalink)]
           (.then task-in-folder-promise
                  (fn [task-in-folder?]
                    (if task-in-folder?
                      (do
                        (.info js/console "check-valid-task: Task is in the folder or an inherited folder: true")
                        (resolve permalink))
                      (do
                        (.error js/console "check-valid-task: Task not found in folder")
                        (reject (js/Error. "check-valid-task: Task not found in folder"))))))))
       (do
         (.info js/console "check-valid-task: PR is not targeted to release branch, returning success")
         (resolve true))))))

(defn link-pr
  [{:keys [pr-url permalink target-branch] :as details}]
  (let [check-valid-task-promise (check-valid-task permalink target-branch)]
    (-> (find-task permalink)
        (.then
         (fn [{:strs [id]}]
           (let [uri (str "https://www.wrike.com/api/v4/tasks/" id "/comments")]
             (-> (http/get uri {:headers (headers)})
                 (.then
                  (fn find-existing-link [response]
                    (reduce
                     (fn [ok comment]
                       (if (.includes (get comment "text") pr-url)
                         (reduced (js/Promise.reject :present))
                         ok))
                     (js/Promise.resolve)
                     (get (parse-body response) "data"))))
                 (.then
                  (fn add-link-comment [& _]
                    (let [comment-text (link-html details)
                          params (clj->js {:text comment-text
                                           :plainText false})]
                      (http/post uri {:headers (headers)
                                      :body (js/JSON.stringify params)}))))
                 (.then
                  (fn [_]
                    (.log js/console (str  "link-pr: PR link sent to task"))
                    (js/Promise.resolve)))
                 (.catch
                  #(if (= % :present)
                     (.log js/console (str  "link-pr: PR link already in comments"))
                     (js/Promise.resolve %)))))))
        (.then
         (fn [_]
           (js/Promise.all [check-valid-task-promise
                            (.then check-valid-task-promise
                                   (fn [result]
                                     (.log js/console (str "check-valid-task-promise value: " result))))])))
        (.catch
         #(do
            (.log js/console (str "link-pr: Rejected with reason: " %))
            (js/Promise.reject %))))))



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
   (if-not (= "-" wanted-status)
     (.then
      (find-task permalink)
      #(update-task-status % {:wanted-status wanted-status
                              :wanted-group "In Progress"}))
     (do
       (js/console.log "Skipping `closed` transition because it's set to \"-\"")
       (js/Promise.resolve))))

(defn complete-task
  [{:keys [permalink]} wanted-status]
  (if-not (= "-" wanted-status)
    (.then
     (find-task permalink)
     #(update-task-status % {:wanted-status wanted-status
                             :wanted-group "Completed"}))
    (do
    (js/console.log "Skipping `merged` transition because it's set to \"-\"")
    (js/Promise.resolve))))

(defn cancel-task
  [{:keys [permalink]} wanted-status]
  (if-not (= "-" wanted-status)
    (.then
     (find-task permalink)
     #(update-task-status % {:wanted-status wanted-status
                             :wanted-group "Cancelled"}))
    (do
    (js/console.log "Skipping `closed` transition because it's set to \"-\"")
    (js/Promise.resolve))))
