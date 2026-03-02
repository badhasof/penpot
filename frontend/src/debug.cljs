;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns debug
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.repair :as cfr]
   [app.common.files.validate :as cfv]
   [app.common.json :as json]
   [app.common.logging :as l]
   [app.common.pprint :as pp]
   [app.common.transit :as t]
   [app.common.types.file :as ctf]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dwc]
   [app.main.data.common :as dcm]
   [app.main.data.dashboard.shortcuts]
   [app.main.data.helpers :as dsh]
   [app.main.data.preview :as dp]
   [app.main.data.viewer.shortcuts]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.common :as dwcm]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.data.workspace.path.shortcuts]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.shortcuts]
   [app.main.errors :as errors]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.util.code-gen :as cg]
   [app.util.code-gen.code-apply :as code-apply]
   [app.util.debug :as dbg]
   [app.util.dom :as dom]
   [app.util.http :as http]
   [app.util.object :as obj]
   [app.util.timers :as timers]
   [beicon.v2.core :as rx]
   [cljs.pprint :refer [pprint]]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(l/set-level! :debug)

(defn ^:export set-logging
  ([level]
   (l/set-level! :app (keyword level)))
  ([ns level]
   (l/set-level! (keyword ns) (keyword level))))

;; These events are excluded when we activate the :events flag
(def debug-exclude-events
  #{:app.main.data.workspace.notifications/handle-pointer-update
    :app.main.data.workspace.notifications/handle-pointer-send
    :app.main.data.websocket/send-message
    :app.main.data.workspace.selection/change-hover-state})

(defn enable!
  [option]
  (dbg/enable! option)
  (case option
    :events
    (set! st/*debug-events* true)

    :events-times
    (set! st/*debug-events-time* true)

    nil)
  (js* "app.main.reinit()"))

(defn disable!
  [option]
  (dbg/disable! option)
  (case option
    :events
    (set! st/*debug-events* false)

    :events-times
    (set! st/*debug-events-time* false)

    nil)
  (js* "app.main.reinit()"))

(defn ^:export toggle-debug
  [name]
  (let [option (keyword name)]
    (if (dbg/enabled? option)
      (disable! option)
      (enable! option))))

(defn ^:export debug-all
  []
  (reset! dbg/state dbg/options)
  (js* "app.main.reinit()"))

(defn ^:export debug-none
  []
  (reset! dbg/state #{})
  (js* "app.main.reinit()"))

(defn ^:export tap
  "Transducer function that can execute a side-effect `effect-fn` per input"
  [effect-fn]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (effect-fn input)
       (rf result input)))))

(defn ^:export logjs
  ([str] (tap (partial logjs str)))
  ([str val]
   (js/console.log str (json/->js val))
   val))

(when (exists? js/window)
  (set! (.-dbg ^js js/window) json/->js)
  (set! (.-pp ^js js/window) pprint))

(defonce widget-style "
  background: black;
  bottom: 10px;
  color: white;
  height: 20px;
  padding-left: 8px;
  position: absolute;
  right: 10px;
  width: 40px;
  z-index: 99999;
  opacity: 0.5;
")

(defn ^:export fps
  "Adds a widget to keep track of the average FPS's"
  []
  (let [last (volatile! (.now js/performance))
        avg  (volatile! 0)
        node (-> (.createElement js/document "div")
                 (obj/set! "id" "fps")
                 (obj/set! "style" widget-style))
        body (obj/get js/document "body")

        do-thing (fn do-thing []
                   (timers/raf
                    (fn []
                      (let [cur (.now js/performance)
                            ts (/ 1000 (* (- cur @last)))
                            val (+ @avg (* (- ts @avg) 0.1))]

                        (obj/set! node "innerText" val)
                        (vreset! last cur)
                        (vreset! avg val)
                        (do-thing)))))]

    (.appendChild body node)
    (do-thing)))

(defn ^:export dump-state []
  (logjs "state" @st/state)
  nil)

(defn ^:export dump-data []
  (let [fdata (-> (dsh/lookup-file @st/state)
                  (get :data))]
    (logjs "file-data" fdata)
    nil))

(defn ^:export dump-buffer []
  (logjs "last-events" @st/last-events)
  nil)

(defn ^:export get-state [str-path]
  (let [path (->> (str/split str-path " ")
                  (map d/read-string)
                  vec)]
    (js/console.log (clj->js (get-in @st/state path))))
  nil)

(defn dump-objects'
  [state]
  (let [objects (dsh/lookup-page-objects state)]
    (logjs "objects" objects)
    nil))

(defn ^:export dump-objects
  []
  (dump-objects' @st/state))

(defn get-object
  [state name]
  (let [objects (dsh/lookup-page-objects state)
        result  (or (d/seek (fn [shape] (= name (:name shape))) (vals objects))
                    (get objects (uuid/parse name)))]
    result))

(defn ^:export dump-object
  [name]
  (clj->js (get-object @st/state name)))

(defn get-selected
  [state]
  (dsh/lookup-selected state))

(defn ^:export dump-selected
  []
  (let [objects (dsh/lookup-page-objects @st/state)
        result  (->> (get-selected @st/state) (map #(get objects %)))]
    (logjs "selected" result)
    nil))


(defn ^:export dump-selected-edn
  []
  (let [objects (dsh/lookup-page-objects @st/state)
        result  (->> (get-selected @st/state) (map #(get objects %)))]
    (pp/pprint result {:length 30 :level 30})
    nil))

(defn ^:export preview-selected
  []
  (st/emit! (dp/open-preview-selected)))

(defn ^:export parent
  []
  (let [objects     (dsh/lookup-page-objects @st/state)
        selected-id (first (dsh/get-selected-ids @st/state))
        parent-id   (dm/get-in objects [selected-id :parent-id])]
    (when-let [parent (get objects parent-id)]
      (js/console.log (str (:name parent) " - " (:id parent))))
    nil))

(defn ^:export frame
  []
  (let [objects     (dsh/lookup-page-objects @st/state)
        selected-id (first (dsh/get-selected-ids @st/state))
        frame-id    (dm/get-in objects [selected-id :frame-id])]
    (when-let [frame (get objects frame-id)]
      (js/console.log (str (:name frame) " - " (:id frame))))
    nil))

(defn ^:export select-by-object-id
  [object-id]
  (let [[_ page-id shape-id _] (str/split object-id #"/")]
    (st/emit! (dcm/go-to-workspace :page-id (uuid/parse page-id)))
    (st/emit! (dws/select-shape (uuid/parse shape-id)))))

(defn ^:export select-by-id
  [shape-id]
  (st/emit! (dws/select-shape (uuid/parse shape-id))))

(defn dump-tree'
  ([state] (dump-tree' state false false false))
  ([state show-ids] (dump-tree' state show-ids false false))
  ([state show-ids show-touched] (dump-tree' state show-ids show-touched false))
  ([state show-ids show-touched show-modified]
   (let [page-id    (get state :current-page-id)
         file       (dsh/lookup-file state)
         libraries  (get state :files)]
     (ctf/dump-tree file page-id libraries {:show-ids show-ids
                                            :show-touched show-touched
                                            :show-modified show-modified}))))
(defn ^:export dump-tree
  ([] (dump-tree' @st/state))
  ([show-ids] (dump-tree' @st/state show-ids false false))
  ([show-ids show-touched] (dump-tree' @st/state show-ids show-touched false))
  ([show-ids show-touched show-modified] (dump-tree' @st/state show-ids show-touched show-modified)))

(defn ^:export dump-subtree'
  ([state shape-id] (dump-subtree' state shape-id false false false))
  ([state shape-id show-ids] (dump-subtree' state shape-id show-ids false false))
  ([state shape-id show-ids show-touched] (dump-subtree' state shape-id show-ids show-touched false))
  ([state shape-id show-ids show-touched show-modified]
   (let [page-id    (get state :current-page-id)
         file       (dsh/lookup-file state)
         libraries  (get state :files)
         shape-id   (if (some? shape-id)
                      (uuid/parse shape-id)
                      (first (dsh/lookup-selected state)))]
     (if (some? shape-id)
       (ctf/dump-subtree file page-id shape-id libraries {:show-ids show-ids
                                                          :show-touched show-touched
                                                          :show-modified show-modified})
       (println "no selected shape")))))

(defn ^:export dump-subtree
  ([shape-id] (dump-subtree' @st/state shape-id))
  ([shape-id show-ids] (dump-subtree' @st/state shape-id show-ids false false))
  ([shape-id show-ids show-touched] (dump-subtree' @st/state shape-id show-ids show-touched false))
  ([shape-id show-ids show-touched show-modified] (dump-subtree' @st/state shape-id show-ids show-touched show-modified)))

(defn ^:export apply-changes
  "Takes a Transit JSON changes"
  [^string changes*]

  (let [file-id (:current-file-id @st/state)
        changes (t/decode-str changes*)]
    (st/emit! (dwc/commit-changes {:redo-changes changes
                                   :undo-changes []
                                   :save-undo? true
                                   :file-id file-id}))))

(defn ^:export fetch-apply
  [^string url]
  (-> (p/let [response (js/fetch url)]
        (.text response))
      (p/then apply-changes)))

(defn ^:export reset-viewport
  []
  (st/emit!
   dw/reset-zoom
   (dw/update-viewport-position {:x (constantly 0) :y (constantly 0)})))

(defn ^:export hide-ui
  []
  (st/emit!
   (dw/toggle-layout-flag :hide-ui)))


(defn ^:export shortcuts
  []

  (letfn [(print-shortcuts [shortcuts]
            (.table js/console
                    (->> shortcuts
                         (map (fn [[key {:keys [command]}]]
                                [(d/name key)
                                 (if (vector? command)
                                   (str/join " | " command)
                                   command)]))
                         (into {})
                         (clj->js))))]
    (let [style "font-weight: bold; font-size: 1.25rem;"]
      (.log js/console "%c Dashboard" style)
      (print-shortcuts app.main.data.dashboard.shortcuts/shortcuts)

      (.log js/console "%c Workspace" style)
      (print-shortcuts app.main.data.workspace.shortcuts/shortcuts)

      (.log js/console "%c Path" style)
      (print-shortcuts app.main.data.workspace.path.shortcuts/shortcuts)

      (.log js/console "%c Viewer" style)
      (print-shortcuts app.main.data.viewer.shortcuts/shortcuts)))
  nil)

(defn ^:export nodeStats
  []
  (let [root-node (dom/query ".viewport .render-shapes")
        num-nodes (->> (dom/seq-nodes root-node) count)]
    #js {:number num-nodes}))

(defn modif->js
  [modif-tree objects]
  (clj->js (into {}
                 (map (fn [[k v]]
                        [(get-in objects [k :name]) v]))
                 modif-tree)))

(defn ^:export dump-modifiers
  []
  (let [objects   (dsh/lookup-page-objects @st/state)
        modifiers (:workspace-modifiers @st/state)]
    (js/console.log (modif->js modifiers objects))
    nil))

(defn ^:export set-workspace-read-only
  [read-only?]
  (st/emit! (dwcm/set-workspace-read-only read-only?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPAIR & VALIDATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Validation and repair

(defn ^:export validate
  ([] (validate nil))
  ([shape-id]
   (let [file       (dsh/lookup-file @st/state)
         libraries  (get @st/state :files)]
     (try
       (->> (if-let [shape-id (some-> shape-id uuid/parse)]
              (let [page (dm/get-in file [:data :pages-index (get @st/state :current-page-id)])]
                (cfv/validate-shape shape-id file page libraries))
              (cfv/validate-file file libraries))
            (group-by :code)
            (clj->js))
       (catch :default cause
         (ex/print-throwable cause))))))

(defn ^:export validate-schema
  []
  (try
    (let [file (dsh/lookup-file @st/state)]
      (cfv/validate-file-schema! file))
    (catch :default cause
      (ex/print-throwable cause))))

(defn ^:export repair
  [reload?]
  (st/emit!
   (ptk/reify ::repair-current-file
     ptk/EffectEvent
     (effect [_ state _]
       (let [features (:features state)
             sid      (:session-id state)

             file     (dsh/lookup-file state)
             libs     (get state :files)

             errors   (cfv/validate-file file libs)
             _        (l/dbg :hint "repair current file" :errors (count errors))

             changes  (cfr/repair-file file libs errors)

             params    {:id (:id file)
                        :revn (:revn file)
                        :vern (:vern file)
                        :session-id sid
                        :changes changes
                        :features features
                        :skip-validate true}]

         (->> (rp/cmd! :update-file params)
              (rx/subs! (fn [_]
                          (when reload?
                            (dom/reload-current-window)))
                        (fn [cause]
                          (ex/print-throwable cause)))))))))

(defn ^:export fix-orphan-shapes
  []
  (st/emit! (dw/fix-orphan-shapes)))

(defn ^:export find-components-norefs
  []
  (st/emit! (dw/find-components-norefs)))

(defn- set-shape-ref*
  [id shape-ref]
  (ptk/reify ::set-shape-ref
    ptk/WatchEvent
    (watch [_ _ _]
      (let [shape-id (uuid/parse id)
            shape-ref (uuid/parse shape-ref)]
        (rx/of (dw/update-shape shape-id {:shape-ref shape-ref}))))))

(defn ^:export set-shape-ref
  [id shape-ref]
  (st/emit! (set-shape-ref* id shape-ref)))

(defn ^:export network-averages
  []
  (.log js/console (clj->js @http/network-averages)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS APPLY TOOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-page-css*
  "Generate CSS for all shapes on the current page."
  [state]
  (let [objects    (dsh/lookup-page-objects state)
        all-ids    (->> (keys objects)
                        (remove #(= % uuid/zero)))
        all-shapes (->> (ctst/sort-z-index objects all-ids)
                        (mapv #(get objects %)))]
    (cg/generate-style-code objects "css" [] all-shapes
                            {:with-prelude? false})))

(defn ^:export get-page-css
  "Get current CSS for all shapes on the page.
   Usage: debug.get_page_css()"
  []
  (get-page-css* @st/state))

(defn ^:export get-page-shapes
  "Get shape info (name, id, type, position, size, parent) for all shapes.
   Usage: debug.get_page_shapes()"
  []
  (let [objects (dsh/lookup-page-objects @st/state)]
    (->> (vals objects)
         (remove #(= (:id %) uuid/zero))
         (sort-by (juxt :frame-id :parent-id :id))
         (mapv (fn [s]
                 (let [parent (get objects (:parent-id s))]
                   (dm/str (:name s)
                           " | id:" (subs (str (:id s)) (- (count (str (:id s))) 12))
                           " | type:" (name (:type s))
                           " | x:" (Math/round (:x s))
                           " y:" (Math/round (:y s))
                           " w:" (Math/round (:width s))
                           " h:" (Math/round (:height s))
                           (when parent
                             (dm/str " | parent:" (:name parent)))))))
         (str/join "\n"))))

(defn ^:export move-shape
  "Move a shape to absolute canvas coordinates.
   Usage: debug.move_shape('a7010a0f63a5', 100, 200)"
  [id-suffix x y]
  (let [objects (dsh/lookup-page-objects @st/state)
        shape   (->> (vals objects)
                     (d/seek #(str/ends-with? (str (:id %)) id-suffix)))]
    (if shape
      (do (st/emit! (dwt/update-position (:id shape) {:x x :y y}))
          (clj->js {:ok true :name (:name shape)}))
      (clj->js {:error (str "No shape found ending with " id-suffix)}))))

(defn ^:export apply-css
  "Apply CSS changes to shapes on the page.
   Diffs against current page CSS and applies only changes.
   Usage: debug.apply_css('.Frame-xxxxxxxxxxxx { width: 300px; }')
   Returns: {ok: true, changes: N} or {error: '...'}"
  [css-text]
  (let [state        @st/state
        objects      (dsh/lookup-page-objects state)
        original-css (get-page-css* state)
        result       (code-apply/apply-css-changes css-text original-css objects)]
    (clj->js result)))

(defn print-last-exception
  []
  (some-> errors/last-exception ex/print-throwable))
