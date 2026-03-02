;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.util.code-gen.code-apply
  "Orchestrates applying CSS edits back to shapes. Parses CSS, diffs against
   current generated CSS, and dispatches batched shape updates via the existing
   mutation system."
  (:require
   [app.common.data :as d]
   [app.main.data.workspace.colors :as dc]
   [app.main.data.workspace.shape-layout :as dwsl]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.store :as st]
   [app.util.code-gen.style-css-parser :as parser]))

;; ---------------------------------------------------------------------------
;; Per-type batch apply functions
;; ---------------------------------------------------------------------------

(defn- apply-direct-batch
  "Merge all direct attribute maps and apply once."
  [shape-id attrs-list]
  (let [merged (reduce merge {} attrs-list)]
    (st/emit!
     (dwsh/update-shapes
      [shape-id]
      (fn [shape] (merge shape merged))
      {:reg-objects? true
       :attrs (set (keys merged))}))))

(defn- apply-dimension-batch
  "Apply dimension changes (width/height)."
  [shape-id attrs-list]
  (let [merged (reduce merge {} attrs-list)]
    (doseq [[attr value] merged]
      (st/emit! (dwt/update-dimensions [shape-id] attr value)))))

(defn- apply-fill-batch
  "Apply the last fill change (only one fill at a time makes sense)."
  [shape-id attrs-list]
  (let [attrs (last attrs-list)
        color {:color (:fill-color attrs)
               :opacity (:fill-opacity attrs 1)}]
    (st/emit! (dc/change-fill [shape-id] color 0 {}))))

(defn- apply-stroke-batch
  "Apply the last full stroke replacement."
  [shape-id attrs-list]
  (let [attrs (last attrs-list)]
    (st/emit!
     (dwsh/update-shapes
      [shape-id]
      (fn [shape]
        (if (seq (:strokes shape))
          (assoc-in shape [:strokes 0] (merge (first (:strokes shape)) attrs))
          (assoc shape :strokes [attrs])))
      {:reg-objects? true
       :attrs #{:strokes}}))))

(defn- apply-stroke-attr-batch
  "Merge all partial stroke attribute updates and apply once."
  [shape-id attrs-list]
  (let [merged (reduce merge {} attrs-list)]
    (st/emit!
     (dwsh/update-shapes
      [shape-id]
      (fn [shape]
        (if (seq (:strokes shape))
          (update-in shape [:strokes 0] merge merged)
          shape))
      {:reg-objects? true
       :attrs #{:strokes}}))))

(defn- apply-shadow-batch
  "Apply the last shadow change."
  [shape-id attrs-list]
  (let [attrs (last attrs-list)]
    (st/emit!
     (dwsh/update-shapes
      [shape-id]
      (fn [shape] (assoc shape :shadow (:shadow attrs)))
      {:attrs #{:shadow}}))))

(defn- apply-blur-batch
  "Apply the last blur change."
  [shape-id attrs-list]
  (let [attrs (last attrs-list)]
    (st/emit!
     (dwsh/update-shapes
      [shape-id]
      (fn [shape] (assoc shape :blur (:blur attrs)))
      {:attrs #{:blur}}))))

(defn- apply-radius-batch
  "Merge all radius attributes and apply once."
  [shape-id attrs-list]
  (let [merged (reduce merge {} attrs-list)]
    (st/emit!
     (dwsh/update-shapes
      [shape-id]
      (fn [shape] (merge shape merged))
      {:reg-objects? true
       :attrs #{:r1 :r2 :r3 :r4}}))))

(defn- apply-layout-batch
  "Deep-merge all layout attributes and apply as a single update."
  [shape-id attrs-list]
  (let [merged (reduce (fn [acc attrs]
                         (reduce-kv
                          (fn [m k v]
                            (if (and (map? v) (map? (get m k)))
                              (update m k merge v)
                              (assoc m k v)))
                          acc attrs))
                       {} attrs-list)]
    (st/emit! (dwsl/update-layout [shape-id] merged {}))))

(defn- apply-layout-child-batch
  "Deep-merge all layout child attributes and apply as a single update."
  [shape-id attrs-list]
  (let [merged (reduce (fn [acc attrs]
                         (reduce-kv
                          (fn [m k v]
                            (if (and (map? v) (map? (get m k)))
                              (update m k merge v)
                              (assoc m k v)))
                          acc attrs))
                       {} attrs-list)]
    (st/emit! (dwsl/update-layout-child [shape-id] merged {}))))

(defn- apply-batched-changes
  "Apply a batch of changes (same type) for a single shape."
  [shape-id change-type attrs-list]
  (case change-type
    :direct       (apply-direct-batch shape-id attrs-list)
    :dimension    (apply-dimension-batch shape-id attrs-list)
    :fill         (apply-fill-batch shape-id attrs-list)
    :stroke       (apply-stroke-batch shape-id attrs-list)
    :stroke-attr  (apply-stroke-attr-batch shape-id attrs-list)
    :shadow       (apply-shadow-batch shape-id attrs-list)
    :blur         (apply-blur-batch shape-id attrs-list)
    :radius       (apply-radius-batch shape-id attrs-list)
    :layout       (apply-layout-batch shape-id attrs-list)
    :layout-child (apply-layout-child-batch shape-id attrs-list)
    nil))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defn apply-css-changes
  "Parse edited CSS, diff against original, and apply changes to shapes.
   Batches all changes per shape per type to minimize event dispatches.
   Returns {:ok true :changes N} on success or {:error message} on failure."
  [edited-css original-css objects]
  (try
    (let [edited-blocks (parser/parse-css-blocks edited-css)
          original-blocks (parser/parse-css-blocks original-css)

          ;; Index original blocks by selector for diffing
          original-index (->> original-blocks
                              (map (fn [b] [(:selector b) (:properties b)]))
                              (into {}))

          ;; Collect all changes grouped by [shape-id change-type]
          all-changes (atom {})
          changes-count (atom 0)]

      (doseq [{:keys [selector properties]} edited-blocks]
        (when-let [shape (parser/match-selector-to-shape selector objects)]
          (let [original-props (get original-index selector {})
                shape-id (:id shape)]

            (doseq [[prop value] properties]
              (let [original-value (get original-props prop)]
                (when (not= value original-value)
                  (when-let [{:keys [type attrs]} (parser/css-value->shape-attrs prop value)]
                    (swap! all-changes update [shape-id type] (fnil conj []) attrs)
                    (swap! changes-count inc))))))))

      ;; Now dispatch batched changes — one call per [shape-id, type]
      (doseq [[[shape-id change-type] attrs-list] @all-changes]
        (apply-batched-changes shape-id change-type attrs-list))

      (if (pos? @changes-count)
        {:ok true :changes @changes-count}
        {:ok true :changes 0 :message "No changes detected"}))

    (catch :default e
      {:error (str "Failed to parse CSS: " (.-message e))})))
