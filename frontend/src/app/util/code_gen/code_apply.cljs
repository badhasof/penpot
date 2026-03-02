;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.util.code-gen.code-apply
  "Orchestrates applying CSS edits back to shapes. Parses CSS, diffs against
   current generated CSS, and dispatches shape updates via the existing
   mutation system."
  (:require
   [app.common.data :as d]
   [app.main.data.workspace.colors :as dc]
   [app.main.data.workspace.shape-layout :as dwsl]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.store :as st]
   [app.util.code-gen.style-css-parser :as parser]
   [potok.v2.core :as ptk]))

(defn- apply-direct-attrs
  "Apply simple shape attribute updates via dwsh/update-shapes."
  [shape-id attrs]
  (st/emit!
   (dwsh/update-shapes
    [shape-id]
    (fn [shape]
      (merge shape attrs))
    {:reg-objects? true
     :attrs (set (keys attrs))})))

(defn- apply-dimension-attrs
  "Apply width/height changes via the transform system."
  [shape-id attrs]
  (doseq [[attr value] attrs]
    (st/emit! (dwt/update-dimensions [shape-id] attr value))))

(defn- apply-fill-attrs
  "Apply fill color changes. Updates first fill or adds one."
  [shape-id attrs]
  (let [color {:color (:fill-color attrs)
               :opacity (:fill-opacity attrs 1)}]
    (st/emit! (dc/change-fill [shape-id] color 0 {}))))

(defn- apply-stroke-attrs
  "Apply full stroke (border) replacement."
  [shape-id attrs]
  ;; Update existing first stroke or add one
  (st/emit!
   (dwsh/update-shapes
    [shape-id]
    (fn [shape]
      (if (seq (:strokes shape))
        (assoc-in shape [:strokes 0] (merge (first (:strokes shape)) attrs))
        (assoc shape :strokes [attrs])))
    {:reg-objects? true
     :attrs #{:strokes}})))

(defn- apply-stroke-attr-update
  "Apply partial stroke attribute update (width, style, or color only)."
  [shape-id attrs]
  (st/emit!
   (dwsh/update-shapes
    [shape-id]
    (fn [shape]
      (if (seq (:strokes shape))
        (update-in shape [:strokes 0] merge attrs)
        shape))
    {:reg-objects? true
     :attrs #{:strokes}})))

(defn- apply-shadow-attrs
  "Apply box-shadow changes."
  [shape-id attrs]
  (st/emit!
   (dwsh/update-shapes
    [shape-id]
    (fn [shape]
      (assoc shape :shadow (:shadow attrs)))
    {:attrs #{:shadow}})))

(defn- apply-blur-attrs
  "Apply blur/filter changes."
  [shape-id attrs]
  (st/emit!
   (dwsh/update-shapes
    [shape-id]
    (fn [shape]
      (assoc shape :blur (:blur attrs)))
    {:attrs #{:blur}})))

(defn- apply-radius-attrs
  "Apply border-radius changes."
  [shape-id attrs]
  (st/emit!
   (dwsh/update-shapes
    [shape-id]
    (fn [shape]
      (merge shape attrs))
    {:reg-objects? true
     :attrs #{:r1 :r2 :r3 :r4}})))

(defn- apply-layout-attrs
  "Apply layout container property changes."
  [shape-id attrs]
  (st/emit! (dwsl/update-layout [shape-id] attrs {})))

(defn- apply-layout-child-attrs
  "Apply layout child property changes."
  [shape-id attrs]
  (st/emit! (dwsl/update-layout-child [shape-id] attrs {})))

(defn- apply-single-change
  "Apply a single parsed CSS change to a shape."
  [shape-id {:keys [type attrs]}]
  (case type
    :direct       (apply-direct-attrs shape-id attrs)
    :dimension    (apply-dimension-attrs shape-id attrs)
    :fill         (apply-fill-attrs shape-id attrs)
    :stroke       (apply-stroke-attrs shape-id attrs)
    :stroke-attr  (apply-stroke-attr-update shape-id attrs)
    :shadow       (apply-shadow-attrs shape-id attrs)
    :blur         (apply-blur-attrs shape-id attrs)
    :radius       (apply-radius-attrs shape-id attrs)
    :layout       (apply-layout-attrs shape-id attrs)
    :layout-child (apply-layout-child-attrs shape-id attrs)
    nil))

(defn apply-css-changes
  "Parse edited CSS, diff against original, and apply changes to shapes.
   Returns {:ok true} on success or {:error message} on failure."
  [edited-css original-css objects]
  (try
    (let [edited-blocks (parser/parse-css-blocks edited-css)
          original-blocks (parser/parse-css-blocks original-css)

          ;; Index original blocks by selector for diffing
          original-index (->> original-blocks
                              (map (fn [b] [(:selector b) (:properties b)]))
                              (into {}))

          changes-applied (atom 0)]

      (doseq [{:keys [selector properties]} edited-blocks]
        (when-let [shape (parser/match-selector-to-shape selector objects)]
          (let [original-props (get original-index selector {})
                shape-id (:id shape)]

            ;; For each property in the edited block, check if it changed
            (doseq [[prop value] properties]
              (let [original-value (get original-props prop)]
                (when (not= value original-value)
                  (when-let [change (parser/css-value->shape-attrs prop value)]
                    (apply-single-change shape-id change)
                    (swap! changes-applied inc))))))))

      (if (pos? @changes-applied)
        {:ok true :changes @changes-applied}
        {:ok true :changes 0 :message "No changes detected"}))

    (catch :default e
      {:error (str "Failed to parse CSS: " (.-message e))})))
