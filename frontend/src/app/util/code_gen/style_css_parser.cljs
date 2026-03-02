;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.util.code-gen.style-css-parser
  "Parses generated CSS text back into structured data and maps CSS property
   values to shape attribute updates. This is the inverse of style-css-values
   and style-css-formats."
  (:require
   [app.common.colors :as cc]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [cuerdas.core :as str]))

(declare parse-tracks)

;; --- CSS Text Parsing ---

(defn- parse-property-line
  "Parse a single CSS property line like 'width: 200px' into [prop value]."
  [line]
  (let [line (str/trim line)]
    (when (and (seq line) (not (str/starts-with? line "/*")))
      (let [colon-idx (str/index-of line ":")]
        (when (and colon-idx (pos? colon-idx))
          (let [prop (str/trim (subs line 0 colon-idx))
                value (-> (subs line (inc colon-idx))
                          (str/trim)
                          (str/rtrim ";")
                          (str/trim))]
            (when (and (seq prop) (seq value))
              [prop value])))))))

(defn parse-css-blocks
  "Parse CSS text into a seq of {:selector s :properties {prop value}}.
   Only handles single class selectors (what our code gen produces)."
  [css-text]
  (let [;; Split on closing braces, process each block
        blocks (str/split css-text "}")]
    (->> blocks
         (keep
          (fn [block]
            (let [block (str/trim block)]
              (when (seq block)
                (let [brace-idx (str/index-of block "{")]
                  (when brace-idx
                    (let [selector-raw (-> (subs block 0 brace-idx)
                                          (str/trim)
                                          ;; Strip leading dot and any > svg suffix
                                          (str/replace #"^\." "")
                                          (str/trim))
                          body (subs block (inc brace-idx))
                          lines (str/split body "\n")
                          properties (->> lines
                                         (keep parse-property-line)
                                         (into {}))]
                      (when (and (seq selector-raw) (seq properties))
                        {:selector selector-raw
                         :properties properties})))))))))))

;; --- Selector to Shape Matching ---

(defn- extract-uuid-suffix
  "Extract the UUID suffix (last 12 chars) from a CSS selector.
   The selector format is: 'name-XXXXXXXXXXXX' where X is the UUID suffix."
  [selector]
  ;; The selector is kebab-cased. The UUID suffix is the last 12 hex chars
  ;; appended after the name portion. We extract by taking the last segment.
  (let [clean (str/replace selector #"-wrapper$" "")
        ;; UUID chars are [a-f0-9], the suffix is always 12 chars
        match (re-find #"([a-f0-9]{12})$" clean)]
    (when match (second match))))

(defn match-selector-to-shape
  "Given a selector string and a map of objects, find the shape whose UUID
   ends with the suffix encoded in the selector."
  [selector objects]
  (when-let [suffix (extract-uuid-suffix selector)]
    (->> (vals objects)
         (d/seek #(str/ends-with? (dm/str (:id %)) suffix)))))

;; --- CSS Value Parsing ---

(defn- parse-pixels
  "Parse a CSS pixel value like '200px' into a number. Returns nil if not a px value."
  [value-str]
  (when-let [match (re-find #"^(-?\d+\.?\d*)px$" (str/trim value-str))]
    (mth/round (js/parseFloat (second match)))))

(defn- parse-number
  "Parse a plain number string."
  [value-str]
  (let [n (js/parseFloat (str/trim value-str))]
    (when-not (js/isNaN n) n)))

(defn- parse-size-value
  "Parse a CSS size value: 'Npx' -> number, '100%' -> :fill, 'auto' -> :auto."
  [value-str]
  (let [v (str/trim value-str)]
    (cond
      (= v "100%") :fill
      (= v "auto") :auto
      :else (parse-pixels v))))

(defn- parse-size-array
  "Parse space-separated pixel values like '8px 12px 8px 12px' into a vector of numbers."
  [value-str]
  (let [parts (str/split (str/trim value-str) #"\s+")]
    (mapv #(or (parse-pixels %) 0) parts)))

(defn- parse-hex-color
  "Parse a hex color string into {:fill-color hex :fill-opacity opacity}.
   Handles #RGB, #RRGGBB, #RRGGBBAA formats."
  [hex-str]
  (let [hex (str/trim hex-str)]
    (cond
      ;; #RRGGBBAA format (Penpot outputs uppercase AA suffix for opacity)
      (and (str/starts-with? hex "#") (= (count hex) 9))
      (let [color (subs hex 0 7)
            alpha-hex (subs hex 7 9)
            alpha (/ (js/parseInt alpha-hex 16) 255)]
        {:fill-color color :fill-opacity (mth/precision alpha 2)})

      ;; Standard #RRGGBB or #RGB
      (str/starts-with? hex "#")
      {:fill-color hex :fill-opacity 1}

      :else nil)))

(defn- parse-rgba-color
  "Parse rgba(r, g, b, a) into {:fill-color hex :fill-opacity a}."
  [value-str]
  (when-let [match (re-find #"rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([0-9.]+))?\s*\)" value-str)]
    (let [r (js/parseInt (nth match 1))
          g (js/parseInt (nth match 2))
          b (js/parseInt (nth match 3))
          a (if (nth match 4) (js/parseFloat (nth match 4)) 1)]
      {:fill-color (cc/rgb->hex [r g b])
       :fill-opacity a})))

(defn- parse-color
  "Parse any CSS color format we generate into {:fill-color :fill-opacity}."
  [value-str]
  (let [v (str/trim value-str)]
    (cond
      (str/starts-with? v "rgba") (parse-rgba-color v)
      (str/starts-with? v "rgb")  (parse-rgba-color v)
      (str/starts-with? v "#")    (parse-hex-color v)
      :else nil)))

(defn- parse-blur
  "Parse 'blur(Npx)' into a number."
  [value-str]
  (when-let [match (re-find #"blur\((\d+\.?\d*)px\)" value-str)]
    (js/parseFloat (second match))))

(defn- parse-border-shorthand
  "Parse 'Npx style color' into {:stroke-width N :stroke-style kw :stroke-color hex :stroke-opacity a}."
  [value-str]
  (when-let [match (re-find #"^(\d+\.?\d*)px\s+(\w+)\s+(.+)$" (str/trim value-str))]
    (let [width (js/parseFloat (nth match 1))
          style (keyword (nth match 2))
          color-parsed (parse-color (nth match 3))]
      (when color-parsed
        {:stroke-width width
         :stroke-style style
         :stroke-color (:fill-color color-parsed)
         :stroke-opacity (:fill-opacity color-parsed)
         :stroke-alignment :inner}))))

(defn- parse-shadow-single
  "Parse a single box-shadow value like 'inset 2px 4px 6px 0px #000000'."
  [shadow-str]
  (let [s (str/trim shadow-str)
        inset? (str/starts-with? s "inset")
        s (if inset? (str/trim (subs s 5)) s)
        ;; Match: Xpx Ypx Bpx Spx color
        match (re-find #"^(-?\d+\.?\d*)px\s+(-?\d+\.?\d*)px\s+(\d+\.?\d*)px\s+(\d+\.?\d*)px\s+(.+)$" s)]
    (when match
      (let [color-parsed (parse-color (nth match 5))]
        (when color-parsed
          {:style (if inset? :inner-shadow :drop-shadow)
           :offset-x (js/parseFloat (nth match 1))
           :offset-y (js/parseFloat (nth match 2))
           :blur (js/parseFloat (nth match 3))
           :spread (js/parseFloat (nth match 4))
           :color {:color (:fill-color color-parsed)
                   :opacity (:fill-opacity color-parsed)}
           :hidden false})))))

(defn- parse-box-shadow
  "Parse a full box-shadow CSS value (comma-separated) into a vector of shadow maps."
  [value-str]
  (let [parts (str/split value-str ",")]
    (->> parts
         (keep parse-shadow-single)
         (vec))))

(defn- parse-border-radius-value
  "Parse border radius: 'Npx' or space-separated 'N1px N2px N3px N4px' or '50%'."
  [value-str]
  (let [v (str/trim value-str)]
    (cond
      (= v "50%") :circle
      :else (let [parts (str/split v #"\s+")]
              (if (= 1 (count parts))
                (parse-pixels (first parts))
                (mapv #(or (parse-pixels %) 0) parts))))))

;; --- CSS Property -> Shape Update Mapping ---

(defn css-value->shape-attrs
  "Given a CSS property name and value string, return a map of shape attributes
   to update, or nil if the property is not supported for reverse mapping.
   Returns {:type :direct|:dimension|:fill|:stroke|:layout|:layout-child|:shadow|:blur
            :attrs {shape-attr-key value}}"
  [property value-str]
  (case property
    ;; --- Opacity ---
    "opacity"
    (when-let [v (parse-number value-str)]
      {:type :direct :attrs {:opacity (min 1 (max 0 v))}})

    ;; --- Mix Blend Mode ---
    "mix-blend-mode"
    {:type :direct :attrs {:blend-mode (keyword value-str)}}

    ;; --- Visibility ---
    "display"
    (cond
      (= value-str "none") {:type :direct :attrs {:hidden true}}
      (= value-str "flex") {:type :layout :attrs {:layout :flex}}
      (= value-str "grid") {:type :layout :attrs {:layout :grid}}
      :else nil)

    ;; --- Dimensions ---
    "width"
    (let [v (parse-size-value value-str)]
      (cond
        (number? v) {:type :dimension :attrs {:width v}}
        (= v :fill) {:type :direct :attrs {:layout-item-h-sizing :fill}}
        (= v :auto) {:type :direct :attrs {:layout-item-h-sizing :auto}}
        :else nil))

    "height"
    (let [v (parse-size-value value-str)]
      (cond
        (number? v) {:type :dimension :attrs {:height v}}
        (= v :fill) {:type :direct :attrs {:layout-item-v-sizing :fill}}
        (= v :auto) {:type :direct :attrs {:layout-item-v-sizing :auto}}
        :else nil))

    ;; --- Min/Max Dimensions ---
    "max-width"
    (when-let [v (parse-pixels value-str)]
      {:type :direct :attrs {:layout-item-max-w v}})

    "min-width"
    (when-let [v (parse-pixels value-str)]
      {:type :direct :attrs {:layout-item-min-w v}})

    "max-height"
    (when-let [v (parse-pixels value-str)]
      {:type :direct :attrs {:layout-item-max-h v}})

    "min-height"
    (when-let [v (parse-pixels value-str)]
      {:type :direct :attrs {:layout-item-min-h v}})

    ;; --- Border Radius ---
    "border-radius"
    (let [parsed (parse-border-radius-value value-str)]
      (cond
        (= parsed :circle) nil ;; can't change shape type via CSS
        (number? parsed) {:type :radius :attrs {:r1 parsed :r2 parsed :r3 parsed :r4 parsed}}
        (vector? parsed) (let [[r1 r2 r3 r4] parsed]
                           {:type :radius :attrs {:r1 (or r1 0) :r2 (or r2 0) :r3 (or r3 0) :r4 (or r4 0)}})
        :else nil))

    "border-start-start-radius"
    (when-let [v (parse-pixels value-str)]
      {:type :radius :attrs {:r1 v}})

    "border-start-end-radius"
    (when-let [v (parse-pixels value-str)]
      {:type :radius :attrs {:r2 v}})

    "border-end-start-radius"
    (when-let [v (parse-pixels value-str)]
      {:type :radius :attrs {:r3 v}})

    "border-end-end-radius"
    (when-let [v (parse-pixels value-str)]
      {:type :radius :attrs {:r4 v}})

    ;; --- Background (solid colors only) ---
    "background"
    (when-let [color (parse-color value-str)]
      {:type :fill :attrs color})

    ;; --- Border ---
    "border"
    (when-let [stroke (parse-border-shorthand value-str)]
      {:type :stroke :attrs stroke})

    "border-width"
    (when-let [v (parse-pixels value-str)]
      {:type :stroke-attr :attrs {:stroke-width v}})

    "border-style"
    {:type :stroke-attr :attrs {:stroke-style (keyword value-str)}}

    "border-color"
    (when-let [color (parse-color value-str)]
      {:type :stroke-attr :attrs {:stroke-color (:fill-color color)
                                  :stroke-opacity (:fill-opacity color)}})

    ;; --- Shadows ---
    "box-shadow"
    (let [shadows (parse-box-shadow value-str)]
      (when (seq shadows)
        {:type :shadow :attrs {:shadow shadows}}))

    ;; --- Blur ---
    "filter"
    (when-let [v (parse-blur value-str)]
      {:type :blur :attrs {:blur {:type :layer-blur :value v :hidden false}}})

    ;; --- Overflow ---
    "overflow"
    {:type :direct :attrs {:show-content (not= value-str "hidden")}}

    ;; --- Flex Container ---
    "flex-direction"
    {:type :layout :attrs {:layout-flex-dir (keyword value-str)}}

    "flex-wrap"
    {:type :layout :attrs {:layout-wrap-type (keyword value-str)}}

    "align-items"
    {:type :layout :attrs {:layout-align-items (keyword value-str)}}

    "align-content"
    {:type :layout :attrs {:layout-align-content (keyword value-str)}}

    "justify-content"
    {:type :layout :attrs {:layout-justify-content (keyword value-str)}}

    "justify-items"
    {:type :layout :attrs {:layout-justify-items (keyword value-str)}}

    ;; --- Flex Child ---
    "align-self"
    {:type :layout-child :attrs {:layout-item-align-self (keyword value-str)}}

    ;; --- Gap ---
    "gap"
    (let [parts (parse-size-array value-str)]
      (when (seq parts)
        (let [row-gap (first parts)
              col-gap (or (second parts) row-gap)]
          {:type :layout :attrs {:layout-gap {:row-gap row-gap :column-gap col-gap}}})))

    "row-gap"
    (when-let [v (parse-pixels value-str)]
      {:type :layout :attrs {:layout-gap {:row-gap v}}})

    "column-gap"
    (when-let [v (parse-pixels value-str)]
      {:type :layout :attrs {:layout-gap {:column-gap v}}})

    ;; --- Padding ---
    "padding"
    (let [parts (parse-size-array value-str)]
      (when (seq parts)
        (let [result (case (count parts)
                       1 {:p1 (nth parts 0) :p2 (nth parts 0) :p3 (nth parts 0) :p4 (nth parts 0)}
                       2 {:p1 (nth parts 0) :p2 (nth parts 1) :p3 (nth parts 0) :p4 (nth parts 1)}
                       3 {:p1 (nth parts 0) :p2 (nth parts 1) :p3 (nth parts 2) :p4 (nth parts 1)}
                       4 {:p1 (nth parts 0) :p2 (nth parts 1) :p3 (nth parts 2) :p4 (nth parts 3)}
                       nil)]
          (when result
            {:type :layout :attrs {:layout-padding result}}))))

    "padding-block-start"
    (when-let [v (parse-pixels value-str)]
      {:type :layout :attrs {:layout-padding {:p1 v}}})

    "padding-inline-end"
    (when-let [v (parse-pixels value-str)]
      {:type :layout :attrs {:layout-padding {:p2 v}}})

    "padding-block-end"
    (when-let [v (parse-pixels value-str)]
      {:type :layout :attrs {:layout-padding {:p3 v}}})

    "padding-inline-start"
    (when-let [v (parse-pixels value-str)]
      {:type :layout :attrs {:layout-padding {:p4 v}}})

    ;; --- Margin ---
    "margin"
    (let [parts (parse-size-array value-str)]
      (when (seq parts)
        (let [result (case (count parts)
                       1 {:m1 (nth parts 0) :m2 (nth parts 0) :m3 (nth parts 0) :m4 (nth parts 0)}
                       2 {:m1 (nth parts 0) :m2 (nth parts 1) :m3 (nth parts 0) :m4 (nth parts 1)}
                       3 {:m1 (nth parts 0) :m2 (nth parts 1) :m3 (nth parts 2) :m4 (nth parts 1)}
                       4 {:m1 (nth parts 0) :m2 (nth parts 1) :m3 (nth parts 2) :m4 (nth parts 3)}
                       nil)]
          (when result
            {:type :layout-child :attrs {:layout-item-margin result}}))))

    "margin-block-start"
    (when-let [v (parse-pixels value-str)]
      {:type :layout-child :attrs {:layout-item-margin {:m1 v}}})

    "margin-inline-end"
    (when-let [v (parse-pixels value-str)]
      {:type :layout-child :attrs {:layout-item-margin {:m2 v}}})

    "margin-block-end"
    (when-let [v (parse-pixels value-str)]
      {:type :layout-child :attrs {:layout-item-margin {:m3 v}}})

    "margin-inline-start"
    (when-let [v (parse-pixels value-str)]
      {:type :layout-child :attrs {:layout-item-margin {:m4 v}}})

    ;; --- Z-Index ---
    "z-index"
    (when-let [v (parse-number value-str)]
      {:type :direct :attrs {:layout-item-z-index (int v)}})

    ;; --- Grid Container ---
    ;; Grid track changes are skipped — removing/reordering tracks can crash
    ;; the layout engine when elements are assigned to removed tracks.
    "grid-template-rows" nil
    "grid-template-columns" nil
    "grid-auto-flow" nil

    ;; --- Unsupported properties (position, transform, left, top, etc.) ---
    nil))

(defn- parse-tracks
  "Parse CSS track syntax like '1fr 200px auto 50%' into Penpot track format."
  [value-str]
  (let [parts (str/split (str/trim value-str) #"\s+")]
    (->> parts
         (keep
          (fn [part]
            (cond
              (str/ends-with? part "fr")
              {:type :flex :value (js/parseFloat (subs part 0 (- (count part) 2)))}

              (str/ends-with? part "%")
              {:type :percent :value (* 100 (/ (js/parseFloat (subs part 0 (- (count part) 1))) 100))}

              (= part "auto")
              {:type :auto :value nil}

              (str/ends-with? part "px")
              {:type nil :value (js/parseFloat (subs part 0 (- (count part) 2)))}

              :else nil)))
         (vec))))
