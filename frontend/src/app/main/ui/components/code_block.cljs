;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.components.code-block
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.util.dom :as dom]
   [app.util.modules :as modules]
   [cuerdas.core :as str]
   [promesa.core :as p]
   [rumext.v2 :as mf]))

(def highlight-fn
  (delay (modules/load-fn 'app.util.code-highlight/highlight!)))

(mf/defc code-block
  {::mf/wrap-props false}
  [{:keys [code type]}]
  (let [block-ref (mf/use-ref)
        code      (str/trim code)]

    (mf/with-effect [code type]
      (when-let [node (mf/ref-val block-ref)]
        (->> @highlight-fn
             (p/fmap (fn [f] (f)))
             (p/fnly (fn [f cause]
                       (if cause
                         (js/console.error cause)
                         (f node)))))))

    [:pre {:class (dm/str type " " (stl/css :code-display)) :ref block-ref} code]))

(mf/defc editable-code-block*
  {::mf/wrap-props false}
  [{:keys [code type on-change on-apply]}]
  (let [textarea-ref (mf/use-ref)
        code         (str/trim code)

        handle-change
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (let [value (dom/get-target-val event)]
             (when on-change
               (on-change value)))))

        handle-key-down
        (mf/use-fn
         (mf/deps on-apply)
         (fn [event]
           ;; Cmd/Ctrl+Enter to apply
           (when (and (= (.-key event) "Enter")
                      (or (.-metaKey event) (.-ctrlKey event)))
             (dom/prevent-default event)
             (dom/stop-propagation event)
             (when on-apply (on-apply)))))]

    [:textarea {:class (stl/css :code-editor)
                :ref textarea-ref
                :default-value code
                :spell-check false
                :auto-correct "off"
                :auto-capitalize "off"
                :on-change handle-change
                :on-key-down handle-key-down}]))

