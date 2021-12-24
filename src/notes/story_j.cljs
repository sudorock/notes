(ns notes.story-j)

(ns akarmic.akarma-web.story-j
  (:require
   [cljs.spec.alpha :as s]
   [akarmic.akarma-web.components.text-field :refer [text-field]]
   [reagent.core :as r]
   [reagent.dom :as dom]
   [spec-tools.core :as st]
   [spec-tools.visitor :as vt])
  (:require-macros
   [akarmic.akarma-web.story-j :refer [base]]))

(require '[spec-tools.json-schema :as j])

(comment
 (vt)
 (s/def ::id string?)
 (s/def ::name string?)
 (s/def ::street string?)
 (s/def ::city #{:tre :hki})
 (s/def ::address (s/keys :req-un [::street ::city]))
 (s/def ::user (s/keys :req-un [::id ::name ::address]))

 ;; visitor to recursively collect all registered spec forms
 (let [specs (atom {})]
   (vt/visit
    ::user
    (fn [a spec b c]
      (cljs.pprint/pprint {:a a :b b :c c :spec spec})
      (if-let [s (s/get-spec spec)]
        (swap! specs assoc spec (s/form s))
        @specs)))))

(defn pathwalk [f path e]
  (let [e' (f path e)]
    (cond
      (map? e') (->> e'
                     (map (fn [[k x]] [k (pathwalk f (conj path k) x)]))
                     (into (empty e')))
      (coll? e') (->> e'
                      (map-indexed (fn [i x] (pathwalk f (conj path i) x)))
                      (into (empty e')))
      :else e')))


(pathwalk (fn [p x]
            (prn p x)
            x)
          []
          {:a {:b 90 :z [0 1]}})
(comment
 (j/transform ::user)
 (vt/visit ::props (fn [& args]
                     (prn args)))
 (:type ::props)
 (st/walk ::props))
(comment
 (s/def ::on-change fn?)
 (s/form ::on-change)
 (s/def ::on (s/nilable fn?))
 (s/def ::name (s/nilable string?))


 (s/get-spec ::name)

 (s/def ::props (s/keys :req-un [::name]))
 (s/form ::props)
 (s/conform ::props {:on-change (fn []) :a 90})
 ;#_(s/ ::props {:on-change (fn []) :a 90})

 (s/describe ::props)

 (s/describe ::on-change)
 (= '(nilable fn?) (s/describe ::on))

 s
 (macroexpand-1 '(base {:component  text-field
                        :prop-specs [::props]
                        :args       [{:on-change (fn [x] (prn "hel"))}]})))

(def base {:component text-field
           :title     :components/text-field
           :arg-specs {:label string?
                       :value string?
                       :x     number?
                       :hint  string?}})

(defmulti spec->form (fn [spec _state]))
(defmulti spec->form (fn [spec _state]))

(defn story
  [{:keys                         [args]
    {:keys [component arg-specs]} :base}]
  (let [state (r/atom {})]
    {:component (fn [] [component @state])
     :forms     (reduce-kv (fn [m k v]
                             (let [type (cond
                                          (= v string?) "text"
                                          (= v number?) "number"
                                          :else "text")]
                               (swap! state assoc k (get args k))
                               (assoc m k (fn []
                                            [:input {:type      type
                                                     :value     (get @state k)
                                                     :on-change (fn [e]
                                                                  (swap! state assoc k (-> e .-target .-value)))}]))))
                           {}
                           arg-specs)}))



(def s (story {:base base
               :args {:value "Sunil"
                      :label "Name"
                      :hint  "Some hint"}}))


(defn render
  [{:keys [component forms state]}]
  [:div
   [component]
   (into [:div]
         (vec (for [[k v] forms]
                [:div
                 [:h1 k]
                 [v]])))])


(comment
 (dom/render (render s) #_[:div "what"] (js/document.getElementById "app")))
