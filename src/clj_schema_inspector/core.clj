(ns clj-schema-inspector.core
  (:require [schema.core :as s]))

(def big-number 30)
(def rare-threshold 0.5)

(def coertions
  {String s/Str
   clojure.lang.Keyword s/Str
   Double s/Num
   java.math.BigInteger s/Num
   Long s/Num
   Boolean s/Bool
   java.util.regex.Pattern s/Regex
   java.util.Date s/Inst
   java.util.UUID s/Uuid})

(def str->cl
  {"s/Str" s/Str
   "s/Num" s/Num
   "s/Bool" s/Bool
   "s/Regex" s/Regex
   "s/Inst" s/Inst
   "s/Uuid" s/Uuid
   "s/maybe" 's/maybe
   "s/either" 's/either
   "s/both" 's/both})

(def cl->str
  {"Num" "s/Num"
   "Str" "s/Str"
   "Bool" "s/Bool"
   "Regex" "s/Regex"
   "Inst" "s/Inst"
   "Uuid" "s/Uuid"
   "maybe" "s/maybe"
   "either" "s/either"
   "both" "s/both"
   "s/Num" "s/Num"
   "s/Str" "s/Str"
   "s/Bool" "s/Bool"
   "s/Regex" "s/Regex"
   "s/Inst" "s/Inst"
   "s/Uuid" "s/Uuid"
   "s/maybe" "s/maybe"
   "s/either" "s/either"
   "s/both" "s/both"
   Number "s/Num"
   String "s/Str"
   clojure.lang.Keyword "s/Str"
   Double "s/Num"
   java.math.BigInteger "s/Num"
   Long "s/Num"
   Boolean "s/Bool"
   java.util.regex.Pattern "s/Regex"
   java.util.Date "s/Inst"
   java.util.UUID "s/Uuid"})

(defn m-class [obj]
  (if (nil? obj)
    s/Any
    (let [cl (class obj)
          res (get coertions cl)]
      (if (nil? res)
        (throw (UnsupportedOperationException. (.getName cl)))
        res))))

(defn to-set [s]
  (if (set? s)
    s
    (if (sequential? s)
      (set s)
      #{s})))

(defn polish-types [s]
  (let [res (if (and (contains? s s/Num) (contains? s s/Int))
              (disj s s/Int)
              s)]
    (if (= 1 (count res))
      (first res)
      (into #{} res))))

(defn inferred-type [t1 t2]
  (if (= s/Any t1)
    s/Any
    (if (= t1 t2)
      t1
      (let [at1 (to-set t1)
            at2 (to-set t2)
            res (into #{} (concat at1 at2))]
        (polish-types res)))))

(declare flat-structure)

(defn flat-sequence [prefix suffix item]
  (let [vs (map #(flat-structure suffix %) item)
        sub-m
        (apply merge-with
               (fn [m1 m2]
                 {:count (max (:count m1) (:count m2))
                  :type (inferred-type (:type m1) (:type m2))
                  :mode nil
                  :values (if (= :not-enum (:values m1))
                            :not-enum
                            (merge-with + (:values m1) (:values m2)))})
               vs)]
    (apply merge
           (map #(hash-map (into [] (concat prefix (key %)))
                           (val %))
                sub-m))))

(defn flat-structure 
  ([item] (flat-structure [] item))
  ([prefix item]
   (if (map? item)
     (into {} (mapcat #(flat-structure
                        (conj prefix (key %)) (val %))
                      item))
     (if (sequential? item)
       (flat-sequence prefix [(pr-str (empty item))] item)
       {prefix {:values {item 1}
                :type (m-class item)
                :mode nil
                :count 1}}))))

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn merge-maps [orig new]
  (if (= {} orig)
    new
    (let [type (inferred-type (:type orig) (:type new))
          count (+ (:count orig) (:count new))
          values (if (= (:values orig) :not-enum)
                   :not-enum
                   (merge-with + (:values orig) (:values new)))]
      {:type type
       :values values
       :mode nil
       :count count})))

(defn polish-record [c m k]
  (let [v (get m k)
        ratio (double (/ (:count v) c))
        mode (if (== 1 ratio)
               :required
               (if (>= ratio rare-threshold) :optional :rare))
        res (if (= (:values v) :not-enum)
              m
              (let [fs (map #(double (/ % c)) (vals (:values v)))
                    med (median fs)]
                (if (and (> c big-number) (<= med 0.1))
                  (assoc-in m [k :values] :not-enum) m)))]
    (assoc-in res [k :mode] mode)))

(defn polish-values [c m]
  (reduce #(polish-record c %1 %2) m (keys m)))

(defn add-map [p n]
  (let [prev-m (:m p)
        prev-count (get p :count 0)
        new-m (flat-structure n)
        res (merge-with merge-maps prev-m new-m)]
    {:count (inc prev-count)
     :m (polish-values (inc prev-count) res)}))

(defn inferred-schema-keys [ms]
  (reduce add-map {} ms))

(defn inferred-branch [m branch]
  (assoc-in m (key branch) (val branch)))

(defn fixed-sequences [m]
  (if (and (map? m) (not (contains? m :type))
           (not (contains? m :values)) (not (contains? m :mode)))
    (let [ks (keys m)]
      (reduce (fn [sub-m k]
                (if (and (string? k)
                         (or (= k "[]") (= k "#{}") (= k "()")))
                  (let [struct (read-string k)]
                    (into struct [(fixed-sequences (get sub-m k))]))
                  (assoc sub-m k (fixed-sequences (get sub-m k)))))
              m ks))
    (let [t (:type m)
          tt (if (set? t) (apply s/either t) t)
          ps (if (= (:mode m) :required) tt (s/maybe tt))]
      (s/explain ps))))

(defn inferred-parsed-schema [schema]
  (fixed-sequences (reduce inferred-branch {} (:m schema))))

(defn x-parse [f m]
  (if (map? m)
    (zipmap (keys m) (map #(x-parse f %) (vals m)))
    (if (vector? m)
      (into [] (map #(x-parse f %) m))
      (if (seq? m)
        (let [ff (first m)]
          (if (symbol? ff)
            (concat [(x-parse f (first m))] (map #(x-parse f %) (rest m)))
            (map #(x-parse f %) m)))
        (if (set? m)
          (into #{} (map #(x-parse f %) m))
          (if (number? m)
            m
            (if (keyword? m)
              m
              (if (class? m)
                (f m)
                (if (string? m)
                  (if-let [res (f m)]
                    res
                    (throw (UnsupportedOperationException. (pr-str m))))
                  (if (symbol? m)
                    (if-let [res (f (name m))]
                      res
                      (throw (UnsupportedOperationException. (pr-str m))))
                    (if (nil? m)
                      (f "s/Any")
                      (throw (UnsupportedOperationException.
                              (str (class m) ":" (pr-str m)))))))))))))))

(defn serialise [m] (x-parse cl->str m))
(defn deserialise [m] (x-parse str->cl m))

(defn inferred-schema [m]
  (let [parsed (deserialise m)
        final (inferred-parsed-schema parsed)]
    (serialise final)))
