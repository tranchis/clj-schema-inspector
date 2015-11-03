(ns clj-schema-inspector.core-test
  (:require [midje.sweet :refer :all]
            [schema.core :as s]
            [clj-schema-inspector.core :refer :all]))

(def optional-t (inc (int (* 100 rare-threshold))))
(def rare-t (int (* 100 rare-threshold)))

(fact "to-set function"
      (to-set 1) => (just #{1})
      (to-set [1 2 3] => (just #{1 2 3}))
      (to-set #{1 2 3}) => (just #{1 2 3}))

(fact "polish-types function"
      (polish-types #{s/Int s/Num}) => s/Num
      (polish-types #{s/Int s/Num s/Str}) => (just #{s/Num s/Str})
      (polish-types #{s/Keyword s/Int s/Str})
      => (just #{s/Keyword s/Int s/Str}))

(fact "inferred-type function"
      (inferred-type s/Int s/Int) => s/Int
      (inferred-type s/Int s/Num) => s/Num
      (inferred-type s/Int s/Str) => (just #{s/Str s/Int})
      (inferred-type #{s/Int s/Str} s/Str) => (just #{s/Int s/Str})
      (inferred-type #{s/Int s/Str} #{s/Keyword s/Symbol})
      => (just #{s/Keyword s/Int s/Symbol s/Str})
      (inferred-type #{s/Int s/Str} #{s/Str s/Symbol})
      => (just #{s/Int s/Symbol s/Str}))

(fact "median function"
      (median [1 1 1 1 1 1 1]) => 1
      (median [1 1 1 1 1 1 2]) => 1
      (median [1 1 2]) => 1)

(fact "m-class function"
      (m-class "hello") => s/Str
      (m-class 1) => s/Int
      (m-class 37467236472346) => s/Int
      (m-class 1.0) => s/Num
      (m-class 1.000000000001) => s/Num
      (m-class :hello) => s/Keyword
      (m-class (symbol "hello")) => s/Symbol)

(fact "flat-structure function"
      (flat-structure {:a {:b {:c 1}}})
      => {[:a :b :c] {:mode nil :type s/Int :values {1 1} :count 1}}
      (flat-structure {:a {:b {:c 1.0}}})
      => {[:a :b :c] {:mode nil :type s/Num :values {1.0 1} :count 1}}
      (flat-structure {:a {:b {:c 1 :d 2}}})
      => {[:a :b :c] {:mode nil :type s/Int :values {1 1} :count 1}
          [:a :b :d] {:mode nil :type s/Int :values {2 1} :count 1}}
      (flat-structure {:a {:b {:c 1 :d 2 :e [1 2 3]}}})
      => {[:a :b :c] {:mode nil :type s/Int :values {1 1} :count 1}
          [:a :b :d] {:mode nil :type s/Int :values {2 1} :count 1}
          [:a :b :e "[]"] {:mode nil :type s/Int :values {1 1 2 1 3 1} :count 1}}
      (flat-structure {:a {:b {:c 1 :d 2 :e [{:a 1} {:b 2} {:c 3}]}}})
      => {[:a :b :c] {:mode nil :type s/Int :values {1 1} :count 1}
          [:a :b :d] {:mode nil :type s/Int :values {2 1} :count 1}
          [:a :b :e "[]" :a] {:mode nil :type s/Int :values {1 1} :count 1}
          [:a :b :e "[]" :b] {:mode nil :type s/Int :values {2 1} :count 1}
          [:a :b :e "[]" :c] {:mode nil :type s/Int :values {3 1} :count 1}}
      (flat-structure {:a {:b [{:c 1} {:d 2}]}})
      => {[:a :b "[]" :c] {:mode nil :type s/Int :values {1 1} :count 1}
          [:a :b "[]" :d] {:mode nil :type s/Int :values {2 1} :count 1}})

(fact "flat-structure function (given prefix)"
      (flat-structure [:0] {:a {:b {:c 1 :d 2}}})
      => {[:0 :a :b :c] {:mode nil :type s/Int :values {1 1} :count 1}
          [:0 :a :b :d] {:mode nil :type s/Int :values {2 1} :count 1}})

(fact "merge-maps function"
      (merge-maps {:mode nil :type s/Any :values {1 1} :count 1}
                  {:mode nil :type s/Any :values {1 1} :count 1})
      => {:mode nil :type s/Any :values {1 2} :count 2}
      (merge-maps {:mode nil :type s/Int :values {1 1} :count 1}
                  {:mode nil :type s/Int :values {2 1} :count 1})
      => {:mode nil :type s/Int :values {1 1 2 1} :count 2}
      (merge-maps {:mode nil :type s/Int :values {1 1} :count 1}
                  {:mode nil :type s/Num :values {2.0 1} :count 1})
      => {:mode nil :type s/Num :values {1 1 2.0 1} :count 2}
      (merge-maps {:mode nil :values {1 1} :type s/Int :count 1}
                  {:mode nil :values {1 1} :type s/Int :count 1})
      => {:mode nil :values {1 2} :type s/Int :count 2}
      (reduce merge-maps {}
              (take big-number (repeat {:mode nil :type s/Int :values {1 1} :count 1})))
      => {:mode nil :values {1 big-number} :type s/Int :count big-number}
      (reduce merge-maps {}
              (take (inc big-number) (repeat {:mode nil :type s/Int :values {1 1} :count 1})))
      => {:mode nil :values {1 (inc big-number)} :type s/Int :count (inc big-number)}
      (reduce merge-maps {}
              [{:mode nil :type s/Int :values {1 1} :count 1}
               {:mode nil :type s/Num :values {1.0 1} :count 1}
               {:mode nil :type s/Str :values {"1" 1} :count 1}]) 
      => {:mode nil :values {1 1 1.0 1 "1" 1} :type #{s/Num s/Str} :count 3})

(fact "polish-record function"
      (polish-record (inc big-number)
                     (zipmap (range (inc big-number))
                             (take (inc big-number)
                                   (repeat {:mode nil :values {1 1}
                                            :type s/Int :count 1})))
                     0) =>
      (zipmap (range (inc big-number))
              (concat [{:mode :rare :values :not-enum
                        :type s/Int :count 1}]
                      (take 30 (repeat {:mode nil :values {1 1}
                                        :type s/Int :count 1}))))
      (polish-record 100
                     {0 {:mode nil :values {1 100}
                         :type s/Int :count 100}}
                     0) =>
      {0 {:mode :required :values {1 100} :type s/Int :count 100}}
      (polish-record 100
                     {0 {:mode nil :values {"2" optional-t}
                         :type s/Str :count optional-t}}
                     0) =>
      {0 {:mode :optional :values {"2" optional-t}
          :type s/Str :count optional-t}}
      (polish-record 100
                     {0 {:mode nil :values {"2" rare-t}
                         :type s/Str :count rare-t}}
                     0) =>
      {0 {:mode :optional :values {"2" rare-t}
          :type s/Str :count rare-t}}
      (polish-record 100
                     {0 {:mode nil :values {"2" big-number
                                            "1" (- 100 big-number)}
                         :type s/Str :count 100}}
                     0) =>
      {0 {:mode :required :values {"2" big-number "1" (- 100 big-number)}
          :type s/Str :count 100}}
      (polish-record 100
                     {0 {:mode nil :values (zipmap (range 100)
                                                   (take 100 (repeat 1)))
                         :type s/Int :count 100}}
                     0) =>
      {0 {:mode :required :values :not-enum
          :type s/Int :count 100}}
      (polish-record 100
                     {0 {:mode nil :values (zipmap (range 10)
                                                   (take 10 (repeat 10)))
                         :type s/Int :count 100}}
                     0) =>
      {0 {:mode :required :values :not-enum
          :type s/Int :count 100}}
      (polish-record 100
                     {0 {:mode nil :values (zipmap (range 5)
                                                   (take 5 (repeat 20)))
                         :type s/Int :count 100}}
                     0) =>
      {0 {:mode :required :values (zipmap (range 5)
                                          (take 5 (repeat 20)))
          :type s/Int :count 100}})

(fact "polish-values function"
      (polish-values big-number
                     (zipmap (range big-number)
                             (take big-number (repeat {:mode nil :values {1 1}
                                                       :type s/Int :count 1}))))
      =>
      (zipmap (range big-number)
              (take big-number (repeat {:mode :rare :values {1 1}
                                        :type s/Int :count 1})))
      (polish-values (inc big-number)
                     (zipmap (range (inc big-number))
                             (take (inc big-number) (repeat {:mode nil :values {1 1}
                                                             :type s/Int :count 1}))))
      =>
      (zipmap (range (inc big-number))
              (take (inc big-number) (repeat {:mode :rare :values :not-enum
                                              :type s/Int :count 1}))))

(fact "add-map function"
      (add-map {} {}) => {:count 1 :m {}}
      (add-map {} {:a {:b [{:c 1} {:d 2}]}})
      =>
      {:count 1
       :m {[:a :b "[]" :c] {:type s/Int :count 1 :mode :required :values {1 1}}
           [:a :b "[]" :d] {:type s/Int :count 1 :mode :required :values {2 1}}}}
      (add-map
       {:count 1
        :m {[:a :b "[]" :c] {:type s/Int :count 1 :mode :required :values {1 1}}
            [:a :b "[]" :d] {:type s/Int :count 1 :mode :required :values {2 1}}}}
       {:a {:b [{:c 1} {:d 2}]}})
      =>
      {:count 2
       :m {[:a :b "[]" :c] {:type s/Int :count 2 :mode :required :values {1 2}}
           [:a :b "[]" :d] {:type s/Int :count 2 :mode :required :values {2 2}}}}
      (reduce add-map {} (take big-number (repeat {:a {:b [{:c 1} {:d 2}]}})))
      =>
      {:count big-number
       :m {[:a :b "[]" :c] {:type s/Int :count big-number :mode :required :values {1 big-number}}
           [:a :b "[]" :d] {:type s/Int :count big-number :mode :required :values {2 big-number}}}}
      (reduce add-map {} (take (inc big-number) (repeat {:a {:b [{:c 1} {:d 2}]}})))
      =>
      {:count (inc big-number)
       :m {[:a :b "[]" :c] {:type s/Int :count (inc big-number) :mode :required :values {1 (inc big-number)}}
           [:a :b "[]" :d] {:type s/Int :count (inc big-number) :mode :required :values {2 (inc big-number)}}}}
      (reduce add-map {} (take 50 (repeat {:a {:b [{:c 1} {:d 2}]}})))
      =>
      {:count 50
       :m {[:a :b "[]" :c] {:type s/Int :count 50 :mode :required :values {1 50}}
           [:a :b "[]" :d] {:type s/Int :count 50 :mode :required :values {2 50}}}}
      (reduce add-map {} (take 50 (interleave (repeat {:a {:b [{:c 1}]}})
                                              (repeat {:a {:b [{:d 2}]}}))))
      =>
      {:count 50
       :m {[:a :b "[]" :c] {:type s/Int :count 25 :mode :optional :values {1 25}}
           [:a :b "[]" :d] {:type s/Int :count 25 :mode :optional :values {2 25}}}}
      (reduce add-map {} (take 5000 (interleave (repeat {:a {:b [{:c 1}]}})
                                                (repeat {:a {:b [{:d 2}]}}))))
      =>
      {:count 5000
       :m {[:a :b "[]" :c] {:type s/Int :count 2500 :mode :optional :values {1 2500}}
           [:a :b "[]" :d] {:type s/Int :count 2500 :mode :optional :values {2 2500}}}}
      (reduce add-map {} (take 10000 (interleave (repeat {:a {:b [{:c 1}]}})
                                                 (repeat {:a {:b [{:d 2}]}})
                                                 (repeat {:a {:b [{:e 3}]}})
                                                 (repeat {:a {:b [{:f 4}]}}))))
      =>
      {:count 10000
       :m {[:a :b "[]" :c] {:type s/Int :count 2500 :mode :rare :values {1 2500}}
           [:a :b "[]" :d] {:type s/Int :count 2500 :mode :rare :values {2 2500}}
           [:a :b "[]" :e] {:type s/Int :count 2500 :mode :rare :values {3 2500}}
           [:a :b "[]" :f] {:type s/Int :count 2500 :mode :rare :values {4 2500}}}}
      (reduce add-map {} (take 10000 (interleave (repeat {:a {:b [{:c 1}]}})
                                                 (repeat {:a {:b [{:c "1"}]}})
                                                 (repeat {:a {:b [{:c :1}]}})
                                                 (repeat {:a {:b [{:c (symbol "1")}]}}))))
      =>
      {:count 10000
       :m {[:a :b "[]" :c]
           {:type #{s/Int s/Keyword s/Str s/Symbol}
            :count 10000 :mode :required
            :values {1 2500 "1" 2500 (symbol "1") 2500 :1 2500}}}})

(fact "inferred-schema-keys function"
      (inferred-schema-keys
       (take 10000 (interleave (repeat {:a {:b [{:c 1}]}})
                               (repeat {:a {:b [{:c "1"}]}})
                               (repeat {:a {:b [{:c :1}]}})
                               (repeat {:a {:b [{:c (symbol "1")}]}}))))
      =>
      {:count 10000
       :m {[:a :b "[]" :c]
           {:type #{s/Int s/Keyword s/Str s/Symbol}
            :count 10000 :mode :required
            :values {1 2500 "1" 2500 (symbol "1") 2500 :1 2500}}}})

(fact "inferred-schema function"
      (inferred-schema
       {:count 1
        :m {[:a :b "[]" :c]
            {:type s/Int :count 1 :mode :required :values {1 1}}}})
      =>
      {:a {:b [{:c s/Int}]}}
      (inferred-schema
       {:count 1
        :m {[:a :b "[]" :c]
            {:type s/Int :count 1 :mode :required :values {1 1}}
            [:a :b "[]" :d]
            {:type s/Int :count 1 :mode :required :values {1 1}}}})
      =>
      {:a {:b [{:c s/Int :d s/Int}]}})

