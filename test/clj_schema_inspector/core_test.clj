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
      (polish-types #{s/Num s/Str}) => (just #{s/Num s/Str})
      (polish-types #{s/Keyword s/Num s/Str})
      => (just #{s/Keyword s/Num s/Str}))

(fact "inferred-type function"
      (inferred-type s/Num s/Num) => s/Num
      (inferred-type s/Num s/Num) => s/Num
      (inferred-type s/Num s/Str) => (just #{s/Str s/Num})
      (inferred-type #{s/Num s/Str} s/Str) => (just #{s/Num s/Str})
      (inferred-type #{s/Num s/Str} #{s/Keyword s/Symbol})
      => (just #{s/Keyword s/Num s/Symbol s/Str})
      (inferred-type #{s/Num s/Str} #{s/Str s/Symbol})
      => (just #{s/Num s/Symbol s/Str}))

(fact "median function"
      (median [1 1 1 1 1 1 1]) => 1
      (median [1 1 1 1 1 1 2]) => 1
      (median [1 1 2]) => 1)

(fact "m-class function"
      (m-class nil) => s/Any
      (m-class (java.util.Date.)) => s/Inst
      (m-class true) => s/Bool
      (m-class (java.util.UUID/randomUUID)) => s/Uuid
      (m-class #"hello") => s/Regex
      (m-class "hello") => s/Str
      (m-class 1) => s/Num
      (m-class 37467236472346) => s/Num
      (m-class 1.0) => s/Num
      (m-class 1.000000000001) => s/Num
      (m-class :hello) => s/Str)

(fact "flat-structure function"
      (flat-structure {:a {:b {:c 1}}})
      => {[:a :b :c] {:mode nil :type s/Num :values {1 1} :count 1}}
      (flat-structure {:a {:b {:c 1.0}}})
      => {[:a :b :c] {:mode nil :type s/Num :values {1.0 1} :count 1}}
      (flat-structure {:a {:b {:c 1 :d 2}}})
      => {[:a :b :c] {:mode nil :type s/Num :values {1 1} :count 1}
          [:a :b :d] {:mode nil :type s/Num :values {2 1} :count 1}}
      (flat-structure {:a {:b {:c 1 :d 2 :e [1 2 3]}}})
      => {[:a :b :c] {:mode nil :type s/Num :values {1 1} :count 1}
          [:a :b :d] {:mode nil :type s/Num :values {2 1} :count 1}
          [:a :b :e "[]"] {:mode nil :type s/Num :values {1 1 2 1 3 1} :count 1}}
      (flat-structure {:a {:b {:c 1 :d 2 :e [{:a 1} {:b 2} {:c 3}]}}})
      => {[:a :b :c] {:mode nil :type s/Num :values {1 1} :count 1}
          [:a :b :d] {:mode nil :type s/Num :values {2 1} :count 1}
          [:a :b :e "[]" :a] {:mode nil :type s/Num :values {1 1} :count 1}
          [:a :b :e "[]" :b] {:mode nil :type s/Num :values {2 1} :count 1}
          [:a :b :e "[]" :c] {:mode nil :type s/Num :values {3 1} :count 1}}
      (flat-structure {:a {:b [{:c 1} {:d 2}]}})
      => {[:a :b "[]" :c] {:mode nil :type s/Num :values {1 1} :count 1}
          [:a :b "[]" :d] {:mode nil :type s/Num :values {2 1} :count 1}})

(fact "flat-structure function (given prefix)"
      (flat-structure [:0] {:a {:b {:c 1 :d 2}}})
      => {[:0 :a :b :c] {:mode nil :type s/Num :values {1 1} :count 1}
          [:0 :a :b :d] {:mode nil :type s/Num :values {2 1} :count 1}})

(fact "merge-maps function"
      (merge-maps {:mode nil :type s/Any :values {1 1} :count 1}
                  {:mode nil :type s/Any :values {1 1} :count 1})
      => {:mode nil :type s/Any :values {1 2} :count 2}
      (merge-maps {:mode nil :type s/Num :values {1 1} :count 1}
                  {:mode nil :type s/Num :values {2 1} :count 1})
      => {:mode nil :type s/Num :values {1 1 2 1} :count 2}
      (merge-maps {:mode nil :type s/Num :values {1 1} :count 1}
                  {:mode nil :type s/Num :values {2.0 1} :count 1})
      => {:mode nil :type s/Num :values {1 1 2.0 1} :count 2}
      (merge-maps {:mode nil :values {1 1} :type s/Num :count 1}
                  {:mode nil :values {1 1} :type s/Num :count 1})
      => {:mode nil :values {1 2} :type s/Num :count 2}
      (reduce merge-maps {}
              (take big-number (repeat {:mode nil :type s/Num :values {1 1} :count 1})))
      => {:mode nil :values {1 big-number} :type s/Num :count big-number}
      (reduce merge-maps {}
              (take (inc big-number) (repeat {:mode nil :type s/Num :values {1 1} :count 1})))
      => {:mode nil :values {1 (inc big-number)} :type s/Num :count (inc big-number)}
      (reduce merge-maps {}
              [{:mode nil :type s/Num :values {1 1} :count 1}
               {:mode nil :type s/Num :values {1.0 1} :count 1}
               {:mode nil :type s/Str :values {"1" 1} :count 1}]) 
      => {:mode nil :values {1 1 1.0 1 "1" 1} :type #{s/Num s/Str} :count 3})

(fact "polish-record function"
      (polish-record (inc big-number)
                     (zipmap (range (inc big-number))
                             (take (inc big-number)
                                   (repeat {:mode nil :values {1 1}
                                            :type s/Num :count 1})))
                     0) =>
      (zipmap (range (inc big-number))
              (concat [{:mode :rare :values :not-enum
                        :type s/Num :count 1}]
                      (take 30 (repeat {:mode nil :values {1 1}
                                        :type s/Num :count 1}))))
      (polish-record 100
                     {0 {:mode nil :values {1 100}
                         :type s/Num :count 100}}
                     0) =>
      {0 {:mode :required :values {1 100} :type s/Num :count 100}}
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
                         :type s/Num :count 100}}
                     0) =>
      {0 {:mode :required :values :not-enum
          :type s/Num :count 100}}
      (polish-record 100
                     {0 {:mode nil :values (zipmap (range 10)
                                                   (take 10 (repeat 10)))
                         :type s/Num :count 100}}
                     0) =>
      {0 {:mode :required :values :not-enum
          :type s/Num :count 100}}
      (polish-record 100
                     {0 {:mode nil :values (zipmap (range 5)
                                                   (take 5 (repeat 20)))
                         :type s/Num :count 100}}
                     0) =>
      {0 {:mode :required :values (zipmap (range 5)
                                          (take 5 (repeat 20)))
          :type s/Num :count 100}})

(fact "polish-values function"
      (polish-values big-number
                     (zipmap (range big-number)
                             (take big-number (repeat {:mode nil :values {1 1}
                                                       :type s/Num :count 1}))))
      =>
      (zipmap (range big-number)
              (take big-number (repeat {:mode :rare :values {1 1}
                                        :type s/Num :count 1})))
      (polish-values (inc big-number)
                     (zipmap (range (inc big-number))
                             (take (inc big-number) (repeat {:mode nil :values {1 1}
                                                             :type s/Num :count 1}))))
      =>
      (zipmap (range (inc big-number))
              (take (inc big-number) (repeat {:mode :rare :values :not-enum
                                              :type s/Num :count 1}))))

(fact "add-map function"
      (add-map {} {}) => {:count 1 :m {}}
      (add-map {} {:a {:b [{:c 1} {:d 2}]}})
      =>
      {:count 1
       :m {[:a :b "[]" :c] {:type s/Num :count 1 :mode :required :values {1 1}}
           [:a :b "[]" :d] {:type s/Num :count 1 :mode :required :values {2 1}}}}
      (add-map
       {:count 1
        :m {[:a :b "[]" :c] {:type s/Num :count 1 :mode :required :values {1 1}}
            [:a :b "[]" :d] {:type s/Num :count 1 :mode :required :values {2 1}}}}
       {:a {:b [{:c 1} {:d 2}]}})
      =>
      {:count 2
       :m {[:a :b "[]" :c] {:type s/Num :count 2 :mode :required :values {1 2}}
           [:a :b "[]" :d] {:type s/Num :count 2 :mode :required :values {2 2}}}}
      (reduce add-map {} (take big-number (repeat {:a {:b [{:c 1} {:d 2}]}})))
      =>
      {:count big-number
       :m {[:a :b "[]" :c] {:type s/Num :count big-number :mode :required :values {1 big-number}}
           [:a :b "[]" :d] {:type s/Num :count big-number :mode :required :values {2 big-number}}}}
      (reduce add-map {} (take (inc big-number) (repeat {:a {:b [{:c 1} {:d 2}]}})))
      =>
      {:count (inc big-number)
       :m {[:a :b "[]" :c] {:type s/Num :count (inc big-number) :mode :required :values {1 (inc big-number)}}
           [:a :b "[]" :d] {:type s/Num :count (inc big-number) :mode :required :values {2 (inc big-number)}}}}
      (reduce add-map {} (take 50 (repeat {:a {:b [{:c 1} {:d 2}]}})))
      =>
      {:count 50
       :m {[:a :b "[]" :c] {:type s/Num :count 50 :mode :required :values {1 50}}
           [:a :b "[]" :d] {:type s/Num :count 50 :mode :required :values {2 50}}}}
      (reduce add-map {} (take 50 (interleave (repeat {:a {:b [{:c 1}]}})
                                              (repeat {:a {:b [{:d 2}]}}))))
      =>
      {:count 50
       :m {[:a :b "[]" :c] {:type s/Num :count 25 :mode :optional :values {1 25}}
           [:a :b "[]" :d] {:type s/Num :count 25 :mode :optional :values {2 25}}}}
      (reduce add-map {} (take 5000 (interleave (repeat {:a {:b [{:c 1}]}})
                                                (repeat {:a {:b [{:d 2}]}}))))
      =>
      {:count 5000
       :m {[:a :b "[]" :c] {:type s/Num :count 2500 :mode :optional :values {1 2500}}
           [:a :b "[]" :d] {:type s/Num :count 2500 :mode :optional :values {2 2500}}}}
      (reduce add-map {} (take 10000 (interleave (repeat {:a {:b [{:c 1}]}})
                                                 (repeat {:a {:b [{:d 2}]}})
                                                 (repeat {:a {:b [{:e 3}]}})
                                                 (repeat {:a {:b [{:f 4}]}}))))
      =>
      {:count 10000
       :m {[:a :b "[]" :c] {:type s/Num :count 2500 :mode :rare :values {1 2500}}
           [:a :b "[]" :d] {:type s/Num :count 2500 :mode :rare :values {2 2500}}
           [:a :b "[]" :e] {:type s/Num :count 2500 :mode :rare :values {3 2500}}
           [:a :b "[]" :f] {:type s/Num :count 2500 :mode :rare :values {4 2500}}}}
      (let [d (java.util.Date.)]
        (reduce add-map {} (take 10000 (interleave (repeat {:a {:b [{:c 1}]}})
                                                   (repeat {:a {:b [{:c "1"}]}})
                                                   (repeat {:a {:b [{:c :1}]}})
                                                   (repeat {:a {:b [{:c d}]}}))))
        =>
        {:count 10000
         :m {[:a :b "[]" :c]
             {:type #{s/Num s/Str s/Inst}
              :count 10000 :mode :required
              :values {1 2500 "1" 2500 d 2500 :1 2500}}}}))

(fact "inferred-schema-keys function"
      (let [d (java.util.Date.)]
        (inferred-schema-keys
          (take 10000 (interleave (repeat {:a {:b [{:c 1}]}})
                                  (repeat {:a {:b [{:c "1"}]}})
                                  (repeat {:a {:b [{:c :1}]}})
                                  (repeat {:a {:b [{:c d}]}}))))
        =>
        {:count 10000
         :m {[:a :b "[]" :c]
             {:type #{s/Num s/Str s/Inst}
              :count 10000 :mode :required
              :values {1 2500 "1" 2500 d 2500 :1 2500}}}}))

(fact "inferred-parsed-schema function"
      (inferred-parsed-schema
       {:count 1
        :m {[:a :b "[]" :c]
            {:type s/Num :count 1 :mode :required :values {1 1}}}})
      =>
      (s/explain {:a {:b [{:c s/Num}]}})
      (inferred-parsed-schema
       {:count 1
        :m {[:a :b "[]" :c]
            {:type s/Num :count 1 :mode :required :values {1 1}}
            [:a :b "[]" :d]
            {:type s/Num :count 1 :mode :required :values {1 1}}}})
      =>
      (s/explain {:a {:b [{:c s/Num :d s/Num}]}}))

(fact "main test"
      (inferred-parsed-schema
       (inferred-schema-keys
        [{:a {:b {:c 1 :d 2 :e [{:a 1} {:b 2} {:c 3}]}}}]))
      =>
      (s/explain {:a {:b {:c s/Num :d s/Num
                          :e [{:a s/Num :b s/Num :c s/Num}]}}})
      (inferred-parsed-schema
       (inferred-schema-keys
        (concat
         [{:a {:b {:c 1 :d 2
                   :e [{:a 1} {:b 2} {:c 3} {:d 4} {:f 1}]}}}]
         (take 20
               (repeat {:a {:b {:c 1 :d 2
                                :e [{:a 1} {:b 2} {:c 3} {:f "1"}]}}})))))
      =>
      (s/explain {:a {:b {:c s/Num :d s/Num
                          :e [{:a s/Num :b s/Num
                               :c s/Num :d (s/maybe s/Num)
                               :f (s/either s/Num s/Str)}]}}})
      (serialise
       (inferred-parsed-schema
        (inferred-schema-keys
         (concat
          [{:a {:b {:c 1 :d 2
                    :e [{:a 1} {:b 2} {:c 3} {:d 4} {:f 1}]}}}]
          (take 20
                (repeat {:a {:b {:c 1 :d 2
                                 :e [{:a 1} {:b 2} {:c 3} {:f "1"}]}}}))))))
      =>
      (serialise
       (s/explain {:a {:b {:c s/Num :d s/Num
                           :e [{:a s/Num :b s/Num
                                :c s/Num :d (s/maybe s/Num)
                                :f (s/either s/Num s/Str)}]}}}))
      (reduce (fn [p n]
                (serialise (add-map (deserialise p) n)))
              {}
              (take 5000 (interleave (repeat {:a {:b [{:c 1}]}})
                                     (repeat {:a {:b [{:d 2}]}}))))
      =>
      (serialise
       {:count 5000
        :m {[:a :b "[]" :c] {:type s/Num :count 2500 :mode :optional :values {1 2500}}
            [:a :b "[]" :d] {:type s/Num :count 2500 :mode :optional :values {2 2500}}}}))

(def s1
  {:count 86, :m {[:payload :email_s] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :experienced_skills "[]"] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:parameters :type] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 3}, [:payload :type] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 2}, [:headers :referer] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 7}, [:headers :accept-encoding] {:type #{"s/Str"}, :values {"gzip, deflate" 82, "gzip, deflate, sdch" 2}, :mode :optional, :count 84}, [:session_id] {:type #{"s/Str"}, :values :not-enum, :mode :required, :count 86}, [:headers :x-forwarded-for] {:type #{"s/Str"}, :values :not-enum, :mode :optional, :count 78}, [:parentId] {:type #{"s/Str"}, :values {"" 83}, :mode :optional, :count 83}, [:method] {:type #{"s/Str"}, :values {"GET" 84, "PUT" 2}, :mode :required, :count 86}, [:entityType] {:type #{"s/Str"}, :values :not-enum, :mode :optional, :count 80}, [:headers :if-none-match] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 5}, [:payload :colleagues "[]" :department] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:payload :department_s] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :name_s] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :availability] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:headers :accept] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 8}, [:payload :advices "[]"] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :id] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 2}, [:name] {:type #{"s/Str"}, :values :not-enum, :mode :required, :count 86}, [:payload :type_s] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :colleagues "[]" :name] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:is_query] {:type #{"s/Bool"}, :values {true 84, false 2}, :mode :required, :count 86}, [:payload :name] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :skills "[]"] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:payload :department] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :status] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :availability_comment] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:headers :x-requested-with] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 5}, [:payload :availability_b] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:user] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 2}, [:headers :connection] {:type #{"s/Str"}, :values {"keep-alive" 8, "close" 76}, :mode :optional, :count 84}, [:parameters :q] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 3}, [:payload :colleagues "[]" :email] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:headers :accept-language] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 8}, [:payload :skills_ss "[]"] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:payload :created_date_dt] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:action] {:type #{"s/Str"}, :values {"find" 84, "update" 2}, :mode :required, :count 86}, [:entityId] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 2}, [:payload :created_date] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 2}, [:headers :host] {:type #{"s/Str"}, :values {"localhost:3001" 6, "cambio.cistechfutures.net" 78}, :mode :optional, :count 84}, [:parameters :entityType] {:type #{"s/Str"}, :values {"html" 78}, :mode :optional, :count 78}, [:id] {:type #{"s/Str"}, :values :not-enum, :mode :required, :count 86}, [:url] {:type #{"s/Str"}, :values :not-enum, :mode :required, :count 86}, [:server_timestamp] {:type #{"s/Str"}, :values :not-enum, :mode :required, :count 86}, [:headers :x-ssl-cipher] {:type #{"s/Str"}, :values :not-enum, :mode :optional, :count 78}, [:payload :description] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:parameters :id] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 3}, [:payload :colleagues "[]" :job_title] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:payload :email] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :codename] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:headers :user-agent] {:type #{"s/Str"}, :values :not-enum, :mode :optional, :count 84}, [:payload :job_title] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:commandName] {:type #{"s/Str"}, :values :not-enum, :mode :required, :count 86}, [:payload :owner] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :job_title_s] {:values :not-enum, :type "s/Str", :mode :rare, :count 1}, [:payload :colleagues "[]" :uid] {:count 1, :type "s/Str", :mode :rare, :values :not-enum}, [:headers :cookie] {:type #{"s/Str"}, :values :not-enum, :mode :rare, :count 8}}})

(def s2
  {:commandName "create-profile", :is_query false, :payload {:username "username", :password "********", :id "username", :job_title "job-title", :email "test-email@test.co.uk", :name "username", :department "department", :colleagues [nil]}, :method "POST", :name "login", :session_id "rxaQzFqrvPxunWe28s9RlCIX4nuqs15E", :headers {:x-ssl-cipher "DHE-RSA-AES256-GCM-SHA384 TLSv1.2 Kx=DH       Au=RSA  Enc=AESGCM(256) Mac=AEAD", :referer "https://cambio.cistechfutures.net/", :user-agent "Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko", :x-requested-with "XMLHttpRequest", :cache-control "no-cache", :host "cambio.cistechfutures.net", :content-length "35", :cookie "connect.sid=s%3ArxaQzFqrvPxunWe28s9RlCIX4nuqs15E.rbKp9OX%2BJRF0b4WWcIvsGJmu0FGe0hOj6JBLzLNEewQ", :accept-encoding "gzip, deflate", :content-type "application/x-www-form-urlencoded; charset=UTF-8", :connection "Keep-Alive", :accept-language "en-GB", :x-forwarded-for "ip.address", :accept "*/*"}, :id "034e40c7-72f5-418c-b8bf-aeba1223bae0", :url "/api/login", :action "create", :server_timestamp "Mon Apr 27 2015 09:43:48 GMT+0000 (UTC)", :parentId "", :entityId "username"})

(fact (serialise (add-map s1 s2)) =not=> (throws Exception))
