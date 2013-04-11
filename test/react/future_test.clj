(ns react.future-test
  (:refer-clojure :exclude [future?])
  (:use clojure.test)
  (:require [react.future :as future])
  (:import [java.util.concurrent TimeoutException]))

(defn future?
  ""
  [f]
  (instance? react.future.IFuture f))

(deftest promise-test
  (testing "Promise - complete/success/failure"
    (let [promise (future/promise)]
      (is (true?  (future/complete promise {:type :success :value nil})))
      (is (false? (future/complete promise nil)))
      (is (= @(.state promise) {:status :completed :data {:type :success :value nil}})))
    
    (let [promise (future/promise)]
      (is (true?  (future/success promise 1)))
      (is (false? (future/success promise 2)))
      (is (= @(.state promise) {:status :completed :data {:type :success :value 1}})))
    
    (let [promise (future/promise)
          exp     (Exception. "BLA")]
      (is (true?  (future/failure promise exp)))
      (is (false? (future/failure promise (Exception. "BLABLA"))))
      (is (= @(.state promise) {:status :completed :data {:type :failure :value exp}}))))
  
  (testing "Promise - on-complete/on-success/on-failure"
    (let [promise      (future/promise)
          res-complete (atom nil)
          res-success  (atom nil)
          res-failure  (atom nil)]
      (is (future? (future/on-complete promise #(reset! res-complete %))))
      (is (future? (future/on-success promise #(reset! res-success %))))
      (is (future? (future/on-failure promise #(reset! res-failure %))))
      (is (= (count (-> promise .state deref :waitq)) 3))
      (is (true? (future/success promise 1)))
      (is (= @res-complete {:type :success :value 1}))
      (is (= @res-success 1))
      (is (= @res-failure nil)))
    
    (let [promise      (future/promise)
          res-complete (atom nil)
          res-success  (atom nil)
          res-failure  (atom nil)
          exp          (Exception. "BLA!")]
      (is (future? (future/on-complete promise #(reset! res-complete %))))
      (is (future? (future/on-success promise #(reset! res-success %))))
      (is (future? (future/on-failure promise #(reset! res-failure %))))
      (is (= (count (-> promise .state deref :waitq)) 3))
      (is (true? (future/failure promise exp)))
      (is (= @res-complete {:type :failure :value exp}))
      (is (= @res-success nil))
      (is (= @res-failure exp))))
  
  (testing "Promise - chaining"
    (let [promise (future/promise)
          results (atom [])
          chained (future/on-complete promise #(swap! results conj [promise %]))
          _       (future/on-complete chained #(swap! results conj [chained %]))]
      (future/success promise 1)
      (is (= @results [[promise {:type :success :value 1}]
                       [chained {:type :success :value 1}]]))))
  
  (testing "Promise - deref"
    (let [promise (future/promise)]
      (is (= (deref promise 1 nil) nil))
      (future/success promise 1)
      (is (= (deref promise 1000 nil) 1))
      (is (= (deref promise) 1)))
    (let [promise (future/promise)]
      (is (= (deref promise 1 nil) nil))
      (future/failure promise (Exception. "BLA"))
      (is (thrown? Exception (deref promise 1000 nil)))
      (is (thrown? Exception (deref promise)))))
    
  (testing "Promise - realized?"
    (let [promise (future/promise)]
      (is (false? (realized? promise)))
      (future/success promise 1)
      (is (true? (realized? promise))))))

(deftest constfuture-test
  (testing "ConstFuture - on-complete/on-success/on-failure"
    (let [future       (future/successful 1)
          res-complete (atom nil)
          res-success  (atom nil)
          res-failure  (atom nil)]
      (is (future? (future/on-complete future #(reset! res-complete %))))
      (is (future? (future/on-success future #(reset! res-success %))))
      (is (future? (future/on-failure future #(reset! res-failure %))))
      (is (= @res-complete {:type :success :value 1}))
      (is (= @res-success 1))
      (is (= @res-failure nil)))

    (let [exp          (Exception. "BLA!")
          future       (future/failed exp)
          res-complete (atom nil)
          res-success  (atom nil)
          res-failure  (atom nil)]
      (is (future? (future/on-complete future #(reset! res-complete %))))
      (is (future? (future/on-success future #(reset! res-success %))))
      (is (future? (future/on-failure future #(reset! res-failure %))))
      (is (= @res-complete {:type :failure :value exp}))
      (is (= @res-success nil))
      (is (= @res-failure exp))))

  (testing "ConstFuture - chaining"
    (let [future  (future/successful 1)
          results (atom [])
          chained (future/on-complete future #(swap! results conj [future %]))
          _       (future/on-complete chained #(swap! results conj [chained %]))]
      (is (= future chained))
      (is (= @results [[future  {:type :success :value 1}]
                       [chained {:type :success :value 1}]]))))

  (testing "ConstFuture - deref"
    (let [future (future/successful 1)]
      (is (= (deref future 1000 nil) 1))
      (is (= (deref future) 1)))
    (let [future (future/failed (Exception. "BLA"))]
      (is (thrown? Exception (deref future)))
      (is (thrown? Exception (deref future 1000 nil)))))

  (testing "ConstFuture - realized?"
    (let [promise (future/promise)]
      (is (false? (realized? promise)))
      (future/success promise 1)
      (is (true? (realized? promise))))))

(deftest complete-with-test
  (let [promise (future/promise)
        future  (future/successful 1)]
    (is (false? (realized? promise)))
    (future/complete-with promise future)
    (is (true? (realized? promise)))
    (is (= @promise 1))))

(deftest transform-with-test
  (testing "Successful transformation"
    (let [promise     (future/promise)
          transformed (future/transform-with promise (fn [_] (future/successful 0)))
          result      (atom nil)]
      (future/on-complete transformed #(reset! result %))
      (is (false? (realized? transformed)))
      (future/success promise 1)
      (is (true? (realized? transformed)))
      (is (= @result {:type :success :value 0}))))
  
  (testing "Failed transformation"
    (let [exp         (Exception. "BLA!")
          promise     (future/promise)
          transformed (future/transform-with promise (fn [_] (future/failed exp)))
          result      (atom nil)]
      (future/on-complete transformed #(reset! result %))
      (is (false? (realized? transformed)))
      (future/success promise 1)
      (is (true? (realized? transformed)))
      (is (= @result {:type :failure :value exp}))))
  
  (testing "Successful transformation with a function which doesn't return a future"
    (let [promise     (future/promise)
          transformed (future/transform-with promise (fn [_] 0))
          result      (atom nil)]
      (future/on-complete transformed #(reset! result %))
      (is (false? (realized? transformed)))
      (future/success promise 1)
      (is (true? (realized? transformed)))
      (is (= @result {:type :success :value 0}))))
  
  (testing "Transformation that raises an exception")
  (let [exp         (Exception. "BLA!")
        promise     (future/promise)
        transformed (future/transform-with promise (fn [_] (throw exp)))
        result      (atom nil)]
    (future/on-complete transformed #(reset! result %))
    (is (false? (realized? transformed)))
    (future/success promise 1)
    (is (true? (realized? transformed)))
    (is (= @result {:type :failure :value exp}))))

(deftest map-test
  (testing "Map on successful future"
    (is (= @(future/map (future/successful 1)
                        (fn [v] (future/successful (inc v))))
           2)))
  
  (testing "Map on successful future"
    (is (thrown? Exception
                 @(future/map (future/successful 1)
                              (fn [v] (future/failed (Exception. "BLA")))))))
  
  (testing "Map on failed future"
    (is (thrown? Exception
                 @(future/map (future/failed (Exception. "BLA"))
                              (fn [v] (future/successful (inc v)))))))
  
  (testing "Map a function which throws an exception"
    (is (thrown? Exception
                 @(future/map (future/successful 1)
                              (fn [_] (throw (Exception. "BLA"))))))))

(deftest recover-test
  (testing "Recover on failed future"
    (is (= @(future/recover (future/failed (Exception. "BLA"))
                            (fn [e] (future/successful 1)))
           1)))
  
  (testing "Recover on failed future"
    (is (thrown? Exception
                 @(future/recover (future/failed (Exception. "BLA"))
                                  (fn [e] (future/failed (Exception. e)))))))
  
  (testing "Recover on successful future"
    (is (= @(future/recover (future/successful 1)
                            (fn [e] (future/failed (Exception. e))))
           1)))
  
  (testing "Recover a function which throws an exception"
    (is (thrown? Exception
                 @(future/recover (future/failed (Exception. "BLA"))
                                  (fn [_] (throw (Exception. "BLA"))))))))

(deftest within-test
  (testing "Within"
    (is (= @(future/within (future/successful 1) 100)
           1)))
    (is (thrown? TimeoutException
                 @(future/within (future/promise) 100))))

(deftest zip-test
  (testing "Zip two successful futures"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/zip p1 p2)]
      (future/success p1 1)
      (future/success p2 2)
      (is (= @p3 [1 2]))))
  
  (testing "Zip a successful future and a failed one"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/zip p1 p2)]
      (future/success p1 1)
      (future/failure p2 (Exception. "P2"))
      (is (thrown? Exception @p3))))
  
  (testing "Zip a failed future and a successfull one"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/zip p1 p2)]
      (future/failure p1 (Exception. "P1"))
      (future/success p2 2)
      (is (thrown? Exception @p3)))))

(deftest collect-test
  (testing "Collect successful futures"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/collect [p1 p2])]
      (future/success p1 1)
      (future/success p2 2)
      (is (= @p3 [1 2]))))
  
  (testing "Collect futures containing a failed one"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/collect [p1 p2])]
      (future/success p1 1)
      (future/failure p2 (Exception. "P2"))
      (is (thrown? Exception @p3)))))

(deftest join-test
  (testing "Join futures"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/join [p1 p2])]
      (future/failure p1 (Exception. "P1"))
      (is (false? (realized? p3)))
      (future/success p2 2)
      (is (true? (realized? p3))))))

(deftest select-test
  (testing "Select (successful)"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/promise)
          p4 (future/select [p1 p2 p3])]
      (is (false? (realized? p4)))
      (future/success p2 1)
      (is (= @p4 1))))
  
  (testing "Select (failed)"
    (let [p1 (future/promise)
          p2 (future/promise)
          p3 (future/promise)
          p4 (future/select [p1 p2 p3])]
      (is (false? (realized? p4)))
      (future/failure p2 (Exception. "P2"))
      (is (thrown? Exception @p4)))))
