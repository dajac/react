(ns react.pool-test
  (:use clojure.test)
  (:require [react.pool :as pool]))

(def executor (java.util.concurrent.Executors/newFixedThreadPool 1))

(deftest future-call-test
  (testing "Successful task"
    (is (= @(pool/future-call executor (fn [] (+ 1 2))) 3)))
  
  (testing "Failed task"
    (is (thrown? Exception
                 @(pool/future-call executor (fn [] (throw (Exception. "BLA")))))))
  
  (testing "Cancelled task"
    (let [f (pool/future-call executor (fn [] (Thread/sleep 10000)))]
      (.cancel f true)
      (is (thrown? java.util.concurrent.CancellationException @f)))))

(deftest future-test
  (testing "Successful task"
    (is (= @(pool/future executor (+ 1 2)) 3)))

  (testing "Failed task"
    (is (thrown? Exception
                 @(pool/future executor (throw (Exception. "BLA"))))))

  (testing "Cancelled task"
    (let [f (pool/future executor (Thread/sleep 10000))]
      (.cancel f true)
      (is (thrown? java.util.concurrent.CancellationException @f)))))
