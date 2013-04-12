(ns react.queue-test
  (:use clojure.test)
  (:require [react.future :as future]
            [react.queue  :as queue]))

(deftest async-queue-test  
  (testing "Idle"
    (let [q (queue/async-queue)]
      (is (= (-> q .state deref :type) :idle))
      (is (= (-> q .state deref :pollers count) 0))
      (is (= (-> q .state deref :offers count) 0))
      (is (= (-> q .state deref :exp) nil))))
    
    (testing "Polling"
      (let [q  (queue/async-queue)
            p1 (queue/poll q)
            p2 (queue/poll q)
            p3 (queue/poll q)]
        (is (= (-> q .state deref :type) :polling))
        (is (= (-> q .state deref :pollers count) 3))
        (is (= (-> q .state deref :offers count) 0))
        (is (= (-> q .state deref :exp) nil))
        
        (is (false? (realized? p1)))
        (is (false? (realized? p2)))
        (is (false? (realized? p3)))
        
        (queue/offer q 1)
        (is (= @p1 1))
        
        (queue/offer q 2)
        (is (= @p2 2))
        
        (queue/offer q 3)
        (is (= @p3 3))
        
        (is (= (-> q .state deref :type) :idle))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 0))
        (is (= (-> q .state deref :exp) nil))))
  
    (testing "Offering"
      (let [q (queue/async-queue)]
        (queue/offer q 1)
        (queue/offer q 2)
        (queue/offer q 3)
      
        (is (= (-> q .state deref :type) :offering))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 3))
        (is (= (-> q .state deref :exp) nil))
      
        (is (= @(queue/poll q) 1))
        (is (= @(queue/poll q) 2))
        (is (= @(queue/poll q) 3))
      
        (is (= (-> q .state deref :type) :idle))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 0))
        (is (= (-> q .state deref :exp) nil))))
  
    (testing "Fail the queue when pollers are waiting"
      (let [q   (queue/async-queue)
            p1  (queue/poll q)
            p2  (queue/poll q)
            p3  (queue/poll q)
            exp (Exception. "FAILED")]
        
        (queue/fail q exp)
        
        (is (= (-> q .state deref :type) :failed))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 0))
        (is (= (-> q .state deref :exp) exp))
        
        (is (thrown? Exception @p1))
        (is (thrown? Exception @p2))
        (is (thrown? Exception @p3))))
    
    (testing "Fail the queue when offers are waiting"
      (let [q   (queue/async-queue)
            exp (Exception. "FAILED")]
        (queue/offer q 1)
        (queue/offer q 2)
        (queue/offer q 3)
        
        (is (= (-> q .state deref :type) :offering))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 3))
        (is (= (-> q .state deref :exp) nil))
        
        (queue/fail q exp)
        
        (is (= (-> q .state deref :type) :failed))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 3))
        (is (= (-> q .state deref :exp) exp))))
    
    (testing "Fail the queue wich offers are waiting and ensure that offers are not lost"
      (let [q   (queue/async-queue)
            exp (Exception. "FAILED")]
        (queue/offer q 1)
        
        (is (= (-> q .state deref :type) :offering))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 1))
        (is (= (-> q .state deref :exp) nil))
        
        (queue/fail q exp)
        
        (is (= (-> q .state deref :type) :failed))
        (is (= (-> q .state deref :pollers count) 0))
        (is (= (-> q .state deref :offers count) 1))
        (is (= (-> q .state deref :exp) exp))
        
        (is (= @(queue/poll q) 1))
        (is (thrown? Exception @(queue/poll q))))))
