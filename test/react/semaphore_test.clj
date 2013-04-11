(ns react.semaphore-test
  (:use clojure.test)
  (:require [react.semaphore :as semaphore]))

(deftest semaphore-test
  (testing "Idle"
    (let [s (semaphore/async-semaphore 1)]
      (is (= (-> s .state deref :permits) 1))
      (is (= (-> s .state deref :waiters count) 0))))
    
  (testing "Acquire / Release"
    (let [s (semaphore/async-semaphore 1)
          a (semaphore/acquire s)
          b (semaphore/acquire s)
          c (semaphore/acquire s)]
      (is (= (-> s .state deref :permits) 0))
      (is (= (-> s .state deref :waiters count) 2))
      
      (is (= @a nil))
      (is (false? (realized? b)))
      (is (false? (realized? c)))
      
      (semaphore/release s)
      
      (is (= (-> s .state deref :permits) 0))
      (is (= (-> s .state deref :waiters count) 1))
      
      (is (= @b nil))
      (is (false? (realized? c)))
      
      (semaphore/release s)
      
      (is (= (-> s .state deref :permits) 0))
      (is (= (-> s .state deref :waiters count) 0))
      
      (is (= @c nil))
      
      (semaphore/release s)
      
      (is (= (-> s .state deref :permits) 1))
      (is (= (-> s .state deref :waiters count) 0))))
  
  (testing "Acquire / Release with max-waiters"
    (let [s (semaphore/async-semaphore 1 :max-waiters 1)
          a (semaphore/acquire s)
          b (semaphore/acquire s)]
      (is (= (-> s .max-waiters) 1))
      
      (is (= (-> s .state deref :permits) 0))
      (is (= (-> s .state deref :waiters count) 1))
      
      (is (= @a nil))
      (is (false? (realized? b)))
      
      (is (thrown? java.util.concurrent.RejectedExecutionException
                   @(semaphore/acquire s))))))
