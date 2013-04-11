(ns react.semaphore
  (:require [react.future :as future])
  (:import [java.util.concurrent RejectedExecutionException]))

(defprotocol IAsyncSemaphore
  (acquire [this])
  (release [this]))

(def cas! compare-and-set!)

(deftype AsyncSemaphore [state max-waiters]
  IAsyncSemaphore
  (acquire [this]
    (if-let [p (let [{:keys [permits waiters] :as s} @state]
                 (if (> permits 0)
                     ; OK
                     (when (cas! state s (update-in s [:permits] dec))
                           (future/successful nil))
                     (if (< (count waiters) max-waiters)
                         ; WAIT
                         (let [p (future/promise)]
                           (when (cas! state s (update-in s [:waiters] conj p))
                                 p))
                         ; Exception
                         (future/failed (RejectedExecutionException. "Max waiters exceeded")))))]
      p
      (recur)))
  
  (release [this]
    (when-not (let [{:keys [permits waiters] :as s} @state]
                (if (empty? waiters)
                    ; INC
                    (cas! state s (update-in s [:permits] inc))
                    ; Notify first waiter
                    (when (cas! state s (update-in s [:waiters] rest))
                          (future/success (first waiters) nil))))
      (recur))))

(defn async-semaphore
  [permits & {:keys [max-waiters] :or {max-waiters Integer/MAX_VALUE}}]
  (AsyncSemaphore. (atom {:permits permits
                          :waiters []})
                   max-waiters))
