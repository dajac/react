(ns react.queue
  (:require [react.future :as future]))

(defprotocol IAsyncQueue
  ""
  (poll [this])
  (offer [this val])
  (fail [this exp]))

; Alias to improve readability
(def cas! compare-and-set!)

(deftype AsyncQueue [state]
  IAsyncQueue
  (poll [this]
    (if-let [p (let [{:keys [type pollers offers exp] :as s} @state]
                 (condp = type
                   :idle     (let [p (future/promise)]
                               (when (cas! state s {:type :polling :pollers [p]})
                                     p))
                   
                   :polling  (let [p (future/promise)]
                               (when (cas! state s {:type :polling :pollers (conj pollers p)})
                                     p))
                   
                   :offering (when (cas! state s (if (> (count offers) 1)
                                                     {:type :offering :offers (rest offers)}
                                                     {:type :idle}))
                                   (future/successful (first offers)))
                   
                   :failed   (if (pos? (count offers))
                                 (when (cas! state s {:type :failed :offers (rest offers) :exp exp})
                                       (future/successful (first offers)))
                                 (future/failed exp))))]
      p
      (recur)))
  
  (offer [this val]
    (when-not (let [{:keys [type pollers offers exp] :as s} @state]
                (condp = type
                  :idle     (cas! state s {:type :offering :offers [val]})
                   
                  :offering (cas! state s {:type :offering :offers (conj offers val)})
                   
                  :polling  (when (cas! state s (if (> (count pollers) 1)
                                                    {:type :polling :pollers (rest pollers)}
                                                    {:type :idle}))
                                  (future/success (first pollers) val))
                   
                  :failed   true))
      (recur val)))
  
  (fail [this exp]
    (when-not (let [{:keys [type pollers offers] :as s} @state]
                (condp = type
                  :idle     (cas! state s {:type :failed :exp exp})
                  
                  :offering (cas! state s {:type :failed :offers offers :exp exp})
                  
                  :polling  (when (cas! state s {:type :failed :exp exp})
                                  (doseq [p pollers] (future/failure p exp))
                                  true)
                  
                  :failed   true))
      (recur exp))))

(defn async-queue
  ""
  []
  (AsyncQueue. (atom {:type :idle})))

(defn queue->lazy-seq
  ""
  [queue]
  (cons @(poll queue) (lazy-seq (queue->lazy-seq queue))))

