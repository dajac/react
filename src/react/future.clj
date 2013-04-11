(ns react.future
  (:refer-clojure :exclude [promise map])
  (:require [clojure.tools.logging :as log])
  (:import [java.util.concurrent Executors ScheduledExecutorService
                                 TimeUnit TimeoutException CountDownLatch ScheduledFuture]
           [java.util.concurrent.atomic AtomicInteger]))

(def ^{:dynamic true} *scheduler*
  (Executors/newSingleThreadScheduledExecutor))

(declare promise)
(declare successful)
(declare failed)

(defprotocol IPromise
  ""
  (complete [this value]))

(defprotocol IFuture
  ""
  (on-complete [this f]))

(deftype Promise [^clojure.lang.Atom state
                  ^CountDownLatch latch]
  IPromise
  (complete [this value]
    (let [{:keys [status waitq] :as s} @state]
      (condp = status
        :pending   (if (compare-and-set! state s {:status :completed
                                                  :data   value})
                       (do (.countDown latch)
                           (doseq [f waitq] (f value))
                           true)
                       (recur value))
        :completed false)))
  
  IFuture
  (on-complete [this f]
    ; TODO Create only one chained future per promise
    (let [chained (promise)
          wrapped (fn [v] (try (f v)
                               (catch Throwable t 
                                 (log/errorf t "Exception caught in callback: %s" t)))
                          (complete chained v))]
      ; CAS - lock free!
      ((fn add-or-run [f]
        (let [{:keys [status data waitq] :as s} @state]
          (condp = status
            :pending   (when-not (compare-and-set! state s {:status :pending
                                                            :waitq  (conj waitq f)})
                         (recur f))
            :completed (f data)))) wrapped)
      chained))
  
  clojure.lang.IDeref
  (deref [this]
    (.await latch)
    (let [{:keys [type value]} (:data @state)]
      (case type
        :success value
        :failure (throw value))))
  
  clojure.lang.IBlockingDeref
  (deref [this timeout-ms timeout-val]
    (if (.await latch timeout-ms TimeUnit/MILLISECONDS)
        (let [{:keys [type value]} (:data @state)]
          (case type
            :success value
            :failure (throw value)))
        timeout-val))
  
  clojure.lang.IPending
  (isRealized [this]
    (= (:status @state) :completed)))

(deftype ConstFuture [data]
  IFuture
  (on-complete [this f]
    (try (f data)
         (catch Throwable t
                nil))
    this)
  
  clojure.lang.IDeref
  (deref [_]
    (let [{:keys [type value]} data]
      (case type
        :success value
        :failure (throw value))))
  
  clojure.lang.IBlockingDeref
  (deref [_ _ _]
    (when-let [{:keys [type value]} data]
      (case type
        :success value
        :failure (throw value))))
  
  clojure.lang.IPending
  (isRealized [this]
    true))

;; Constructors

(defn promise
  ""
  []
  (Promise. (atom {:status :pending}) (CountDownLatch. 1)))

(defn successful
  ""
  [value]
  (ConstFuture. {:type :success :value value}))

(defn failed
  ""
  [exp]
  (ConstFuture. {:type :failure :value exp}))

;; Functions

(defn success
  ""
  [promise value]
  (complete promise {:type :success :value value}))

(defn failure
  ""
  [promise exp]
  (complete promise {:type :failure :value exp}))

(defn complete-with
  ""
  [promise future]
  (on-complete future (partial complete promise)))

(defn successful?
  ""
  [future]
  (if (realized? future)
      (try @future
           true
           (catch Throwable t
                  false))
      false))

(defn failed?
  ""
  [future]
  (if (realized? future)
      (try @future
           false
           (catch Throwable t
                  true))
      false))

;; Callbacks

(defn on-success
  ""
  [future f]
  (on-complete future
    (fn [{:keys [type value]}]
      (case type
        :success (f value)
        :failure nil))))

(defn on-failure
  ""
  [future f]
  (on-complete future
    (fn [{:keys [type value]}]
      (case type
        :success nil
        :failure (f value)))))

(defmacro on-complete-fn
  ""
  [future & fntail]
  `(on-complete future (fn ~@fntail)))

(defmacro on-success-fn
  ""
  [future & fntail]
  `(on-success future (fn ~@fntail)))

(defmacro on-failure-fn
  ""
  [future & fntail]
  `(on-failure future (fn ~@fntail)))

;; Combinators

(defn transform-with
  ""
  [future f]
  (let [promise (promise)]
    (on-complete future
      (fn [result]
        (complete-with promise
                       (try (let [res (f result)]
                              (if (satisfies? IFuture res)
                                  res
                                  (successful res)))
                            (catch Throwable t
                                   (failed t))))))
    promise))

(defn map
  ""
  [future f]
  (transform-with future
    (fn [{:keys [type value]}]
      (case type
        :success (f value)
        :failure future))))

(defn recover
  ""
  [future f]
  (transform-with future
    (fn [{:keys [type value]}]
      (case type
        :success future
        :failure (f value)))))

(defmacro transform-with-fn
  ""
  [future & fntail]
  `(transform-with future (fn ~@fntail)))

(defmacro map-fn
  ""
  [future & fntail]
  `(map future (fn ~@fntail)))

(defmacro recover-fn
  ""
  [future & fntail]
  `(recover future (fn ~@fntail)))

(defn within
  ""
  [future timeout-ms]
  (let [p    (promise)
        f    (fn [] (failure p (TimeoutException. (format "Timed out after %s ms" timeout-ms))))
        task (.schedule ^ScheduledExecutorService *scheduler*
                        ^Callable f
                        ^long timeout-ms
                        ^TimeUnit TimeUnit/MILLISECONDS)]
    (on-complete future
      (fn [res]
        (when-not (realized? p)
          (.cancel ^ScheduledFuture task true)
          (complete p res))))
    p))

(defn zip
  ""
  [a b]
  (let [p (promise)]
    (on-complete a
      (fn [{:keys [type value] :as res-a}]
        (case type
          :success (on-complete b
                     (fn [{:keys [type value] :as res-b}]
                       (case type
                         :success (success p (flatten (list (:value res-a)
                                                            (:value res-b))))
                         :failure (failure p value))))
          :failure (failure p value))))
    p))

(defn collect
  ""
  [futures]
  (let [p       (promise)
        counter (AtomicInteger. (count futures))
        results (object-array (count futures))]
    (loop [i 0 futures futures]
      (when-not (empty? futures)
        (on-complete (first futures)
          (fn [{:keys [type value]}]
            (case type
              :success (do (aset results i value)
                           (when (zero? (.decrementAndGet counter))
                             (success p (seq results))))
              :failure (failure p value))))
        (recur (inc i) (rest futures))))
    p))

(defn join
  ""
  [futures]
  (let [p       (promise)
        counter (AtomicInteger. (count futures))]
    (doseq [f futures]
      (on-complete f
        (fn [_]
          (when (zero? (.decrementAndGet counter))
            (success p nil)))))
    p))

(defn select
  ""
  [futures]
  (let [p (promise)]
    (doseq [f futures]
      (on-complete f (partial complete p)))
    p))
