(ns react.pool
  (:refer-clojure :exclude [future future-call])
  (:require [react.future :as future])
  (:import [java.util.concurrent Future ExecutorService CancellationException]))

;; Almost identical copies of clojure.core future, only difference is
;; the executor parameter

(defn ^:private binding-conveyor-fn
  [f]
  (let [frame (clojure.lang.Var/getThreadBindingFrame)]
    (fn
      ([]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f))
      ([x]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x))
      ([x y]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y))
      ([x y z]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y z))
      ([x y z & args]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (apply f x y z args)))))

(defn ^:static future-call
  "Takes an executor instance and a function of no args and yields a
   (async) future object that will invoke the function in another thread,
   and will cache the result and return it on all subsequent calls to
   deref/@. If the computation has not yet finished, calls to deref/@
   will block, unless the variant of deref with timeout is used."
  [^ExecutorService executor f & {:keys [preserve-bindings?]
                                  :or {preserve-bindings? true}}]
  (let [promise (future/promise)
        func    (fn [] (try (future/success promise (f))
                            (catch Throwable t
                                   (future/failure promise t))))
        result  (.submit ^ExecutorService executor
                         ^Callable (if preserve-bindings?
                                       (binding-conveyor-fn func)
                                       func))]
    (reify
      react.future.IFuture
      (on-complete [_ f]
        (future/on-complete promise f))
      
      clojure.lang.IDeref
      (deref [_]
        (deref promise))
      
      clojure.lang.IBlockingDeref
      (deref [_ timeout-ms timeout-val]
        (deref promise timeout-ms timeout-val))
      
      clojure.lang.IPending
      (isRealized [_]
        (realized? promise))
      
      Future
      (get [_]
        (.get ^Future result))
      
      (get [_ timeout unit]
        (.get ^Future result timeout unit))
      
      (isCancelled [_]
        (.isCancelled ^Future result))
        
      (isDone [_]
        (.isDone ^Future result))
        
      (cancel [_ interrupt?]
        (future/failure promise (CancellationException.))
        (.cancel ^Future result interrupt?)))))

(defmacro future
  "Takes an executor instance and a body of expressions and yields a
   (async) future object that will invoke the body in another thread,
   and will cache the result and return it on all subsequent calls to
   deref/@. If the computation has not yet finished, calls to deref/@
   will block, unless the variant of deref with timeout is used.."
  [^ExecutorService executor & body]
  `(future-call ~executor (^{:once true} fn* [] ~@body)))
