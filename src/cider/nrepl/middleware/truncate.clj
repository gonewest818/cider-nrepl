(ns cider.nrepl.middleware.truncate
  "Substitute truncated output in place of excessively long outputs,
  and store the full results in a cache for later inspection."
  (:refer-clojure :exclude [read-string])  ;; ???
  (:require [cider.nrepl.middleware.util.error-handling :refer [base-error-response]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.string :as string])   ;; ??? ensure we need this
  (:import clojure.tools.nrepl.transport.Transport))

(def cache (atom {}))
(def maximum-length 250)  ; TODO: make this value configurable

(defn truncated-response
  "If the length of the response's value exceeds the configured
  threshold, then truncate the string and save the full reply for
  later. Return a modified response containing the truncated string as
  well as a :truncate-hash key that can be supplied to the
  \"truncate-get-cached\" or \"truncate-delete-hashed\" ops."
  [{:keys [transport] :as msg} response which-key]
  (let [string-value (get response which-key)]
    ;; truncate only string responses exceeding the threshold
    (if (and (= java.lang.String (type string-value))
             (some? maximum-length)
             (> (count string-value) maximum-length))
      (let [trunc (str (subs string-value 0 maximum-length) "...")
            hash-key (-> response
                         (select-keys [:session :id :time-stamp])
                         hash
                         Long/toHexString)]
        (swap! cache assoc hash-key string-value)
        (assoc response
               which-key trunc
               :truncate-hash hash-key
               :truncate-which which-key))
      response)))

(defn truncate-transport
  [{:keys [^Transport transport] :as msg}]
  (reify Transport
    (recv [this] (.recv transport))
    (recv [this timeout] (.recv transport timeout))
    (send [this response]
      (if-let [which-key (some #{:value :pprint-out} (keys response))]
        (.send transport (truncated-response msg response which-key))
        (.send transport response))
      this)))

(defn eval-msg
  [msg]
  (assoc msg :transport (truncate-transport msg)))

(defn eval-reply
  [handler msg]
  (handler (eval-msg msg)))

(defn- success [{:keys [transport] :as msg} result-map]
  (transport/send transport (response-for msg result-map {:status :done})))

(defn- failure [{:keys [transport] :as msg} err err-kw]
  (transport/send transport (base-error-response msg err err-kw :done)))

(defn get-cached [{:keys [hash-key] :as msg}]
  (try (if-let [full (and hash-key (get @cache hash-key))]
         (success msg {:value full :retrieved-key hash-key})
         (success msg {:value nil :retrieved-key nil}))
       (catch Exception e (failure msg e :truncate-get-cached-error))))

(defn delete-cached [{:keys [hash-key] :as msg}]
  (try (and hash-key (swap! cache dissoc hash-key))
       (success msg {:deleted-key hash-key})
       (catch Exception e (failure msg e :truncate-delete-cached-error))))

;;; Middleware op handling
(defn handle-truncate [handler msg]
  (case (:op msg)
    "eval" (eval-reply handler msg)
    "truncate-get-cached" (get-cached msg)
    "truncate-delete-cached" (delete-cached msg)
    (handler msg)))
