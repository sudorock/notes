(ns notes.something
  (:require [com.climate.claypoole :as cp]
            [com.climate.claypoole.lazy :as cpl]
            [com.climate.claypoole.impl :as impl]
            [allstreet.web-drivers.curl :as curl]
            [allstreet.prelude.core :as p]
            [allstreet.web-drivers.hybrid :as hybrid]
            [allstreet.prelude.seqs :as seqs]
            [allstreet.web-drivers.playwright :as playwright])
  (:import [clojure.lang PersistentQueue]
           [java.util.concurrent LinkedBlockingQueue Executors ExecutorService]))

(set! *warn-on-reflection* true)

(defn results
  [pool buffer-size f & colls]
  (if (cp/serial? pool)
    (apply map f colls)
    (let [[shutdown? pool] (impl/->threadpool pool)
          buffer-size (or buffer-size (impl/get-pool-size pool) 10)
          result-q    (LinkedBlockingQueue. (int buffer-size))
          run-one     (fn [a]
                        (let [p (promise)]
                          @(deliver p
                                    (cp/future-call
                                     pool
                                     ;; Use with-meta for priority threadpools
                                     (with-meta #(try (apply f a)
                                                      (finally (.put result-q @p)))
                                                {:args a})))))]
      (->> colls
           ;; make sure we're not chunking
           (map impl/unchunk)
           ;; use map to take care of argument alignment
           (apply map vector)
           ;; make futures
           (map run-one)
           ;; force buffer-size futures to start work in the pool
           ;; read the results from the futures in the queue
           (map (fn [_] (impl/deref-fixing-exceptions (.take result-q))))
           (impl/seq-open #(if shutdown? (cp/shutdown pool)))))))


(defn process
  [[step & remaining-steps] ^LinkedBlockingQueue result-q input]
  (when-let [{:keys [pool f early-return-pred]
              :or   {early-return-pred (constantly false)}} step]
    (.submit ^ExecutorService pool
             ^Runnable (fn []
                         (try
                           (let [result        (f input)
                                 early-return? (or (early-return-pred result)
                                                   (empty? remaining-steps))]
                             (if early-return?
                               (.put result-q result)
                               (process remaining-steps result-q result)))
                           (catch Throwable t
                             (prn t)))))))

(defn results
  [steps buffer-size coll]
  (let [result-q (LinkedBlockingQueue. (int buffer-size))
        steps    (mapv #(assoc % :pool (Executors/newScheduledThreadPool (:threads % 1))) steps)
        process  (partial process steps result-q)
        shutdown #(doseq [{:keys [pool]} steps]
                    (.shutdown ^ExecutorService pool))]
    (->> coll
         (map process)
         (seque buffer-size)
         (map (fn [_] (.take result-q)))
         (p/seq-with-finally shutdown))))

(comment
 ()
 (def pipeline
   [{:f                 #(curl/get % {:retry? false})
     :early-return-pred #(<= (:status %) 399)
     :name              :curl
     :threads           5}
    {:f       #(playwright/get (:url %) {:driver/type :playwright/firefox})
     :name    :play
     :threads 5}])

 (def test-pipeline
   [{:f                 (fn [i]
                          (Thread/sleep 2000)
                          i)
     :early-return-pred (fn [i]
                          (odd? i))
     :threads           10}
    {:f       (fn [i]
                (Thread/sleep 5000)
                i)
     :threads 10}])


 (count r)

 (def r1 (time (doall (results test-pipeline 5 (range 10)))))
 (def r2 (time (doall (cpl/upmap 5 (fn [i]
                                     (Thread/sleep 2000)
                                     (when (odd? i)
                                       (Thread/sleep 5000))
                                     i)
                                 (range 10)))))

 (frequencies (map :status r1))
 (frequencies (map :status r2))
 (map :time-ms r1)
 (time (curl/get "https://www.idx.co.id/en-us/listed-companies/company-profiles/"))
 (def r1 (time (doall (results pipeline 5 urls))))
 (def r2 (time (doall (cpl/upmap 5 #(hybrid/get %) urls)))))

(def urls
  (flatten (repeat 5 ["https://www.idx.co.id/en-us/listed-companies/company-profiles/"
                      "https://www.idx.co.id/perusahaan-tercatat/prospektus/"])))
(comment
 (hybrid/get "https://www.borsabcn.es/ing/aspx/Empresas/Empresas.aspx"))

(comment
 (def urls
   ["https://www.borsabcn.es/ing/aspx/Empresas/Empresas.aspx"
    "https://www.bolsabilbao.es/ing/aspx/Empresas/Empresas.aspx"
    "https://www.nse.co.ke/"
    "https://www.bolsamadrid.es/esp/aspx/Empresas/Exclusiones.aspx"
    "https://www.athexgroup.gr/el/web/guest/companies-map/-/cmap/m/-1/2"
    "https://www.meff.es/ing/Home"
    "https://www.bursamalaysia.com/bm/"
    "https://www.bolsamadrid.es/esp/aspx/Empresas/Admisiones.aspx"
    "https://dsebd.org/company_listing.php"
    "https://www.bolsavalencia.es/esp/aspx/Portada/Portada.aspx"
    "https://www.idx.co.id/perusahaan-tercatat/prospektus/"
    "https://nasdaqbaltic.com/statistics/en/news"
    "https://www.kap.org.tr/en/bist-sirketler"
    "https://zse.hr/en/papir-311/310?isin=HR3MAJRA0002"
    "https://nasdaqbaltic.com/statistics/lv/shares"
    "https://hnx.vn/vi-vn/"
    "https://www.tpex.org.tw/web/regular_emerging/apply_schedule/applicant/applicant_companies.php?l=zh-tw"
    "https://www.idx.co.id/en-us/listed-companies/company-profiles/"
    "https://live.euronext.com/en/markets/oslo/equities/list"
    "https://nasdaqbaltic.com/lv/"
    "https://www.nsx.com.au/marketdata/announcements/"
    "https://nasdaqbaltic.com/statistics/lt/news?filter=1&num=25"
    "https://zse.hr/en/list-of-issuers/178"
    "https://www.nsx.com.au/marketdata/company-directory/details/8EP/"
    "https://www.meff.es/esp/Home"
    "https://www.dsebd.org/forthcoming.php"
    "https://www.borsabcn.es/esp/aspx/Empresas/HechosRelevantesUltimos.aspx"
    "https://www.cse.com.cy/el-GR/home/"
    "https://www.bvb.ro/FinancialInstruments/Details/FinancialInstrumentsDetails.aspx?s=TTS"]))

(defn get-results
  [urls n-light n-heavy]
  (let [light-chan   (to-chan! urls)
        heavy-chan   (chan 20)
        results-chan (chan 20)

        _            (prn 9)
        light-tasks  (doall (repeatedly n-light #(go-loop []
                                                          (if-let [url (<! light-chan)]
                                                            (let [res #_(curl/get url {:retry? false}) (<! (a/thread (curl/get url {:retry? false})))]
                                                              (prn "c" (put! heavy-chan res))
                                                              (recur))
                                                            (close! heavy-chan)))))

        _            (prn 10)
        heavy-tasks  (doall (repeatedly n-heavy #(go-loop []
                                                          (if-let [{:keys [status url] :as res} (<! heavy-chan)]
                                                            (if (and status (> status 399))
                                                              (let [res (<! (a/thread (ply/get url)))]
                                                                (prn "b1" (put! results-chan res))
                                                                (recur))
                                                              (do
                                                                (prn "b2" (put! results-chan res))
                                                                (recur)))
                                                            (close! results-chan)))))]

    #_(dotimes [_ 2]
        (go-loop []
                 (if-let [url (<! light-chan)]
                   (let [res (curl/get url {:retry? false})]
                     (when (put! heavy-chan res)
                       (prn "c succeeded"))
                     (recur))
                   (close! heavy-chan))))

    #_(dotimes [_ 2]
        (go-loop []
                 (if-let [{:keys [status url] :as res} (<! heavy-chan)]
                   (if (and status (> status 399))
                     (let [res (ply/get url)]
                       (when (put! results-chan res)
                         (prn "b1 succeeded"))
                       (recur))
                     (do
                       (when (put! results-chan res)
                         (prn "b2 succeeded"))
                       (recur)))
                   (close! results-chan))))

    #_(<!! (a/map identity tasks))
    #_(close! results-chan)
    (<!! (a/into [] results-chan))))

(def urls ["https://www.borsabcn.es/ing/aspx/Empresas/Empresas.aspx"
           "https://www.bolsabilbao.es/ing/aspx/Empresas/Empresas.aspx"
           "https://www.nse.co.ke/"
           "https://www.bolsamadrid.es/esp/aspx/Empresas/Exclusiones.aspx"
           "https://www.athexgroup.gr/el/web/guest/companies-map/-/cmap/m/-1/2"
           "https://www.meff.es/ing/Home"
           "https://www.bursamalaysia.com/bm/"
           "https://www.bolsamadrid.es/esp/aspx/Empresas/Admisiones.aspx"
           "https://dsebd.org/company_listing.php"
           "https://www.bolsavalencia.es/esp/aspx/Portada/Portada.aspx"
           "https://www.idx.co.id/perusahaan-tercatat/prospektus/"
           "https://nasdaqbaltic.com/statistics/en/news"
           "https://www.kap.org.tr/en/bist-sirketler"
           "https://zse.hr/en/papir-311/310?isin=HR3MAJRA0002"
           "https://nasdaqbaltic.com/statistics/lv/shares"
           "https://hnx.vn/vi-vn/"
           "https://www.tpex.org.tw/web/regular_emerging/apply_schedule/applicant/applicant_companies.php?l=zh-tw"
           "https://www.idx.co.id/en-us/listed-companies/company-profiles/"
           "https://live.euronext.com/en/markets/oslo/equities/list"
           "https://nasdaqbaltic.com/lv/"
           "https://www.nsx.com.au/marketdata/announcements/"
           "https://nasdaqbaltic.com/statistics/lt/news?filter=1&num=25"
           "https://zse.hr/en/list-of-issuers/178"
           "https://www.nsx.com.au/marketdata/company-directory/details/8EP/"
           "https://www.meff.es/esp/Home"
           "https://www.dsebd.org/forthcoming.php"
           "https://www.borsabcn.es/esp/aspx/Empresas/HechosRelevantesUltimos.aspx"
           "https://www.cse.com.cy/el-GR/home/"
           "https://www.bvb.ro/FinancialInstruments/Details/FinancialInstrumentsDetails.aspx?s=TTS"])

(comment
 (require '[allstreet.web-drivers.curl :as curl])
 (time (curl/get "https://vikatan.quintype.io/ping"))
 (def good-urls (read-string (slurp "/Users/indy/Downloads/webdriver/good-urls.edn")))
 (require '[com.climate.claypoole :as cp])
 (frequencies (map :status trial-6))

 "https://www.jse.co.za/"
 "https://www.bmv.com.mx/en/issuers/issuers-information"
 (take 50 good-urls)
 (map :url (filter #(< (:time-ms) 10000) trial))
 (set (map :url (->> trial
                     (filter #(< (:time-ms %) 10000))
                     (remove :fail))))

 (get "https://www.idx.co.id/perusahaan-tercatat/prospektus/")
 '("https://www.idx.co.id/perusahaan-tercatat/prospektus/"
   "https://www.idx.co.id/en-us/listed-companies/company-profiles/")
 (get "https://www.borsabcn.es/esp/aspx/Empresas/HechosRelevantesUltimos.aspx" {:playwright/driver-pool pool})
 (map :url (remove :fail? trial-3))
 (count (filter :fail? (filter #(< (:time-ms %) 10000) trial)))
 (map :url (filter #(> (:status %) 399) trial-6))
 (def trial-6 (time (vec (cp/upmap 32
                                   (fn [url]
                                     (prn url)
                                     (curl/get url))
                                   (take 32 urls)))))
 (def pool-ply-6 nil))
