(ns goodreads.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [clj-http.client :as client]
            [clojure.xml :as xml]
            [clojure.data.zip.xml :as zx]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [manifold.deferred :as d]))

; key: U72x1vCYKt4nFpQLhJ3g
; secret: tBF5p5i2aIkvNo3GHOwj8DoVty0ZvtO1iv8R7V94PM

(defn client-get [url]
  (client/get url {:cookie-policy :standard}))

(defn goodreads-get-books [token user-id]
  (client-get (str "https://www.goodreads.com/review/list?key=" token "&v=2&id=" user-id)))

(defn goodreads-book-info [token book-id]
  (client-get (str "https://www.goodreads.com/book/show/" book-id ".XML?key=" token)))

(defn zip-str [s]
  (zip/xml-zip
   (xml/parse (java.io.ByteArrayInputStream. (.getBytes s "UTF-8")))))

(defn book-status [review]
  (let [id-tag (zx/xml1-> review :book :id)
        shelf-tag (zx/xml1-> review :shelves :shelf)]
    {:id (zx/text id-tag)
     :status (keyword (zx/attr shelf-tag :name))}))

(defn read-book [book]
  (let [book-xml1-> (partial zx/xml1-> book)
        id-tag (book-xml1-> :id)
        title-tag (zx/xml1-> book :title)
        link-tag (zx/xml1-> book :link)
        authors-name-tags (zx/xml-> book :authors :author :name)
        average_rating-tag (zx/xml1-> book :average_rating)]
    {:id (zx/text id-tag)
     :title (zx/text title-tag)
     :link (zx/text link-tag)
     :author-names (map zx/text authors-name-tags)
     :average-rating (bigdec (zx/text average_rating-tag))}))

(defn similar-book [token id]
  (let [book-zip (zip-str (:body (goodreads-book-info token id)))]
    (map read-book (zx/xml-> book-zip :book :similar_books :book))))

(defn user-books-provider [token id]
  (let [books-zip (zip-str (:body (goodreads-get-books token id)))]
    (map book-status (zx/xml-> books-zip :reviews :review))))

(defn similar-provider [token book-ids]
  (flatten (pmap #(similar-book token %) book-ids)))

(defn recomendations
  [{:keys [token user-id number-books]}]
  (let [user-books (user-books-provider token user-id)
        user-readed-book-ids (->> user-books
                                  (filter #(= (:status %) :read))
                                  (map :id))
        not-read (fn [{id :id}] (not-any? #(= % id) user-readed-book-ids))]
    (d/future (->> user-books
                   (map :id)
                   (similar-provider token)
                   (filter not-read)
                   (sort-by :average-rating >)
                   (take number-books)))))

(def cli-options [["-n" "--number-books NUMBER" "How many books do you want to recommend"
                   :default 10
                   :parse-fn #(Integer/parseInt %)
                   :validate [#(< 0 % 500) "Must be a number between 0 and 500"]]
                  ["-t" "--timeout-ms MILLISECONDS" "Wait before finished"
                   :default 10000
                   :parse-fn #(Integer/parseInt %)
                   :validate [#(< 0 % 300000) "Must be a number between 0 and 300000"]]
                  ["-h" "--help"]])

(defn book->str [{:keys [title link author-names]}]
  (format "\"%s\" by %s\nMore: %s"
          title
          (str/join ", " author-names)
          link))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary) (System/exit 0))
      (some? errors) (do (println errors) (System/exit 1))
      (empty? args) (do (println "Please, specify user's token") (System/exit 1))
      :else (let [[token user-id] args
                  config {:token token :user-id user-id :number-books (:number-books options)}
                  books (-> (recomendations config)
                            (d/timeout! (:timeout-ms options) ::timeout)
                            deref)]
              (cond
                (= ::timeout books) (println "Not enough time :(")
                (empty? books) (println "Nothing found, leave me alone :(")
                :else (doseq [[i book] (map-indexed vector books)]
                        (println (str "#" (inc i)))
                        (println (book->str book))
                        (println)))))))