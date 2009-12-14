(ns http-request)

(let [cmap (merge
            (into {} (map (fn [x] [(format "%02x" x) (char x)]) (range 256)))
            (into {} (map (fn [x] [(format "%02X" x) (char x)]) (range 256))))]
  (defn url-unquote [s]
    (loop [[head tail] (.split s "%" 2)]
      (if (empty? tail)
        head
        (if (> (count tail) 1)
          (let [hex (subs tail 0 2)
                ch (or (cmap hex) (str "%" hex))
                [h t] (.split (subs tail 2) "%" 2)]
            (recur [(str head ch h) t]))
          (str head tail))))))


(defn querystring-as-seq [qs]
  (map
   (fn [kv]
     (map url-unquote
          (map #(.replace % "+" " ")
               (.split kv "=" 2)))) (.split qs "&|;")))


(defn querystring-as-multimap [qs]
  (loop [coll (querystring-as-seq qs) map {}]
    (if (empty? coll)
      map
      (recur
       (rest coll)
       (merge-with
        (fn [x y] (if (vector? x) (conj x y) [x y]))
        (apply assoc {} (first coll)) map)))))


(defn multi-map [multimap]
  (fn
    ([key]
       (if (coll? (multimap key))
         (last (multimap key))
         (multimap key)))
    ([option key]
       (option
        {:all (if (coll? (multimap key))
                (multimap key)
                (if (contains? multimap key)
                  (vector (multimap key))
                  (vector)))}))))


