(ns test-core
  (:use [clojure.test]
        [http-request]))

(def qs "fruit=apple&pet=goldfish;drink=cold+beer;drink=water&phrase=Hello%20World")


(comment 
  (def req {:GET (multi-map (querystring-as-multimap qs))})

  ((req :GET) "drink")
  ((req :GET) "phrase")
  ((req :GET) "pet")
  ((req :GET) :all "pet")
  ((req :GET) :all "drink"))


(deftest url-unquote-test
  (is (= "hello world" (url-unquote "hello%20world"))))

(deftest querystring-as-seq-test
  (is (= '(("fruit" "apple")
           ("pet" "goldfish")
           ("drink" "cold beer")
           ("drink" "water")
           ("phrase" "Hello World"))
         (querystring-as-seq qs))))

(deftest querystring-as-multimap-test
  (let [p (querystring-as-multimap qs)]
    (is (= "apple" (p "fruit") ))
    (is (= "goldfish" (p "pet")))
    (is (= ["water" "cold beer"] (p "drink")))
    (is (= "Hello World" (p "phrase")))))

(deftest multi-map-test
  (let [m (multi-map {"single" "one" "many" ["one" "two" "three"]})]
    (is (= "one" (m "single")))
    (is (= ["one"] (m :all "single")))
    (is (= "three" (m "many")))
    (is (= ["one" "two" "three"] (m :all "many")))
    (is (nil? (m "not here")))
    (is (= [] (m :all "not here")))))