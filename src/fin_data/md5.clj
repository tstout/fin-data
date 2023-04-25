(ns fin-data.md5
  (:import [java.security MessageDigest]
           [java.util Base64]))

(defn base64-encode [text]
  (-> (Base64/getEncoder)
      (.encodeToString text)))

(defn md5 [s]
  {:pre [(string? s)]}
  (let [md (MessageDigest/getInstance "MD5")]
    (.update md (.getBytes s))
    (base64-encode (.digest md))))

(comment
  *e
  (md5 "abcdefhijklmnop")
  
  (md5 1.0)
  (time (md5 "abcdelskiekskallajf"))


  ;;
  )