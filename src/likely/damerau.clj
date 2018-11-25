(ns likely.damerau
  (:import likely.DamerauLevenshtein))


(defn- substr [s i l]
  (when s
    (if (> (+ i l) (count s))
      (if (> i (count s))
        ""
        (subs s i))
      (subs s i l))))

(defn damerau
  ([src target max]
   (damerau (substr src 0 max) (substr target 0 max)))
  ([src target]
   (cond (empty? src) (count target)
         (empty? target) (count src)
         true    (DamerauLevenshtein/distanceOf src target))))
