#!/usr/bin/env bb

(def parsed-csv (-> *command-line-args*
                    first
                    slurp
                    csv/read-csv))

(let [header (map keyword (first parsed-csv))
      records (map #(zipmap header %) (rest parsed-csv))]
  (prn (vec records)))

(System/exit 0)
