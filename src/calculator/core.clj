(ns calculator.core
  (:gen-class)
  (:require
    [incanter.core :as core]
    [incanter.bayes :as bayes]
    [incanter.charts :as charts]
    [incanter.datasets :as ds]
    [incanter.pdf :as pdf]
    [incanter.stats :as stats]
    [incanter.io :as io]
    [clojure.string :as string]
    [clojure.walk :refer [postwalk]] ;REPL: (require '[clojure.walk :refer [postwalk]])
    ))

(defn e [a b]
"
Function overlaoding to do exponents.
"
  	(bigdec (Math/pow a b))
)

(defn strToInt [s]
"Turns a string into an big decimal"
  (bigdec (str s))
)



(defn find-last-op-index [eq]
"Returns the next index of the next symbol in the order of operations."

  (let [op-sub (.indexOf eq (symbol "-"))
        op-add (.indexOf eq (symbol "+"))
        op-div (.indexOf eq (symbol "/"))
        op-mult (.indexOf eq (symbol "*"))
        op-e (.indexOf eq (symbol "e"))
      ]
  
    (cond
      (not (= -1 op-sub)) op-sub
      (not (= -1 op-add)) op-add
      (not (= -1 op-div)) op-div
      (not (= -1 op-mult)) op-mult
      (not (= -1 op-e)) op-e
      :else -1
    )

  )
)



(defn evaluate [node]
  (if-let [sym (:function node)]
    (if (= (.indexOf ["e"] (str sym)) 0)
      ;(symbol "calculator.core" sym)
      (bigdec (apply (resolve (symbol "calculator.core" (str sym))) (:children node)))
      (bigdec (apply (resolve sym) (:children node)))
    )
    node
  )
)




(defn build-tree [expression]
  (if (= (count expression) 1)
    (cond 
      (vector? (nth expression 0)) (build-tree (nth expression 0)) 
      :else (strToInt (nth expression 0))
    )
    
    (let [
      lastOp (find-last-op-index expression)
      newLi [(subvec expression 0 lastOp) (subvec expression (+ lastOp 1))]
      expression-tree {:function (symbol (nth expression lastOp))
                        :children [
                          (build-tree (nth newLi 0))
                          (build-tree (nth newLi 1))
                        ]
                      }
      
    ]
      expression-tree
    )
  )
)



"
Def seperate-parts:
  [(2 + (3 + 4)) + (3 + 9 + (2 - 3) + ((3 + 4) + 1) + 2)]
  [ [2 + ( 3 + 4 )] + [3 + 9 + ( 2 - 3 ) + ( ( 3 + 4 ) + 1 ) + 2] ]
  [ [2 + [ 3 + 4 ]] + [3 + 9 + [ 2 - 3 ] + [ ( 3 + 4 ) + 1 ] + 2] ]
  [ [2 + [ 3 + 4 ]] + [3 + 9 + [ 2 - 3 ] + [ [ 3 + 4 ] + 1 ] + 2] ]
"

(defn split-on 

  ([string delimiters] 
    (def final-list [])
    (def running-string "")
    (def updated-string1 (clojure.string/replace string #"\^" "e"))
    (def updated-string2 (clojure.string/replace updated-string1 #"\(" "["))
    (def updated-string3 (clojure.string/replace updated-string2 #"\)" "]"))
    (split-on updated-string3 delimiters final-list running-string)
  ) 

  ([string delimiters final-list running-string]
    (if (= (count string) 0)
      (into [] (concat final-list [(strToInt running-string)]))
      (if (clojure.string/index-of delimiters (nth string 0))
        (let [new-list (into [] (concat final-list 
                                  (if (= (count running-string) 0)
                                    [(str (nth string 0))]
                                    (into [] (concat  [(strToInt running-string)] [(str (nth string 0))]))
                                  ) 
                                )
                        )
              new-running-string ""
             ]
             (cond 
              (= (count string) 1) new-list
              :else (split-on (subs string 1) delimiters new-list new-running-string)
             )
        )
        (let [new-string (apply str (concat running-string (str (nth string 0))))]
          (split-on (subs string 1) delimiters final-list new-string)
        ) 
        ; Throw error if neither condition is met.
      )
    )
  )

)


(defn -main
  "This evaluates mathematical expressions"
  [& args]
  (def delimiters "e+-*/()[]!")
  (def expression-list (split-on (apply str args) delimiters))
  (def str-to-eval (with-out-str (println expression-list)))
  ;(println str-to-eval)
  (def new-expression-list (read-string str-to-eval))
  ;(println new-expression-list)
  (def expression-tree (build-tree new-expression-list))
  ;(println expression-tree)
  
  (println (postwalk evaluate expression-tree))
) 