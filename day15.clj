
(require '[clojure.string :as str])
(def INITIAL-HEALTH 200)
(def DEBUG true)

(defn extract-terrain [board]
    ;; list of strings
    (into [] (map (fn [line] (str/replace line #"[EG]" ".")) board)))

(defn extract-units [board]
    ;; unit structure:
    ;; [[row col] chr hp]
    (filter #(str/index-of "EG" (get % 1))
        (for [row (range (count board))
              col (range (count (get board 0)))]
            [[row col] (get (get board row) col) INITIAL-HEALTH])))

(defn open-map [filename]
    (let [file-contents (slurp filename)
          board (str/split file-contents #"\n")]
        [(extract-terrain board) (extract-units board)]))

(defn unit-at [[row col] units]
    (first (filter
        (fn [[[unit-row unit-col] & _]]
            (and (= unit-row row) (= unit-col col)))
        units)))

(defn free-space? [[row col] terrain units]
    (and
        (= \. (get (get terrain row) col))
        (not (unit-at [row col] units))))

(defn adjacent-spaces [[row col] terrain units]
    (filter
        #(free-space? % terrain units)
        (list [(- row 1) col] [row (- col 1)] [row (+ col 1)] [(+ row 1) col])))

(defn adjacent-enemies [[row col] exception-unit units]
    (sort-by
        #(into [] (list (get % 2) %)) ; lowest hp
        (filter
            #(not= exception-unit (get % 1))
            (filter (fn [x] x)
                (map
                    #(unit-at % units)
                        (list
                            [(- row 1) col]
                            [row (- col 1)] [row (+ col 1)]
                            [(+ row 1) col]))))))

(defn in-range [exception-unit terrain units]
    ;; it's fine if there are some double-ups here
    (apply
        concat
        (map
            #(adjacent-spaces (get % 0) terrain units)
            (filter #(not= exception-unit (get % 1)) units))))

(defn bfs-recursive [end-coord terrain units q seen]
    ;; returns the shortest distance from start coord to end coord.
    ;; note that start coord and end coord should both be empty:
    ;; - first step is a square adjacent to moving unit
    ;; - last step is a square adjacent to target unit
    ;; (start coord can be full though)
    (let [[current-coord dist] (first q)
          q-next (rest q)]
        (cond
            (= current-coord end-coord) dist
            (empty? q) nil ; no path
            true (recur end-coord terrain units
                (concat
                    q-next
                    (map #(list % (+ dist 1))
                        (filter
                            (fn [x] (and
                                (not (contains? seen x))
                                (nil? (some #(= (first %) x) q-next))))
                            (adjacent-spaces current-coord terrain units))))
                (merge seen current-coord)))))

;; variant of the above that allows multiple targets
(defn bfs-multi [targets terrain units q seen solutions]
    (let [[current-coord dist] (first q)
          q-next (rest q)
          arrived? (some #(= current-coord %) targets)]
        (if (and (not arrived?) (empty? q)) solutions
            (recur targets terrain units
                (if (or arrived? (> (count solutions) 0))
                    (filter #(<= dist (second %)) q-next)
                    (concat
                        q-next
                        (map #(list % (+ dist 1))
                            (filter
                                (fn [x] (and
                                    (not (contains? seen x))
                                    (nil? (some #(= (first %) x) q-next))))
                                (adjacent-spaces current-coord terrain units)))))
                (merge seen current-coord)
                (if arrived? (sort (cons current-coord solutions)) solutions)))))

(defn shortest-path [start-coord end-coord terrain units]
    (bfs-recursive end-coord terrain units (list (list start-coord 0)) #{}))

(defn filter-closest [start-coord targets terrain units]
    (bfs-multi targets terrain units (list (list start-coord 0)) #{} (list)))

(defn combat-round
    ([terrain units] (combat-round terrain units 0))
    ([terrain units unit-index]
        (if (>= unit-index (count units))
            ; end of round
            [units true]
            ; mid-round
            (let [[a-coord a-sym a-hp] (nth units unit-index)
                  targets (filter #(not= a-sym (get % 1)) units)]
                (if (empty? targets)
                    ; end of combat
                    [units false]
                    ; combat continues
                    (let [dest-squares
                            (sort (filter-closest
                                a-coord
                                (in-range a-sym terrain units)
                                terrain units))
                          ;_ (println [a-coord a-sym] "in-range" (in-range a-sym terrain units))
                          ;foo (println [a-coord a-sym] "dest" dest-squares)
                          ;zzz (println (adjacent-spaces a-coord terrain units))
                          ;quz (println (doall (map #(shortest-path % (first dest-squares) terrain units) (adjacent-spaces a-coord terrain units))))
                          ;baz (println (sort-by #(into [] (list (shortest-path %
                            ;(first dest-squares) terrain units) %)) ; by dist, then lexical
                            ;(adjacent-spaces a-coord terrain units)))
                          movement-options
                            (filter
                                #(some? (shortest-path % (first dest-squares) terrain units))
                                (adjacent-spaces a-coord terrain units))
                          ;_ (println [a-coord a-sym] "mvmt" movement-options)
                          next-step
                            (if
                                (and
                                    (empty? (adjacent-enemies a-coord a-sym units))
                                    (not-empty dest-squares))
                                ; move towards target
                                (first (sort-by
                                    #(into [] (list
                                        (shortest-path
                                            % (first dest-squares) terrain units)
                                        %))
                                    movement-options))
                                a-coord)
                          ;bar (println next-step)
                          adjacent-targets
                            (adjacent-enemies next-step a-sym units)
                          next-units
                            (filter #(> (get % 2) 0)
                                (map (fn [u]
                                    (cond
                                        (and (not-empty adjacent-targets)
                                             (= u (first adjacent-targets)))
                                            [(get u 0) (get u 1) (- (get u 2) 3)]
                                        (= u [a-coord a-sym a-hp])
                                            [next-step a-sym a-hp]
                                        true
                                            u))
                                    units))]
                        (recur terrain next-units (+ 1 unit-index))))))))

(defn draw-all [terrain units]
    (for [row (range (count terrain))
          col (range (count (get terrain row)))]
        (let [u (unit-at [row col] units)]
            (print (if u (get u 1) (get (get terrain row) col)))
            (if (= (+ col 1) (count (get terrain row))) (println)))))

(defn unit-summary [units]
    (doall (map println units)))

(defn game-over? [units]
    (< (count (distinct (map #(get % 1) units))) 2))

(defn total-hp [units]
    (apply + (map #(get % 2) units)))

(defn battle-outcome [terrain units round]
    (if DEBUG
        (do
            (println "Round" round)
            (doall (draw-all terrain units))
            (unit-summary units)))
    (let [[new-units round-ended] (combat-round terrain units)]
        (if round-ended
            (recur terrain (sort new-units) (+ round 1))
            [(sort new-units) round])))

(let [[terrain units] (open-map "input15.txt")]
    (let [[final-units final-round] (battle-outcome terrain units 0)]
        (println (* final-round (total-hp final-units)))))
    ;(println (shortest-path [2 10] [2 12] terrain units))
    ;(println (shortest-path [2 5] [7 5] terrain units))
    ;(println (shortest-path [2 10] [2 15] terrain units))
    ;(println (shortest-path [2 5] [20 11] terrain units))
    ;(println (shortest-path [2 5] [30 11] terrain units))
    ;(println (filter-closest [2 5] '([2 7] [4 5] [4 6]) terrain units))
    ;(println (in-range \E terrain units))
    ;(println (filter-closest [6 3] (in-range \E terrain units) terrain units))

; 205202 is too low
; doesn't correctly figure out some of the examples
