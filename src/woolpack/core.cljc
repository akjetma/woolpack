(ns woolpack.core)

(def ^:const right 0)
(def ^:const down 90)
(def ^:const left 180)
(def ^:const up 270)

(defn abs
  [n]
  (if (neg? n)
    (* n -1)
    n))

(defn d-trans
  "translate a point diagonally by some magnitude in some quadrant"
  [mag [x y] x-dir y-dir]
  [(x-dir x mag)
   (y-dir y mag)])

(defn pad-box
  "expand or contract a box's points"
  [[a b
    c d]
   amt]
  (let [pad (partial d-trans amt)]
    [(pad a - -) (pad b + -)
     (pad c - +) (pad d + +)]))

(defn box-corners
  "given two points, return four points defining box"
  [[x1 y1] [x2 y2]]
  [[x1 y1] [x2 y1] 
   [x1 y2] [x2 y2]])

(defn box-lines
  "takes [top-left top-right bot-left bot-right] box corners and returns
   [top bot left right] line segments."
  [[a b 
    c d]]
  [[a b] [c d] [a c] [b d]])

(defn intersecting?
  "takes two line segments, returns true if they intersect"
  [a b]
  (boolean
   (let [[[x1 y1] [x2 y2]] a
         [[x3 y3] [x4 y4]] b]
     (when-not (= (= y1 y2) (= y3 y4))
       (or (and (or (<= x1 x3 x2)
                    (>= x1 x3 x2))
                (or (<= y3 y1 y4)
                    (>= y3 y1 y4)))
           (and (or (<= x3 x1 x4)
                    (>= x3 x1 x4))
                (or (<= y1 y3 y2)
                    (>= y1 y3 y2))))))))

(defn pierces?
  "takes two corner-boxes and finds out if any of their line segments intersect."
  [i j]
  (let [[iab icd iac ibd] (box-lines i)
        [jab jcd jac jbd] (box-lines j)]
    (or (intersecting? iab jac) (intersecting? iab jbd)
        (intersecting? icd jac) (intersecting? icd jbd)
        (intersecting? iac jab) (intersecting? iac jcd)
        (intersecting? ibd jab) (intersecting? ibd jcd))))

(defn engulfs?
  "tests whether one corner-box entirely encompasses another."
  [[ia ib
    ic id]
   [ja jb
    jc jd]]
  (let [compare-points (fn [[x1 y1] [x2 y2] x-comp y-comp]
                         (and (x-comp x1 x2) (y-comp y1 y2)))]
    (and (compare-points ia ja < <)
         (compare-points ib jb > <)
         (compare-points ic jc < >)
         (compare-points id jd > >))))

(defn collide?
  "tests whether two corner-boxes overlap in any way."
  [i j]
  (or
   (engulfs? i j) 
   (engulfs? j i)
   (pierces? i j)))

(defn start
  "given the ending coordinate and rotation of the previous 
  shape, find where the next shape should start based on its
  rotation."
  [{:keys [x y last-rotation]} {:keys [height]} rotation]
  (if (= 90 (abs (- rotation last-rotation)))
    [x y]
    (condp = rotation
      right [x (- y height)]
      left [x (+ y height)]
      up [(- x height) y]
      down [(+ x height) y])))

(defn end
  "returns the coordinate that describes the opposite corner of
  a box of width and height rotated about the starting coordinate."
  [x y {:keys [width height]} rotation]
  (condp = rotation
    right [(+ x width) (+ y height)]
    down [(- x height) (+ y width)]
    left [(- x width) (- y height)]
    up [(+ x height) (- y width)]))

(defn find-place
  "tries to find a valid placement for a rectangle given the current cloud state."
  ([cloud word] (find-place cloud word (:rotation cloud) 0))
  ([cloud word rotation tries]
   (let [[x1 y1] (start cloud word rotation)
         [x2 y2] (end x1 y1 word rotation)
         [x-min x-max] (sort [x1 x2])
         [y-min y-max] (sort [y1 y2])
         box (box-corners [x-min y-min] [x-max y-max])
         mini (pad-box box -1)
         success? (not-any? (partial collide? mini) (:boxes cloud))
         exhausted? (= 5 tries)]
     (cond 
       success? [x1 y1 x2 y2 rotation box]       
       exhausted? nil
       :else 
       (find-place 
        cloud 
        word 
        (mod (- rotation 90) 360) 
        (inc tries))))))

(defn add-word
  "if a word can be placed into the wordcloud, add it to the cloud
  otherwise skip it and add it to a list of rectangles that couldn't
  be placed. (to be retried later)"
  [cloud word]
  (if-let [placement (find-place cloud word)]
    (let [[x1 y1 x2 y2 new-rotation box] placement
          placed (assoc word :x x1 :y y1 :rotation new-rotation)]
      (-> cloud
          (assoc :x x2
                 :y y2                  
                 :last-rotation new-rotation
                 :rotation (mod (+ new-rotation 90) 360))
          (update :placed conj placed)
          (update :boxes conj box)
          (update :x-min min x1 x2)
          (update :y-min min y1 y2)
          (update :x-max max x1 x2)
          (update :y-max max y1 y2)))
    (update cloud :unplaceable conj word)))

(defn random-start
  [{:keys [x-min y-min x-max y-max]}]
  (rand-nth
   [{:x x-min :y y-min :rotation right :last-rotation left}
    {:x x-max :y y-min :rotation down :last-rotation up}
    {:x x-max :y y-max :rotation left :last-rotation right}
    {:x x-min :y y-min :rotation up :last-rotation down}]))

(defn expand-cloud
  "add to a cloud given a list of words
  contrived example: 
  > (expand-cloud [{:width 100 :height 10}])
  #=> {:placed [{:width 100 :height 10 :x 20 :y 70}]}"
  ([words] 
   (expand-cloud
    {:x-min 0 :x-max 0 :y-min 0 :y-max 0
     :boxes [] :placed [] :unplaceable []
     :runs 0}
    words))
  ([cloud words]
   (let [{:keys [unplaceable runs] :as greater-cloud}
         (reduce 
          add-word 
          (merge cloud (random-start cloud)) 
          words)]
     (if (or (empty? unplaceable) (= runs 10))
       greater-cloud
       (expand-cloud
        (assoc greater-cloud :unplaceable [] :runs (inc runs))
        (shuffle unplaceable))))))
 
(defn fit-cloud
  "adjust the locations of the words in the cloud and 
  the boundaries of the cloud so that it starts at 0 0"
  [{:keys [x-min x-max y-min y-max placed] :as cloud}]
  (assoc cloud 
         :x-max (- x-max x-min)
         :y-max (- y-max y-min)
         :placed (map 
                  #(-> % (update :x - x-min) (update :y - y-min))
                  placed)))

(defn billow
  [words]
  (fit-cloud
   (expand-cloud words)))
