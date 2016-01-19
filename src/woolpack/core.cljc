(ns woolpack.core)

(def ^:const right 0)
(def ^:const down 90)
(def ^:const left 180)
(def ^:const up 270)
(def ^:const directions [right down left up])

(def ^:const catalyst
  "This is the starting point for the structure that describes the wordcloud.
   - last-rotation, rotation, x, and y are values determined during iteration n
     to compute iteration n+1 and are not used outside of the construction process.
   - x-min, x-max, y-min, y-max are recomputed with each iteration to keep track
     of the bounding box of the cloud.
   - boxes aggregates bounding box lines for the words. to be used for collision detection
     throughout the construction process. not used outside of contruction.
   - placed carries the meat of the information, the input words with x, y, and rotation
     attributes added.
   - when a word cannot be placed, it goes into unplaceable to be retried later.
   - runs keeps track of the number of times we have (re)tried placing the 
     input words."
  {:last-rotation up
   :rotation right
   :x 0 
   :y 0 
   :x-min 0 
   :x-max 0
   :y-min 0 
   :y-max 0
   :boxes []
   :placed []
   :unplaceable []
   :runs 0})

(defn box-corners
  "returns a vector with [top-left top-right bot-left bot-right] coords 
   given the top-left and bot-right corners. padding shrinks or expands the
   box about its center."
  ([xy-min xy-max] (box-corners xy-min xy-max 0))
  ([[x1 y1] [x2 y2] pad]
   (let [x1 (- x1 pad)
         y1 (- y1 pad)
         x2 (+ x2 pad)
         y2 (+ y2 pad)]
     [[x1 y1] [x2 y1] 
      [x1 y2] [x2 y2]])))

(defn d-trans
  [mag [x y] x-dir y-dir]
  [(x-dir x mag)
   (y-dir y mag)])

(defn pad-box
  [[a b
    c d]
   amt]
  (let [pad (partial d-trans amt)]
    [(pad a - -) (pad b + -)
     (pad c - +) (pad d + +)]))

(defn box-corners
  [[x1 y1] [x2 y2]]
  [[x1 y1] [x2 y1] 
   [x1 y2] [x2 y2]])

(defn box-lines
  "takes [top-left top-right bot-left bot-right] box corners and returns
   [top bot left right] line segments."
  [[a b 
    c d]]
  [[a b] [c d] [a c] [b d]])

(defn orthogonal?
  "takes two line segments, returns true if the lines are perpendicular."
  [[[_ y1] [_ y2]] 
   [[_ y3] [_ y4]]]
  (not= (= y1 y2)
        (= y3 y4)))

(defn intersecting?
  "takes two line segments, returns true if they intersect"
  [a b]
  (boolean
   (when (orthogonal? a b) 
     (let [[[x1 y1] [x2 y2]] a
           [[x3 y3] [x4 y4]] b]
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
  (condp = [last-rotation rotation]
    [right right] [x (- y height)]
    [right left] [x (+ y height)]
    [left left] [x (+ y height)]
    [left right] [x (- y height)]
    [up up] [(- x height) y]
    [up down] [(+ x height) y]
    [down down] [(+ x height) y]
    [down up] [(- x height) y]        
    [x y]))

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
                 :rotation (mod (+ new-rotation 180) 360))
          (update :placed conj placed)
          (update :boxes conj box)
          (update :x-min min x1 x2)
          (update :y-min min y1 y2)
          (update :x-max max x1 x2)
          (update :y-max max y1 y2)))
    (update cloud :unplaceable conj word)))

(defn expand-cloud
  "add to a cloud given a list of words
  contrived example: 
  > (expand-cloud {:placed []} [{:width 100 :height 10}])
  #=> {:placed [{:width 100 :height 10 :x 20 :y 70}]}"   
  ([cloud words]
   (let [{:keys [unplaceable x-max y-min y-max runs] :as greater-cloud} (reduce add-word cloud words)]
     (if (or (empty? unplaceable) (= runs 10))
       greater-cloud
       (expand-cloud
        (assoc 
         greater-cloud
         :x x-max
         :y (rand-nth (range y-min y-max)) 
         :rotation (rand-nth directions) 
         :last-rotation (rand-nth directions)
         :unplaceable []
         :runs (inc runs))
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
   (expand-cloud catalyst words)))
