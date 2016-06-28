(ns structured-data)

(defn
  do-a-thing
  [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn
  spiff
  [v]
  (+ (get v 0) (get v 2)))

(defn
  cutify
  [v]
  (conj  v "<3"))


(defn
  spiff-destructuring
  [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn
  height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))


(defn
  divides?
  [div n]
  (if (= (mod n div) 0)
    true
    false))

(defn
  square?
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (divides? (+ y1 y2) (+ x1 x2))
      true
      false)))

(defn
  area
  [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
      (* (- x1 x2) (- y1 y2))))

(defn
  contains-point?
  [rectangle point]
  (let [
         [[x1 y1] [x2 y2]] rectangle
         [Px Py] point]
    (if
      (and (<= x1 Px x2) (<= y1 Py y2))
      true
      false))) ;don't need to explicitly state these conditions.

(defn
  contains-rectangle?
  [outer inner]
  (let [
         [[x1O y1O] [x2O y2O]] outer
         [[x1I y1I] [x2I y2I]] inner ]
         (if           ;could have used contains-point?
           (and
             (and (<= x1O x1I x2O) (<= x1O x2I x2O))
             (and (<= y1O y1I y2O) (<= y1O y2I y2O)))
           true
           false))) ;don't need to explicitly state these conditions.

(defn
  title-length
  [book]
  (count
    (:title book)))

(defn
  author-count
  [book]
  (count
    (:authors book)))


(defn
  multiple-authors?
  [book]
  (if
    (> (count (:authors book)) 1)
    true
    false))

(defn
  add-author
  [book new-author]
  (assoc book
    :authors
    (conj
      (:authors book) new-author)))

(defn
  alive?
  [author]
  (not
    (contains?
      author :death-year)))


(defn
  element-lengths
  [coll]
  (map
    count coll))

(defn
  second-elements
  [coll]
  (map
    (fn [x] (get x 1))
    coll))


(defn
  titles
  [books]
  (map
    (:title
      books)))


(defn
  stars
  [n]
  (apply
    str
    (repeat
      n
      \*)))

(defn
  monotonic?
  [a-seq]
  (or
    (apply
      <= a-seq)
    (apply
      >= a-seq)))


(defn
  toggle
  [a-set elem]
  (if
    (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))


(defn
  contains-duplicates?
  [a-seq]
  (if
    (>
      (count a-seq)
      (count (set a-seq)))
    true
    false))

(defn
  old-book->new-book
  [book]
  (assoc
     book
     :authors
     (set
       (:authors
         book))))

(defn
  has-author?
  [book author]
  (contains?
    (:authors book)
    author))

(defn
  authors
  [books]
  (apply
    clojure.set/union
    (map :authors books))) ; need the 'map' to apply to the collection of book(s)


(defn
  all-author-names
  [books]
  (set
    (map
      :name
      (authors books))))


(defn
  author->string
  [author]
  (cond
    (:birth-year author) (str (:name author) " (" (:birth-year  author) " - " (:death-year author) ")")
     :else (str (:name author))))


(defn
  authors->string
  [authors]
  (apply ;to the item of the list below
    str
    (interpose ", " ;returns a list so need to use 'apply' above
               (map
                 author->string
                 authors))))


(defn
  book->string
  [book]
   (apply str (:title book) ", written by " (authors->string (:authors book))))



(defn
  books->string
  [books]
  (cond
    (== (count books) 1)
      (str "1 book. " (apply str (map book->string books))) ; why can't I just str the whole sub-function?
    (> (count books) 1)
      (str (count books) " books." (apply str (interpose ". "(map book->string books))))
    :else "No books."))

(defn
  books-by-author
  [author books]
  (filter
    (fn
      [boook] ;notice that after eval, the last value is the last item in the 'books' vector. filter's 2nd arg is books, and it is traversed entirely
      (has-author? book author))
    books)); notice the value here after eval is the only one which evaluates true to the internal function (1st arg to 'filter')



(defn
  author-by-name
  [name authors]
    (first
      (filter
        (fn
          [author]
          (= (:name author) name))
        authors)))


(defn
  living-authors
  [authors]
    (filter
      alive?
      authors))


(defn
  has-a-living-author?
  [book]
   (not
     (empty?
     (living-authors (:authors book)))))


(defn
  books-by-living-authors
  [books]
    (filter
      has-a-living-author?
      books))


; %________%
