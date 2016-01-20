
;constant grid size
(def grid-size 10)

(def world
  '#{
     ;World locations
     (isa base location) (at base t0)
     (isa river location) (at river t8) (at river t9) (at river t10) (at river t11)
     ;Actors
     (isa h1 actor) (at h1 1) (holds h1 nil)
     (isa c1 actor) (at c1 1) (holds c1 nil)
     (isa e1 actor) (at e1 1) (holds e1 nil)
     ;Resources
     (isa r1 resource) (at r1 t5) (prepared r1)
     (isa r2 resource) (at r2 t6)
     (isa r3 resource) (at r3 t7)
     })

(defn calc-adj [pos]
  (list
    (- pos 1)
    (+ pos 1)
    (+ pos grid-size)
    (- pos grid-size)
    (+ (- pos grid-size) 1)
    (- (- pos grid-size) 1)
    (+ (+ pos grid-size) 1)
    (- (+ pos grid-size) 1)
    )
  )

(defn make-tile-id [c grid]
  (let [x (first c)
        y (second c)
        c (+ (* grid y) x)]
    (symbol (str "t" c))
    )
  )

(defn gen-tile-info [x y tiles grid]
  (let [t (make-tile-id `(~x ~y) grid)]
    (concat
      (list
        `(~'isa ~t ~'tile)
        `(~'at (~x ~y) ~t))
      tiles)
    )
  )

(defn gen-grid [grid]
  (loop [y 0 tiles nil]
    (if (< y grid)
      (recur
        (inc y)
        (concat
          tiles
          (loop [x 0 tiles nil]
            (if (< x grid)
              (recur (inc x) (gen-tile-info x y tiles grid))
              tiles))))
      tiles)))

(defn gen-connections [grid tiles]
  (concat
    (map
      (fn [tile]
        (let [c (first tile)
              t1 (second tile)]
          (map (fn [coord] `(~'connects ~t1 ~(make-tile-id coord grid)))
            (coord-adjs c grid)
            )
          )
        )
      (mfor ['(at (?x ?y) ?t) tiles] (list (list (? x) (? y)) (? t))))
    tiles
    )
  )

(defn coord-adjs [c gs]
  (remove
    #(or
       (>= (first %) gs)
       (>= (second %) gs)
       (< (first %) 0)
       (< (second %) 0))
    (let [x (first c)
          y (second c)]
      (list
        `(~(inc x) ~y)
        `(~(dec x) ~y)
        `(~x ~(inc y))
        `(~x ~(dec y))
        `(~(inc x) ~(inc y))
        `(~(dec x) ~(dec y))
        `(~(inc x) ~(dec y))
        `(~(dec x) ~(inc y))
        )
      )
    )
  )

(defn update-path
  [current newp]
  {:state (:state newp),
   :cmds (concat (:cmds current) (:cmd newp)),
   :txt (concat (:txt current) (:txt newp))
   })

;Get adjacent tiles of the passed
(defn get-valid-moves [tile]
  (let [patterns (map (fn [x] `(~'isa ~x ~'tile)) (calc-adj tile))]
    (set (intersection (set patterns) (set world)))
    )
  )

(def harvester-ops
  '{search
    {:name search
     :achieves (at ?actor ?tile2) ;tile1 = currentTile tile2 = destinationTile
     :when ((at ?actor ?tile1) (:guard (not= (? tile1) (? tile2))))
     :post ()
     :pre ((contains? (get-valid-moves ?tile1) `(~'isa ~?tile2 ~'tile)))
     :del ((at ?actor ?tile1))
     :add ((at ?actor ?tile2))
     :cmd ()
     :txt (?actor moves from ?tile1 to ?tile2)
     }
    prepare
    {:name prepare
     :achieves (at ?res ?restile)
     :when ((next-to ?actor ?rawsource))
     :post ()
     :pre ((next-to ?actor ?rawsource))
     :del ((at ?rawsource ?restile))
     :add ((at ?res ?restile))
     :cmd ()
     :txt (?actor prepares a ?res at ?restile)
     }
    notify
    {:name notify
     :achieves (on ?x ?y)
     :when ((at ?x ?sx) (at ?y ?sy) (:guard (not= (? sx) (? sy))))
     :post ((protected ?sx) (protected ?sy)
             (cleartop ?x)
             (cleartop ?y)
             (hand empty))
     :pre ()
     :del ((at ?x ?sx)
            (cleartop ?y)
            (protected ?sx)
            (protected ?sy))
     :add ((at ?x ?sy)
            (on ?x ?y))
     :cmd ((pick-from ?sx)
            (drop-at ?sy))
     :txt (put ?x on ?y)

     }
    })

(def transporter-ops
  '{load
    {:name load
     :achieves (loaded ?res ?transporter) ;tile1 = currentTile tile2 = destinationTile
     :when ((next-to ?res ?transporter))
     :post ()
     :pre ((next-to ?res ?transporter)
            (empty ?transporter))
     :del ((next-to ?res ?transporter)
            (empty ?transporter))
     :add ((loaded ?res ?transporter))
     :cmd ()
     :txt (?transporter loads ?res)
     }
    prepare
    {:name prepare
     :achieves (at ?res ?restile)
     :when ((next-to ?actor ?rawsource))
     :post ()
     :pre ((next-to ?actor ?rawsource))
     :del ((at ?rawsource ?restile))
     :add ((at ?res ?restile))
     :cmd ()
     :txt (?actor prepares a ?res at ?restile)
     }
    notify
    {:name notify
     :achieves (on ?x ?y)
     :when ((at ?x ?sx) (at ?y ?sy) (:guard (not= (? sx) (? sy))))
     :post ((protected ?sx) (protected ?sy)
             (cleartop ?x)
             (cleartop ?y)
             (hand empty))
     :pre ()
     :del ((at ?x ?sx)
            (cleartop ?y)
            (protected ?sx)
            (protected ?sy))
     :add ((at ?x ?sy)
            (on ?x ?y))
     :cmd ((pick-from ?sx)
            (drop-at ?sy))
     :txt (put ?x on ?y)

     }
    })

(def engineer-ops
  '{build
    {:name build
     :achieves (on bridge ?tile) ;tile1 = currentTile tile2 = destinationTile
     :when ((on ?actor ?tile)
             (has ?actor ?resource))
     :post ()
     :pre ((on ?actor ?tile)
            (has ?actor ?resource)
            (connects ?tile ?tile1)
            (on water ?tile1)
            )
     :del ((has ?actor ?resource)
            (on water ?tile1))
     :add ((on bridge ?tile))
     :cmd ()
     :txt (?actor builds a bridge on ?tile)
     }
    prepare
    {:name prepare
     :achieves (at ?res ?restile)
     :when ((next-to ?actor ?rawsource))
     :post ()
     :pre ((next-to ?actor ?rawsource))
     :del ((at ?rawsource ?restile))
     :add ((at ?res ?restile))
     :cmd ()
     :txt (?actor prepares a ?res at ?restile)
     }
    notify
    {:name notify
     :achieves (on ?x ?y)
     :when ((at ?x ?sx) (at ?y ?sy) (:guard (not= (? sx) (? sy))))
     :post ((protected ?sx) (protected ?sy)
             (cleartop ?x)
             (cleartop ?y)
             (hand empty))
     :pre ()
     :del ((at ?x ?sx)
            (cleartop ?y)
            (protected ?sx)
            (protected ?sy))
     :add ((at ?x ?sy)
            (on ?x ?y))
     :cmd ((pick-from ?sx)
            (drop-at ?sy))
     :txt (put ?x on ?y)

     }
    })
