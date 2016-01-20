;constant grid size
(def grid-size 4)
(def world
  '(
     ;World Coordinates
     (isa 0 tile) (isa 1 tile) (isa 2 tile) (isa 3 tile)
     (isa 4 tile) (isa 5 tile) (isa 6 tile) (isa 7 tile)
     (isa 8 tile) (isa 9 tile) (isa 10 tile) (isa 11 tile)
     (isa 12 tile) (isa 13 tile) (isa 14 tile) (isa 15 tile)

     ;World locations
     (isa base location) (at base t0)
     (isa river location) (at river t8) (at river t9) (at river t10) (at river t11)
     ;Actors
     (isa h1 actor) (at h1 t1) (holds h1 nil)
     (isa c1 actor) (at c1 base) (holds c1 nil)
     (isa e1 actor) (at e1 base) (holds e1 nil)
     ;Resources
     (isa r1 resource) (at r1 t5) (prepared r1)
     (isa r2 resource) (at r2 t6)
     (isa r3 resource) (at r3 t7)
     ))

  (defn grid-tester [grid]
    (loop
      [x (* grid grid)
       tiles nil
       i 0]
      (if (<= i x)
        (recur x (cons `(~'isa ~i ~'tile) tiles) (inc i))
      tiles))
    )

(defn update-path
  [current newp]
  { :state (:state newp),
    :cmds  (concat (:cmds current) (:cmd newp)),
    :txt   (concat (:txt current) (:txt newp))
    })

;Get adjacent tiles of the passed
(defn get-valid-moves [tile]
  (let [patterns ( map (fn [x] `(~'isa ~x ~'tile)) (calc-adj tile))]
    (vec (intersection (set patterns) (set world)))
    )
  )

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

