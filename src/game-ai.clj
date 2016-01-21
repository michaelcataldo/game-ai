;constant grid size
(def grid-size 10)

(def world
  '#{
     ;World locations
     ;(isa base location) (at base t0)
     ;(isa river location) (at river t8) (at river t9) (at river t10) (at river t11)
     ;Actors
     (isa h1 actor) (at h1 t1) (holds h1 :nil) (handles :nil h1)
     (isa c1 actor) (at c1 t1) (handles prepared c1)
     ;(isa e1 actor) (at e1 base)
     ;Resources
     (isa r1 resource) (unprepared r1) (at r1 t2)
     ;     (isa r2 resource) (at r2 t2) (prepared r2)
     ;     (isa r3 resource) (at r3 t2) (prepared r3)

     ;(blocked t10)
     }
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
        `(~'isa ~t ~'tile))
      ;`(~'at (~x ~y) ~t))
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

;Get adjacent tiles of the passed
(defn get-valid-moves [tile]
  (let [patterns (map (fn [x] `(~'isa ~x ~'tile)) (calc-adj tile))]
    (set (intersection (set patterns) (set world)))
    )
  )

(def generic-ops
  '{moveto
    {:name moveto
     :achieves (at ?actor ?t2)
     :when ((isa ?actor actor) (isa ?t1 tile) (isa ?t2 tile) (at ?actor ?t1) (:guard (not= (? t1) (? t2))) (:not (blocked ?t2)))
     ;:post (())
     :pre ((at ?actor ?t1))
     :del ((at ?actor ?t1))
     :add ((at ?actor ?t2))
     ;:cmd (())
     :txt (?actor moves from ?t1 to ?t2)
     }
    pickup
    {:name pickup
     :achieves (holds ?actor ?r)
     :when ((isa ?actor actor)(at ?r ?t) (handles ?rs ?actor) (:not (handles :nil ?actor)))
     :post ((holds ?actor :nil) (at ?actor ?t))
     :pre ((isa ?actor actor) (isa ?r resource) (at ?r ?t)  (?rs ?r))
     :del ((at ?r ?t) (holds ?actor :nil))
     :add ((holds ?actor ?r))
     ;:cmd (())
     :txt (?actor picks up ?r from ?t)
     }
    drop
    {:name drop
     :achieves (at ?r ?t)
     :when ((isa ?actor actor))
     :post ((holds ?actor ?r) (at ?actor ?t))
     :pre ( (isa ?r resource)(holds ?actor ?r) (at ?actor ?t)) ;; currently can have mutliple things on one tile
     :del ((holds ?actor ?r))
     :add ((at ?r ?t) (holds ?actor :nil))
     ;:cmd (())
     :txt (?actor drops ?r on ?t)
     }

    prepare
    {:name prepare
     :achieves (prepared ?r)
     :when ((at ?r ?t))
     :post ((at ?actor ?t))
     :pre ((isa ?actor actor) (unprepared ?r))
     :del ((unprepared ?r))
     :add ((prepared ?r))
     :cmd ()
     :txt (?actor prepares ?r at ?t)
     }


    }
  )

(def harvester-ops
  '{prepare
    {:name prepare
     :achieves (prepared ?r)
     :when ((at ?r ?t))
     :post ((at ?actor ?t))
     :pre ((isa ?actor actor) (unprepared ?r))
     :del ((unprepared ?r))
     :add ((prepared ?r))
     :cmd ()
     :txt (?actor prepares ?r at ?t)
     }
    })

(def collector-ops
  '{})

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

