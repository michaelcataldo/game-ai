;constant grid size
(def grid-size 10)

(def world
  '#{
     ;World locations
     ;(isa base location) (at base t0)
     ;(isa river location) (at river t8) (at river t9) (at river t10) (at river t11)
     ;Actors
     (isa h1 actor) (at h1 t1) (holds h1 :nil) (handles h1 unprepared)
     (isa c1 actor) (at c1 t2) (holds c1 :nil) (handles c1 prepared)
     (isa e1 actor) (at e1 t3) (holds e1 :nil) (handles e1 stored)

     (on river t8)


     ;(isa e1 actor) (at e1 base)
     ;Resources
     (isa r1 resource) (unprepared r1) (at r1 t5)
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
    ))


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

(defn world-state [gs]
  (into #{} (concat world (gen-grid gs)))
  )

(def wstate (world-state grid-size))



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

;;Get adjacent tiles of the passed
;(defn get-valid-moves [tile]
;  (let [patterns (map (fn [x] `(~'isa ~x ~'tile)) (calc-adj tile))]
;    (set (intersection (set patterns) (set world)))
;    )
;  )

(def generic-ops
  '{moveto
    {:name moveto
     :achieves (at ?actor ?t2)
     :when ((isa ?actor actor) (isa ?t1 tile) (isa ?t2 tile) (at ?actor ?t1) (:guard (not= (? t1) (? t2))) (:not (blocked ?t2)))
     ;:post (())
     :pre ((at ?actor ?t1))
     :del ((at ?actor ?t1))
     :add ((at ?actor ?t2))
     :cmd ((move-to ?t2 ?actor))
     :txt (?actor moves from ?t1 to ?t2)
     }
    pickup
    {:name pickup
     :achieves (holds ?actor ?r)
     :when ((at ?r ?t)  (handles ?actor ?rs) (:not(unprepared ?r)))
     :post ((?rs ?r) (at ?actor ?t))
     :pre ((handles ?actor ?rs))
     :del ((at ?r ?t) (holds ?actor :nil))
     :add ((holds ?actor ?r))
     :cmd ((pickup ?r ?actor ?t))
     :txt (?actor picks up ?r from ?t)
     }
    drop
    {:name drop
     :achieves (at ?r ?t)
     :when ((handles ?actor prepared)) ;;restricted to using prepared instead of ?rs multiple goals should solve
     :post ((holds ?actor ?r) (at ?actor ?t))
     :pre  ((isa ?r resource) (holds ?actor ?r) (at ?actor ?t)) ;; currently can have mutliple things on one tile
     :del ((holds ?actor ?r) )
     :add ((at ?r ?t) (holds ?actor :nil))
     :cmd ((drop ?r ?actor ?t))
     :txt (?actor drops ?r on ?t)
     }
    prepare
    {:name prepare
     :achieves (prepared ?r)
     :when ((at ?r ?t) (handles ?actor unprepared))
     :post ((at ?actor ?t))
     :pre ((isa ?actor actor) (unprepared ?r))
     :del ((unprepared ?r))
     :add ((prepared ?r))
     :cmd ((prepare ?r ?actor))
     :txt (?actor prepares ?r at ?t)
     }
    store
    {:name store
     :achieves (stored ?r)
     :when ((handles ?actor prepared))
     :post ((prepared ?r) (holds ?actor ?r) (at ?actor t1)) ;;Order of post conditions threw raise condition
     :pre ((isa ?actor actor) (prepared ?r))
     :del ((prepared ?r) (holds ?actor ?r))
     :add ((stored ?r) (holds ?actor :nil) (at ?r t1))
     :cmd ((store ?r ?actor))
     :txt (?actor stores ?r at t1)
     }
    build
    {:name build
     :achieves (on bridge ?t)
     :when ((isa ?r resource) (handles ?actor stored) (on river ?t))
     :post ((stored ?r)(holds ?actor ?r)(at ?actor ?t))
     :pre  ((isa ?r resource) (holds ?actor ?r) (at ?actor ?t))
     :del ((holds ?actor ?r) (on river ?t) (stored ?r) (isa ?r resource))
     :add ((on bridge ?t) (holds ?actor :nil))
     :cmd ((build-bridge ?actor ?t))
     :txt (?actor builds a bridge at ?t)
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

;================================
; Netlogo comms & filters
;================================

; wrap x in quotes
(defn str-qt [x]
  (str " \"" x "\" ")
  )

(let [
       sp    " "
       qt    "\""
       ]


  (defmatch nlogo-translate-cmd []
    ((create-tile ?id ?x ?y) :=> (str 'tile.create (str-qt (? id)) (str-qt (? x)) ((? y))))
    ((create-resource ?id ?t) :=> (str 'resource-create (str-qt (? id))(str-qt (? t))))
    ((create-agent ?id ?t ?atype) :=> (str 'agent.create (str-qt (? id))(str-qt (? t)) ((? atype))))
    ((move-to ?t ?a)   :=> (str 'agent.move-to  (str-qt (? t))(str-qt (? a))))
    ((pickup ?r ?a ?t)   :=> (str 'agent.pickup (str-qt (? r)) (str-qt (? a))(str-qt(? t))))
    ((drop ?r ?a ?t)   :=> (str 'agent.drop sp  (str-qt(? r)) (str-qt(? a) (str-qt(? t)))))
    ((prepare ?r ?a)   :=> (str 'agent.prepare  (str-qt(? r)) (str-qt(? a))))
    ((store ?r ?a)   :=> (str 'agent.store (str-qt (? r)) (str-qt(? a))))
    ((build-bridge ?a ?t)   :=> (str 'agent.build (str-qt (? a)) (str-qt (? t))))
    ( ?_            :=> (ui-out :dbg 'ERROR '(unknown NetLogo cmd)))
    )
  )

;(defn get-tile-coord [world tile]
;  ;(at (?x ?y) ~?tile)
;  (mfind [`(~'at ~'(?x ?y) ~tile) world] (str (str-qt (? x)) (str-qt (? y))))
;  )

(defn setup-world [world-size]
  (nlogo-send (str "world.set-size "  world-size))
  (nlogo-send (str "world.create-agent" (str-qt 'H1) (str-qt 'T52) (str-qt 'H)))
  (nlogo-send (str "world.create-agent" (str-qt 'H2) (str-qt 'T53) (str-qt 'H)))
  )