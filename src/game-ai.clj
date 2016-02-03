(require '[cgsx.tools.matcher :refer :all])

;constant grid size
(def grid-size 16)

(def world
  '#{
     ;World locations
     (isa b1 base) (at b1 t130)
     ;(isa river location) (at river t8) (at river t9) (at river t10) (at river t11)
     ;Actors
     (isa h1 actor) (at h1 t56) (holds h1 :nil) (handles h1 unprepared)
     (isa c1 actor) (at c1 t55) (holds c1 :nil) (handles c1 prepared)
     (isa e1 actor) (at e1 t57) (holds e1 :nil) (handles e1 stored)

     (isa l3 lake)  (at l3 t12)
     (isa l4 lake)  (at l4 t28)
     (isa l5 lake)  (at l5 t44)
     (isa l6 lake)  (at l6 t60)
     (isa l7 lake)  (at l7 t76)
     (isa l8 lake)  (at l8 t92)
     (isa l9 lake)  (at l9 t108)
     (isa l10 lake)  (at l10 t124)
     (isa l11 lake)  (at l11 t140)
     (isa l12 lake)  (at l12 t156)
     (isa l13 lake)  (at l13 t172)
     (isa l14 lake)  (at l14 t188)
     (isa l15 lake)  (at l15 t204)
     (isa l16 lake)  (at l16 t220)
     (isa l17 lake)  (at l17 t236)
     (isa l18 lake)  (at l18 t252)
     (isa l19 lake)  (at l19 t268)

     (isa l20 lake)  (at l20 t13)
     (isa l21 lake)  (at l21 t29)
     (isa l22 lake)  (at l22 t45)
     (isa l23 lake)  (at l23 t61)
     (isa l24 lake)  (at l24 t77)
     (isa l25 lake)  (at l25 t93)
     (isa l26 lake)  (at l26 t109)
     (isa l27  lake)  (at l27 t125)
     (isa l28  lake)  (at l28 t141)
     (isa l29  lake)  (at l29 t157)
     (isa l30  lake)  (at l30 t173)
     (isa l31  lake)  (at l31 t189)
     (isa l32  lake)  (at l32 t205)
     (isa l33  lake)  (at l33 t221)
     (isa l34  lake)  (at l34 t237)
     (isa l35  lake)  (at l35 t253)
     (isa l36  lake)  (at l36 t269)


;     (isa l37 lake)  (at l37 t139)
;     (isa l38 lake)  (at l38 t138)
;     (isa l39 lake)  (at l39 t137)
;     (isa l40 lake)  (at l40 t136)
;     (isa l41 lake)  (at l41 t135)
;     (isa l42 lake)  (at l42 t134)
;     (isa l43 lake)  (at l43 t133)
;     (isa l44 lake)  (at l44 t132)



     ;(isa e1 actor) (at e1 base)
     ;Resources
     (isa r1 resource) (at r1 t214) (unprepared r1)
     (isa r2 resource) (at r2 t82) (unprepared r2)
     (isa r3 resource) (at r3 t40) (unprepared r3)

     ;(blocked t10)
     }
  )

(defn get-actors []

  )

;((create-tile ?id ?x ?y) :=>
;((create-resource ?id ?t) :=>
;((create-base ?id ?t) :=> (st
;((create-agent ?id ?t ?atype)
;                                  )

(defn nlogo-translate-wstate [wstate]
  ;;actor
  (mfor ['[at ?actor ?loc] wstate] '(create-agent (? actor) (? loc) (first (? actor))))
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
     :when ((handles ?actor prepared) (isa ?base base) (at ?base ?t))
     :post ((prepared ?r) (holds ?actor ?r) (at ?actor ?t)) ;;Order of post conditions threw raise condition
     :pre ((isa ?actor actor) (prepared ?r))
     :del ((prepared ?r) (holds ?actor ?r))
     :add ((stored ?r) (holds ?actor :nil) (at ?r ?t))
     :cmd ((store ?r ?actor ?t))
     :txt (?actor stores ?r at ?t)
     }
     ; (isa l1 lake)  (at l1 t34)
    build
    {:name build
     :achieves (on bridge ?t)
     :when ((isa ?r resource) (handles ?actor stored) (isa ?l lake) (at ?l ?t))
     :post ((stored ?r)(holds ?actor ?r)(at ?actor ?t))
     :pre  ((isa ?r resource) (holds ?actor ?r) (at ?actor ?t))
     :del ((holds ?actor ?r) (isa ?l lake) (at ?l ?t) (stored ?r) (isa ?r resource))
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

  (defmatch nlogo-translate-setup []
    ;((create-tile ?id ?x ?y) :=> (str 'tile.create (str-qt (? id)) (str-qt (? x)) ((? y))))
    ;((create-resource ?id ?t) :=> (str 'world.create-resource (str-qt (? id))(str-qt (? t))))
    ((?t ?a actor) :=> (str 'world.create-agent (str-qt (? a)) (str-qt (? t)) (str-qt (first (str (? a))))))
    ((?t ?r resource) :=> (str 'world.create-resource (str-qt (? r)) (str-qt (? t))))
    ((?t ?l lake) :=> (str 'world.create-lake (str-qt (? l)) (str-qt (? t))  ))
    ((?t ?b base) :=> (str 'world.create-base (str-qt (? b)) (str-qt (? t))  ))
    ( ?_            :=> (ui-out :dbg 'ERROR '(unknown NetLogo cmd)))
    )

  (defmatch nlogo-translate-cmd []
    ((move-to ?t ?a)   :=> (str 'agent.move-to  (str-qt (? t))(str-qt (? a))))
    ((pickup ?r ?a ?t)   :=> (str 'agent.pickup (str-qt (? r)) (str-qt (? a))))
    ((drop ?r ?a ?t)   :=> (str 'agent.drop sp  (str-qt(? r)) (str-qt(? a) (str-qt(? t)))))
    ((prepare ?r ?a)   :=> (str 'agent.prepare  (str-qt(? r)) (str-qt(? a))))
    ((store ?r ?a ?t)   :=> (str 'agent.store (str-qt (? r)) (str-qt(? a)) (str-qt(? t))))
    ((build-bridge ?a ?t)   :=> (str 'agent.build (str-qt (? a)) (str-qt (? t))))
    ( ?_            :=> (ui-out :dbg 'ERROR '(unknown NetLogo cmd)))
    )
  )

(defn nlogo-send-setup [cmd]
  (let [cmd-str (nlogo-translate-setup (into '() cmd))]
    ;(ui-out :comm 'NL==> cmd-list "   \t" cmd-str)
    ; (ui-out :comm "     " cmd-str)
    ;(println  cmd-str)
    (nlogo-send cmd-str)
    ))

(defn nlogo-send-setups [cmd-list]
  (dotimes [c (count cmd-list)]
    (nlogo-send-setup (nth cmd-list c))
    )
  )

(defn nlogo-run [goal]
  (nlogo-send "setup")
  (nlogo-send (str "world.set-size "  grid-size))

  ;(print "sending setup commands")
  (nlogo-send-setups (mfor* ['([isa ?a ?ent] [at ?a ?l]) world] [(? ent) (? a) (? l)]))

  ;(print "sending planner commands")
  (nlogo-send-cmds (:cmds (planner wstate goal generic-ops)))
  )

;(defn coord-adjs [c gs]
;  (remove
;    #(or
;       (>= (first %) gs)
;       (>= (second %) gs)
;       (< (first %) 0)
;       (< (second %) 0))
;    (let [x (first c)
;          y (second c)]
;      (list
;        `(~(inc x) ~y)
;        `(~(dec x) ~y)
;        `(~x ~(inc y))
;        `(~x ~(dec y))
;        `(~(inc x) ~(inc y))
;        `(~(dec x) ~(dec y))
;        `(~(inc x) ~(dec y))
;        `(~(dec x) ~(inc y))
;        )
;      )
;    )
;  )

;(defn gen-connections [grid tiles]
;  (concat
;    (map
;      (fn [tile]
;        (let [c (first tile)
;              t1 (second tile)]
;          (map (fn [coord] `(~'connects ~t1 ~(make-tile-id coord grid)))
;            (coord-adjs c grid)
;            )
;          )
;        )
;      (mfor ['(at (?x ?y) ?t) tiles] (list (list (? x) (? y)) (? t))))
;    tiles
;    )
;  )

;;Get adjacent tiles of the passed
;(defn get-valid-moves [tile]
;  (let [patterns (map (fn [x] `(~'isa ~x ~'tile)) (calc-adj tile))]
;    (set (intersection (set patterns) (set world)))
;    )
;  )


;(defn get-tile-coord [world tile]
;  ;(at (?x ?y) ~?tile)
;  (mfind [`(~'at ~'(?x ?y) ~tile) world] (str (str-qt (? x)) (str-qt (? y))))
;  )


;(defn setup-world [world-size]
;  (nlogo-send "setup")
;
;  (nlogo-send (str "world.set-size "  world-size))
;
;  (nlogo-send (str "world.create-base" (str-qt 'base) (str-qt 't1)))
;
;  (nlogo-send (str "world.create-river" (str-qt 'T34)))
;
;  (nlogo-send (str "world.create-agent" (str-qt 'h1) (str-qt 't2) (str-qt 'H)))
;  (nlogo-send (str "world.create-agent" (str-qt 'c1) (str-qt 't3) (str-qt 'C)))
;  (nlogo-send (str "world.create-agent" (str-qt 'e1) (str-qt 't4) (str-qt 'E)))
;
;  (nlogo-send (str "world.create-resource" (str-qt 'r1) (str-qt 't55)))
;  (nlogo-send (str "world.create-resource" (str-qt 'r2) (str-qt 't71)))
;  (nlogo-send (str "world.create-resource" (str-qt 'r3) (str-qt 't19)))
;  )

