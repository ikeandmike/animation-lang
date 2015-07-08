;; Michael Giancola - Individual Project Design

(require "world-cs1102.rkt")

;;;;;;;;;;;;;;; THE GLOBALS ;;;;;;;;;;;;;;;

(define WIDTH 800)
(define HEIGHT 500)
(define IMGS empty)

;;;;;;;;;;;;;;; THE DESIGN ;;;;;;;;;;;;;;;

;; a canvas is (make-canvas list[cmd])
(define-struct canvas (cmds))

;; a cmd is either
;; - (make-display-animation animation) or
;; - (make-repeat-until collision list[cmd]) or
;; - (make-collision symbol symbol) or
;; - (make-at-edge symbol symbol) or
;; - (make-move-cmd symbol) or
;; - (make-jump-cmd symbol posn) or
;; - (make-random-jump-cmd symbol) or
;; - (make-delete-cmd symbol) or
;; - (make-change-vel symbol posn)

;; a display-animation is (make-display-animation animation)
(define-struct display-animation (ani))

;; a animation is either
;; - (make-a-circle symbol string number posn posn) or
;; - (make-a-rect symbol string number number posn posn)

;; a circle is (make-circle symbol string number posn posn)
(define-struct a-circle (name color radius loc speed))

;; a rectangle is (make-rect symbol string number number posn posn)
(define-struct a-rect (name color x-len y-len loc speed))

;; a repeat-until is (make-repeat-until collision list[cmd]) 
(define-struct repeat-until (end-event actions))

;; a collision is (make-collision symbol symbol)
(define-struct collision (ani1-name ani2-name))

;; a at-edge is (make-at-edge symbol symbol)
;; Allowed symbols for edge are 'top, 'bottom, 'left, 'right
(define-struct at-edge (ani-name edge))

;; a move-cmd is (make-move-cmd symbol)
(define-struct move-cmd (ani-name))

;; a jump-cmd is (make-jump-cmd symbol posn)
(define-struct jump-cmd (ani-name new-loc))

;; a random-jump-cmd is (make-random-jump-cmd symbol)
(define-struct random-jump-cmd (ani-name))

;; a delete-cmd is (make-delete-cmd symbol)
(define-struct delete-cmd (ani-name))

;; a change-vel is (make-change-vel symbol posn)
(define-struct change-vel (ani-name new-vel))

;;;;;;;;;;;;;;; THE EXAMPLE DATA - NON-MACRO VERSION ;;;;;;;;;;;;;;;

(define ANI-1
  (make-canvas
   (list
    (make-display-animation (make-a-circle 
                             'c1 
                             "red" 
                             50
                             (make-posn 100 100)
                             (make-posn 40 15)))
    (make-display-animation (make-a-rect
                             'r1
                             "blue"
                             75
                             400
                             (make-posn 700 250)
                             (make-posn 0 0)))
    (make-repeat-until (make-collision 'c1 'r1)
                       (list (make-move-cmd 'c1)))
    (make-delete-cmd 'r1)
    (make-change-vel 'c1 (make-posn -40 15))
    (make-repeat-until (make-at-edge 'c1 'left)
                       (list (make-move-cmd 'c1))))))

(define ANI-2
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                         "purple"
                                         50
                                         (make-posn 700 300)
                                         (make-posn 0 0)))
    (make-repeat-until (make-at-edge 'c1 'top)
                       (list (make-random-jump-cmd 'c1))))))

(define ANI-3
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                         "orange"
                                         50
                                         (make-posn 150 100)
                                         (make-posn 0 20)))
    (make-display-animation (make-a-rect 'green-rect
                                         "green"
                                         600
                                         50
                                         (make-posn 400 450)
                                         (make-posn 0 0)))
    (make-repeat-until (make-collision 'c1 'green-rect)
                       (list (make-move-cmd 'c1)))
    (make-display-animation (make-a-rect 'red-rect
                                         "red"
                                         50
                                         350
                                         (make-posn 700 200)
                                         (make-posn 0 0)))
    (make-change-vel 'c1 (make-posn 20 0))
    (make-repeat-until (make-collision 'c1 'red-rect)
                       (list (make-move-cmd 'c1)))
    (make-random-jump-cmd 'c1))))

;; My animation shows two circles moving towards each other. At the point when
;; they collide, the larger one continues moving in the same direction, while
;; the smaller one is removed from the scene
(define MY-ANIMATION
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'big-c
                                            "yellow"
                                            100
                                            (make-posn 200 300)
                                            (make-posn 20 0)))
    (make-display-animation (make-a-circle 'small-c
                                            "blue"
                                            50
                                            (make-posn 600 300)
                                            (make-posn -20 0)))
    (make-repeat-until (make-collision 'big-c 'small-c)
                       (list (make-move-cmd 'big-c)
                             (make-move-cmd 'small-c)))
    (make-delete-cmd 'small-c)
    (make-repeat-until (make-at-edge 'big-c 'right)
                       (list (make-move-cmd 'big-c))))))

;;;;;;;;;;;;;;; THE MACROS ;;;;;;;;;;;;;;;

(define-syntax canvas
  (syntax-rules ()
    [(canvas cmd ...)
     (make-canvas
      (list cmd ...))]))

(define-syntax display
  (syntax-rules ()
    [(display ani)
     (make-display-animation ani)]))

(define-syntax a-circle
  (syntax-rules ()
    [(a-circle name color rad loc vel)
     (make-a-circle name color rad loc vel)]))

(define-syntax a-rect
  (syntax-rules ()
    [(a-rect name color x-len y-len loc vel)
     (make-a-rect name color x-len y-len loc vel)]))

(define-syntax repeat
  (syntax-rules ()
    [(repeat event action ...)
     (make-repeat-until event (list action ...))]))

(define-syntax collision
  (syntax-rules ()
    [(collision name1 name2)
     (make-collision name1 name2)]))

(define-syntax at-edge
  (syntax-rules ()
    [(at-edge name a-edge)
     (make-at-edge name a-edge)]))

(define-syntax random-jump
  (syntax-rules ()
    [(random-jump name)
     (make-random-jump-cmd name)]))

(define-syntax move
  (syntax-rules ()
    [(move name)
     (make-move-cmd name)]))

(define-syntax delete
  (syntax-rules ()
    [(delete name)
     (make-delete-cmd name)]))

(define-syntax change-vel
  (syntax-rules ()
    [(change-vel name vel)
     (make-change-vel name vel)]))

(define-syntax posn
  (syntax-rules ()
    [(posn num1 num2)
     (make-posn num1 num2)]))

;;;;;;;;;;;;;;; THE EXAMPLE DATA - MACRO VERSION ;;;;;;;;;;;;;;;

(define M-ANI-1
  (canvas
   (display (a-circle
             'c1
             "red"
             50
             (posn 100 100)
             (posn 40 15)))
   (display (a-rect
             'r1
             "blue"
             75
             400
             (posn 700 250)
             (posn 0 0)))
   (repeat (collision 'c1 'r1)
           (move 'c1))
   (delete 'r1)
   (change-vel 'c1 (posn -40 15))
   (repeat (at-edge 'c1 'left)
           (move 'c1))))

(define M-ANI-2
  (canvas
   (display (a-circle 'c1
                      "purple"
                      50
                      (posn 700 300)
                      (posn 0 0)))
   (repeat (at-edge 'c1 'top)
           (random-jump 'c1))))

(define M-ANI-3
  (canvas
   (display (a-circle 'c1
                      "orange"
                      50
                      (posn 150 100)
                      (posn 0 20)))
   (display (a-rect 'green-rect
                    "green"
                    600
                    50
                    (posn 400 450)
                    (posn 0 0)))
   (repeat (collision 'c1 'green-rect)
           (move 'c1))
   (display (a-rect 'red-rect
                    "red"
                    50
                    350
                    (posn 700 200)
                    (posn 0 0)))
   (change-vel 'c1 (posn 20 0))
   (repeat (collision 'c1 'red-rect)
           (move 'c1))
   (random-jump 'c1)))

;;;;;;;;;;;;;;; THE INTERPRETER ;;;;;;;;;;;;;;;

;; run-canvas : canvas -> scene
;; produces a scene based on the given canvas
(define (run-canvas a-canvas)
  (begin
    (big-bang WIDTH HEIGHT (/ 1 28) true)
    (run-cmdlist (canvas-cmds a-canvas))
    (set! IMGS empty))) ;;resets the global for next canvas

;; run-cmdlist : list[cmd] -> scene
;; loops through each command. for each, it runs it, 
;; draws the updated scene, and sleeps for 0.15 seconds
(define (run-cmdlist cmds)
  (for-each (lambda (a-cmd) (begin
                              (run-cmd a-cmd)
                              (draw-canvas IMGS)
                              (sleep/yield 0.15))) ;;used 0.15 to make the animations look a bit smoother
            cmds))

;; run-cmd : cmd -> void (or boolean for the collision cmd)
;; executes an individual command
(define (run-cmd a-cmd)
  (cond [(display-animation? a-cmd) (show-animation a-cmd)]
        [(repeat-until? a-cmd) (repeat-func a-cmd)]
        [(collision? a-cmd) (collided? a-cmd)]
        [(at-edge? a-cmd) (at-edge-cmd a-cmd)]
        [(move-cmd? a-cmd) (move-animation-cmd a-cmd)]
        [(jump-cmd? a-cmd) (jump-animation-cmd a-cmd)]
        [(random-jump-cmd? a-cmd) (random-jump-animation-cmd a-cmd)]
        [(delete-cmd? a-cmd) (delete-animation-cmd a-cmd)]
        [(change-vel? a-cmd) (change-vel-cmd a-cmd)]
        ))

;; show-animation : display-animation -> void
;; adds the animation to the global list of animations to display
(define (show-animation a-disp-ani)
  (let ([a-ani (display-animation-ani a-disp-ani)])
    (set! IMGS (cons a-ani IMGS))))

;; repeat-func : cmd -> void
;; repeats a list of commands until a specified end event
;; is satisfied (eg. two animations collide)
(define (repeat-func a-cmd)
  (cond [(not (run-cmd (repeat-until-end-event a-cmd)))
         (begin (run-cmdlist (repeat-until-actions a-cmd))
                (repeat-func a-cmd))]
        [else void]))

;; collided? : cmd -> boolean
;; returns true if the two animations collided,
;; false if they didn't
(define (collided? a-cmd)
  (let ([ani1 (get-animation (collision-ani1-name a-cmd) IMGS)]
        [ani2 (get-animation (collision-ani2-name a-cmd) IMGS)])
    (cond [(and (a-circle? ani1)
                (a-circle? ani2))
           (check-collide-circles ani1 ani2)]
          [(and (a-circle? ani1)
                (a-rect? ani2))
           (check-collide-circle-rect ani1 ani2)]
          [(and (a-rect? ani1)
                (a-circle? ani2))
           (check-collide-circle-rect ani2 ani1)]
          [(and (a-rect? ani1)
                (a-rect? ani2))
           (check-collide-rects ani1 ani2)])))

;; check-collide-circles : a-circle a-circle -> boolean
;; checks if two circles are touching by seeing if the
;; distance between their centers is closer than the sum
;; of their radii
(define (check-collide-circles ani1 ani2)
  (<= (get-distance (a-circle-loc ani1) (a-circle-loc ani2))
      (+ (a-circle-radius ani1) (a-circle-radius ani2))))

;; check-collide-circle-rect : a-circle a-rect -> boolean
;; checks if a circle and a rect have collided
;; (algorithm explained in project report)
(define (check-collide-circle-rect cir rec)
  (<= (get-distance (a-circle-loc cir) (a-rect-loc rec))
      (+ (a-circle-radius cir) (/ (+ (a-rect-x-len rec) (a-rect-y-len rec)) 
                                  3))))

;; check-collide-rects : a-rect a-rect -> boolean
;; checks if two rects have collided
;; (algorithm explained in project report)
(define (check-collide-rects ani1 ani2)
  (<= (get-distance (a-rect-loc ani1) (a-rect-loc ani2))
      (+ (/ (+ (* (/ 1 2) (a-rect-x-len ani1)) 
               (sqrt (+ (sqr (* (/ 1 2) (a-rect-x-len ani1)))
                        (sqr (* (/ 1 2) (a-rect-y-len ani1)))))) 
            2)
         (/ (+ (* (/ 1 2) (a-rect-x-len ani2)) 
               (sqrt (+ (sqr (* (/ 1 2) (a-rect-x-len ani2)))
                        (sqr (* (/ 1 2) (a-rect-y-len ani2))))))
            2))))

;; at-edge-cmd : cmd -> boolean
;; true if animation is at the specified edge, false otherwise
(define (at-edge-cmd a-cmd)
  (let ([a-ani (get-animation (at-edge-ani-name a-cmd) IMGS)]
        [a-edge (at-edge-edge a-cmd)])
    (cond [(a-circle? a-ani) (at-edge-circ a-ani a-edge)]
          [(a-rect? a-ani) (at-edge-rect a-ani a-edge)])))

;; at-edge-circ : animation symbol -> boolean
;; helper for at-edge, used for circles
(define (at-edge-circ a-ani a-edge)
  (cond [(symbol=? 'top a-edge)
           (<= (- (posn-y (a-circle-loc a-ani)) (a-circle-radius a-ani))
               0)]
          [(symbol=? 'bottom a-edge)
           (>= (+ (posn-y (a-circle-loc a-ani)) (a-circle-radius a-ani))
               HEIGHT)]
          [(symbol=? 'left a-edge)
           (<= (- (posn-x (a-circle-loc a-ani)) (a-circle-radius a-ani))
               0)]
          [(symbol=? 'right a-edge)
           (>= (+ (posn-x (a-circle-loc a-ani)) (a-circle-radius a-ani))
               WIDTH)]))

;; at-edge-rect : animation symbol -> boolean
;; helper for at-edge, used for rects
(define (at-edge-rect a-ani a-edge)
  (let ([dist (/ (+ (* (/ 1 2) (a-rect-x-len a-ani)) 
                    (sqrt (+ (sqr (* (/ 1 2) (a-rect-x-len a-ani)))
                             (sqr (* (/ 1 2) (a-rect-y-len a-ani)))))) 
                 2)])
    (cond [(symbol=? 'top a-edge)
           (<= (- (posn-y (a-rect-loc a-ani)) dist)
               0)]
          [(symbol=? 'bottom a-edge)
           (>= (+ (posn-y (a-rect-loc a-ani)) dist)
               HEIGHT)]
          [(symbol=? 'left a-edge)
           (<= (- (posn-x (a-rect-loc a-ani)) dist)
               0)]
          [(symbol=? 'right a-edge)
           (>= (+ (posn-x (a-rect-loc a-ani)) dist)
               WIDTH)])))

;; move-animation-cmd : cmd -> void
;; moves the specified animation based on its velocity
(define (move-animation-cmd a-cmd)
  (move-animation (get-animation (move-cmd-ani-name a-cmd) IMGS)))

;; move-animation : animation -> void
;; updates IMGS with a version of the given animation that
;; has been moved based on its velocity
(define (move-animation a-ani)
  (let ([name (cond [(a-circle? a-ani) (a-circle-name a-ani)]
                    [(a-rect? a-ani) (a-rect-name a-ani)])])
    (begin (run-cmd
            (make-delete-cmd name))
           (cond [(a-circle? a-ani)
                  (set! IMGS (cons (change-pos a-ani (make-posn (+ (posn-x (a-circle-loc a-ani))
                                                                   (posn-x (a-circle-speed a-ani)))
                                                                (+ (posn-y (a-circle-loc a-ani))
                                                                   (posn-y (a-circle-speed a-ani)))))
                                   IMGS))]
                 [(a-rect? a-ani)
                  (set! IMGS (cons (change-pos a-ani (make-posn (+ (posn-x (a-rect-loc a-ani))
                                                                   (posn-x (a-rect-speed a-ani)))
                                                                (+ (posn-y (a-rect-loc a-ani))
                                                                   (posn-y (a-rect-speed a-ani)))))
                                   IMGS))]))))

;; delete-animation-cmd : cmd -> void
;; deletes the specified animation
(define (delete-animation-cmd a-cmd)
  (delete-animation (get-animation (delete-cmd-ani-name a-cmd) IMGS)))

;; delete-animation : animation -> void
;; removes the animation from the list of animations to draw (IMGS)
(define (delete-animation a-ani)
  (let ([a-name (cond [(a-circle? a-ani) (a-circle-name a-ani)]
                      [(a-rect? a-ani) (a-rect-name a-ani)])])
    (set! IMGS (filter (lambda (obj) (cond [(a-circle? obj)
                                            (not (symbol=? a-name (a-circle-name obj)))]
                                           [(a-rect? obj)
                                            (not (symbol=? a-name (a-rect-name obj)))]))
                       IMGS))))

;; jump-animation-cmd : cmd -> void
;; puts the animation in a new location based on the given new-loc
(define (jump-animation-cmd a-cmd)
  (jump-animation (get-animation (jump-cmd-ani-name a-cmd) IMGS) (jump-cmd-new-loc a-cmd)))

;; jump-animation : animation posn -> void
;; updates IMGS with a version of the given animation
;; that has been moved to the given location
(define (jump-animation a-ani a-loc)
  (let ([name (cond [(a-circle? a-ani) (a-circle-name a-ani)]
                    [(a-rect? a-ani) (a-rect-name a-ani)])])
    (begin (run-cmd
            (make-delete-cmd name))
           (cond [(a-circle? a-ani)
                  (set! IMGS (cons (change-pos a-ani a-loc) IMGS))]
                 [(a-rect? a-ani)
                  (set! IMGS (cons (change-pos a-ani a-loc) IMGS))]))))

;; random-jump-animation-cmd : cmd -> void
;; puts the animation in a random new location
(define (random-jump-animation-cmd a-cmd)
  (jump-animation (get-animation (random-jump-cmd-ani-name a-cmd) IMGS) (random-posn)))

;; change-vel-cmd : cmd -> void
;; changes the given animation's velocity
(define (change-vel-cmd a-cmd)
  (change-velocity (change-vel-ani-name a-cmd)
                   (get-animation (change-vel-ani-name a-cmd) IMGS)
                   (change-vel-new-vel a-cmd)))

;; change-velocity : symbol animation posn
;; updates IMGS with a version of the given animation
;; that has the given velocity
(define (change-velocity name a-ani new-vel)
  (begin (run-cmd
          (make-delete-cmd name))
         (cond [(a-circle? a-ani)
                (set! IMGS (cons (make-a-circle (a-circle-name a-ani)
                                                (a-circle-color a-ani)
                                                (a-circle-radius a-ani)
                                                (a-circle-loc a-ani)
                                                new-vel)
                                 IMGS))]
               [(a-rect? a-ani)
                (set! IMGS (cons (make-a-rect (a-rect-name a-ani)
                                              (a-rect-color a-ani)
                                              (a-rect-x-len a-ani)
                                              (a-rect-y-len a-ani)
                                              (a-rect-loc a-ani)
                                              new-vel)
                                 IMGS))])))

;;;;;;;;;;;;;;; MISCELLANEOUS HELPERS ;;;;;;;;;;;;;;;

;; get-animation : symbol list[animation] -> animation or false if not in list
;; returns the animation with the param name
(define (get-animation name anis)
  (cond [(empty? anis) false]
        [(cons? anis) (cond [(a-circle? (first anis))
                             (cond [(symbol=? name (a-circle-name (first anis)))
                                    (first anis)]
                                   [else (get-animation name (rest anis))])]
                            [(a-rect? (first anis))
                             (cond [(symbol=? name (a-rect-name (first anis)))
                                    (first anis)]
                                   [else (get-animation name (rest anis))])])]))

;; change-pos : animation posn -> animation
;; returned animation is same type as input, but the position is changed to new-loc
(define (change-pos a-ani new-loc)
  (cond [(a-circle? a-ani) (make-a-circle (a-circle-name a-ani)
                                          (a-circle-color a-ani)
                                          (a-circle-radius a-ani)
                                          new-loc
                                          (a-circle-speed a-ani))]
        [(a-rect? a-ani) (make-a-rect (a-rect-name a-ani)
                                      (a-rect-color a-ani)
                                      (a-rect-x-len a-ani)
                                      (a-rect-y-len a-ani)
                                      new-loc
                                      (a-rect-speed a-ani))]))

;; random-posn : -> posn
;; returns a random posn between WIDTH and HEIGHT
(define (random-posn)
  (make-posn (random WIDTH) (random HEIGHT)))

;; get-distance : posn posn -> number
;; returns the distance between two positions
(define (get-distance loc1 loc2)
  (sqrt (+ (sqr (- (posn-x loc1)
                   (posn-x loc2)))
           (sqr (- (posn-y loc1)
                   (posn-y loc2))))))

;; draw-canvas : list[animation] -> scene
(define (draw-canvas anis)
  (update-frame (draw-world anis)))

;; draw-world : list[animation] -> scene
;; draws each element of the world into a scene
(define (draw-world anis)
  (cond [(empty? anis) (empty-scene WIDTH HEIGHT)]
        [(cons? anis) (let ([a-ani (first anis)])
                        (cond [(a-circle? a-ani) (place-image (circle (a-circle-radius a-ani)
                                                                      "solid"
                                                                      (a-circle-color a-ani))
                                                              (posn-x (a-circle-loc a-ani)) 
                                                              (posn-y (a-circle-loc a-ani))
                                                              (draw-world (rest anis)))]
                              [(a-rect? a-ani) (place-image (rectangle (a-rect-x-len a-ani)
                                                                       (a-rect-y-len a-ani)
                                                                       "solid"
                                                                       (a-rect-color a-ani))
                                                            (posn-x (a-rect-loc a-ani))
                                                            (posn-y (a-rect-loc a-ani))
                                                            (draw-world (rest anis)))]))]))

;;;;;;;;;;;;;;; EXTRA TEST CASES ;;;;;;;;;;;;;;;

(define MOVE-TEST
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                           "red"
                                           75
                                           (make-posn 200 400)
                                           (make-posn 50 0)))
    (make-move-cmd 'c1)
    (make-move-cmd 'c1)
    (make-move-cmd 'c1)
    (make-move-cmd 'c1)
    )))

(define JUMP-TEST
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                           "red"
                                           75
                                           (make-posn 200 400)
                                           (make-posn 50 0)))
    (make-jump-cmd 'c1 (make-posn 100 100))
    (make-jump-cmd 'c1 (make-posn 50 50))
    (make-jump-cmd 'c1 (make-posn 300 300))
    )))

(define RANDOM-JUMP-TEST
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                           "red"
                                           75
                                           (make-posn 200 400)
                                           (make-posn 50 0)))
    (make-random-jump-cmd 'c1)
    (make-random-jump-cmd 'c1)
    (make-random-jump-cmd 'c1)
    )))

(define COLLISION-TEST-CIRC
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                           "red"
                                           75
                                           (make-posn 200 400)
                                           (make-posn 10 0)))
    (make-display-animation (make-a-circle 'c2
                                           "blue"
                                           75
                                           (make-posn 400 400)
                                           (make-posn -10 0)))
    (make-repeat-until (make-collision 'c1 'c2)
                       (list (make-move-cmd 'c1)
                             (make-move-cmd 'c2))))))

(define COLLISION-TEST-CR
  (make-canvas
   (list
    (make-display-animation (make-a-circle 'c1
                                           "red"
                                           75
                                           (make-posn 200 400)
                                           (make-posn 10 0)))
    (make-display-animation (make-a-rect 'r1
                                           "blue"
                                           75
                                           75
                                           (make-posn 400 400)
                                           (make-posn -10 0)))
    (make-repeat-until (make-collision 'c1 'r1)
                       (list (make-move-cmd 'c1)
                             (make-move-cmd 'r1))))))