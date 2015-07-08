;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname design) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
;; Michael Giancola - Individual Project Design

(define WIDTH 800)
(define HEIGHT 500)
(define LEFT-WALL 0)
(define TOP-WALL 0)

;; a canvas is (make-canvas list[cmd])
(define-struct canvas (cmds))

;; a cmd is either
;; - (make-display-animation animation) or
;; - (make-animation-interaction (animation animation -> boolean) list[cmd] list[cmd]) or
;; - (make-move-until-collision (animation graphics-object -> boolean) animation graphics-object) or
;; - (make-jump-until-collision (animation graphics-object -> boolean) animation graphics-object) or
;; - (make-move-cmd animation) or
;; - (make-jump-cmd animation jump-loc) or
;; - (make-move-x-times move-cmd number) or
;; - (make-jump-x-times jump-cmd number) or
;; - (make-delete-cmd animation)

;; a display-animation is (make-display-animation animation)
(define-struct display-animation (a-animation))

;; a animation-interaction is (make-animation-interaction (animation graphics-object -> boolean) list[cmd] list[cmd])
(define-struct animation-interaction (collided? if-true if-false))

;; a move-until-collision is (make-move-until-collision (animation graphics-object -> boolean) animation graphics-object)
(define-struct move-until-collision (collided? obj1 obj2))

;; a jump-until-collision is (make-jump-until-collision (animation graphics-object -> boolean) animation graphics-object)
(define-struct jump-until-collision (collided? obj1 obj2))

;; a move-cmd is (make-move-cmd animation)
(define-struct move-cmd (a-ani))

;; a jump-cmd is (make-jump-cmd animation jump-loc)
(define-struct jump-cmd (a-ani new-loc))

;; a jump-loc is either
;; - a posn, or
;; - (make-random-posn (scene-width scene-height))

;; a move-x-times is (make-move-x-times move-cmd number)
(define-struct move-x-times (move times))

;; a jump-x-times is (make-jump-x-times jump-cmd number)
(define-struct jump-x-times (jump times))

;; a delete-cmd is (make-delete-cmd animation)
(define-struct delete-cmd (a-ani))

;; a animation is (make-animation image posn posn posn)
(define-struct animation (img init-loc curr-loc speed))

;; a image is either
;; - (make-circle radius ...) or
;; - (make-rectangle x-length y-length ...) or
;; - other image (ie. jpeg)

;; a graphics-object is either
;; - (make-animation image posn posn posn) or
;; - a number (ie. a edge of the screen)

;; a random-posn is (make-random-posn number number)
(define-struct random-posn (scene-width scene-height))
;; this will be necessary to generate a random position,
;; within the scene's boundaries, for random jumping

(define ANI-1
  (let (
        [circle (make-animation (circle 50 "solid" "red")
                                (make-posn 100 100)
                                (make-posn 100 100)
                                (make-posn 30 40))]
        
        [rectangle (make-animation (rectangle 100 400 "solid" "blue")
                                   (make-posn 600 250)
                                   (make-posn 600 250)
                                   (make-posn 0 0))]
        [circ-touching-rect? (lambda (circ rect) (and (= (posn-x (animation-curr-loc circ)) (posn-x (animation-curr-loc rect)))
                                                      (= (posn-y (animation-curr-loc circ)) (posn-y (animation-curr-loc rect)))))]
        [circ-touching-wall? (lambda (circ wall) (= (posn-x (animation-curr-loc circ)) wall))])
    
    (make-canvas
     (list
      (make-display-animation circle)
      (make-display-animation rectangle)
      (make-move-until-collision circ-touching-rect?
                                 circle
                                 rectangle)
      (make-animation-interaction circ-touching-rect?
                                  (list (make-delete-cmd rectangle)
                                        (make-animation (animation-img circle)
                                                        (animation-init-loc circle)
                                                        (animation-curr-loc circle)
                                                        (make-posn (- 0 (posn-x (animation-speed circle)))
                                                                   (posn-y (animation-speed circle)))))
                                  (list empty))
      (make-move-until-collision circ-touching-wall?
                                 circle
                                 LEFT-WALL)))))

(define ANI-2
  (let (
        [circle (make-animation (circle 50 "solid" "purple")
                                (make-posn 700 300)
                                (make-posn 700 300)
                                (make-posn 0 0))]
        [circ-touching-wall? (lambda (circ wall) (= (posn-y (animation-curr-loc circ)) wall))])
    
    (make-canvas
     (list
      (make-display-animation circle)
      (make-jump-until-collision circ-touching-wall?
                                 circle
                                 TOP-WALL)))))

(define ANI-3 
  (let (
        [circle (make-animation (circle 50 "solid" "orange")
                                (make-posn 150 100)
                                (make-posn 150 100)
                                (make-posn 0 20))]
        [green-rect (make-animation (rectangle 700 50 "solid" "green")
                                    (make-posn 400 400)
                                    (make-posn 400 400)
                                    (make-posn 0 0))]
        [red-rect (make-animation (rectangle 50 250 "solid" "red")
                                  (make-posn 200 400)
                                  (make-posn 200 400)
                                  (make-posn 0 0))]
        [circ-touching-rect? (lambda (circ rect) (and (= (posn-x (animation-curr-loc circ)) (posn-x (animation-curr-loc rect)))
                                                      (= (posn-y (animation-curr-loc circ)) (posn-y (animation-curr-loc rect)))))])
    
    (make-canvas
     (list
      (make-display-animation circle)
      (make-display-animation green-rect)
      (make-move-until-collision circ-touching-rect?
                                 circle
                                 green-rect)
      
      (make-animation-interaction circ-touching-rect?
                                  (list (make-animation (animation-img circle)
                                                        (animation-init-loc circle)
                                                        (animation-curr-loc circle)
                                                        (make-posn 20 0))
                                        (make-display-animation red-rect))
                                  (list empty))
      (make-move-until-collision circ-touching-rect?
                                 circle
                                 red-rect)
      (make-animation-interaction circ-touching-rect?
                                  (list (make-jump-cmd circle (make-random-posn WIDTH HEIGHT)))
                                  (list empty))))))

;; My animation shows two circles moving towards each other. At the point when
;; they collide, the larger one continues moving in the same direction, while
;; the smaller one is removed from the scene
(define MY-ANIMATION
  (let (
        [big-circ (make-animation (circle 100 "solid" "red")
                                  (make-posn 200 400)
                                  (make-posn 200 400)
                                  (make-posn 20 0))]
        [small-circ (make-animation (circle 50 "solid" "blue")
                                    (make-posn 600 400)
                                    (make-posn 600 400)
                                    (make-posn -20 0))]
        [circles-touching? (lambda (big-circ small-circ) (and (= (posn-x (animation-curr-loc big-circ)) (posn-x (animation-curr-loc small-circ)))
                                                              (= (posn-y (animation-curr-loc big-circ)) (posn-y (animation-curr-loc small-circ)))))]
        [circ-touching-wall? (lambda (circ wall) (= (posn-x (animation-curr-loc circ)) wall))])
    
    (make-canvas
     (list
      (make-display-animation big-circ)
      (make-display-animation small-circ)
      (make-move-until-collision circles-touching?
                                 big-circ
                                 small-circ)
      (make-animation-interaction circles-touching?
                                  (list (make-delete-cmd small-circ))
                                  (list empty))
      (make-move-until-collision circ-touching-wall?
                                 big-circ
                                 WIDTH)))))