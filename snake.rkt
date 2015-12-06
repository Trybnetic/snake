;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct game-state (snake next-direction direction candy score))
(define-struct snake (head tail))

(define WIDTH 600)
(define HEIGHT 400)
(define SEGMENT-SIZE 10)
(define MENU-HEIGHT 20)
(define CANDY-COLOR "red")

(define HEAD-SEGMENT (text "□" 15 "black"))
(define CANDY-SEGMENT (text "◌" 15 "red"))
(define TAIL-SEGMENT (text "□" 15 "green"))

; A Field is a Image
; interp. the field of the game
(define BACKGROUND
  (underlay/xy
   (empty-scene WIDTH HEIGHT)
   0 0
   (rectangle WIDTH MENU-HEIGHT "solid" "blue")))

; A Speed is a Number
; interp. speed of the game
(define STANDARD-SPEED 1/10)

; A Direction is one of:
;- "left"
;- "right"
;- "up"
;- "down"
(define UP "up")
(define DOWN "down")
(define RIGHT "right")
(define LEFT "left")

; A Color is a String
; interp. a Color
(define RED "red")
(define YELLOW "yellow")

; A Location is a structure: (make-posn Number Number)
; interp. x and y coordinate of a location on screen.
(define CENTER (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
(define CORNER (make-posn 0 0))

; A Segment is a Location
; interp. a Segment of the game
(define SNAKE-HEAD (make-posn 200 100))
(define SNAKE-HEAD-1 (make-posn (+ (posn-x SNAKE-HEAD) 1) 100))
(define SNAKE-HEAD-2 (make-posn (+ (posn-x SNAKE-HEAD) 1) 100))
(define SNAKE-HEAD-3 (make-posn (+ (posn-x SNAKE-HEAD) 1) 100))
(define CANDY CENTER)

; A Score is a Number
; interp. number of collected Candys
(define INIT-SCORE 0)

; A Snake is a structure: (make-snake Segment (list-of Segment))
; interp. the snake in the game, currently just the head of the snake
(define SNAKE (make-snake SNAKE-HEAD (list SNAKE-HEAD-3 SNAKE-HEAD-2 SNAKE-HEAD-1)))

; A State is a structure: (make-game-state Snake Direction Direction Location Score)
; interp. current position of the snake, position of the target
(define INITIAL-STATE (make-game-state SNAKE LEFT LEFT CENTER INIT-SCORE))

; Segment Segment -> Boolean
; checks whether two locations are identical
(check-expect (location=? CANDY CENTER) #true)
(check-expect (location=? CENTER CORNER) #false)
(define (location=? seg1 seg2)
  (and (= (posn-x seg1) (posn-x seg2))
       (= (posn-y seg1) (posn-y seg2))))

; Segment -> Boolean
; checks wether a Location is inside the field
(check-expect (inside? CENTER) #true)
(check-expect (inside? (make-posn -50 -50)) #false)
(define (inside? seg)
  (and (< 0 (posn-x seg) WIDTH)
       (< MENU-HEIGHT (posn-y seg) HEIGHT)))

; Segment (list-of Segment) -> Boolean
; checks whether a Location is inside a list of Locations
(check-expect (segment-in-list? CENTER empty) #false)
(check-expect (segment-in-list? CENTER (list CORNER CENTER)) #true)
(define (segment-in-list? seg lis)
  (cond
    ((empty? lis) #f)
    (else (or (location=? seg (first lis)) (segment-in-list? seg (rest lis))))))

; Snake -> Boolean
; checks whether a Snake bit itself
(check-expect (bite? (make-snake CENTER (list CORNER CENTER))) #true)
(define (bite? snake)
  (segment-in-list? (snake-head snake) (snake-tail snake)))

; Number -> Number
; increment x
(define (add10 x)
  (+ x 10))

; Number -> Number
; decrement x
(define (sub10 x)
  (- x 10))

; Segment Direction -> Segment
; Moves a Segment in the Direction
(check-expect (move-segment (make-posn 100 100) UP) (make-posn 100 90))
(check-expect (move-segment (make-posn 100 100) DOWN) (make-posn 100 110))
(check-expect (move-segment (make-posn 100 100) RIGHT) (make-posn 110 100))
(check-expect (move-segment (make-posn 100 100) LEFT) (make-posn 90 100))
(define (move-segment seg key)
  (cond ((string=? key UP) (make-posn (posn-x seg)
                                      (sub10 (posn-y seg))))
        ((string=? key DOWN) (make-posn (posn-x seg)
                                        (add10 (posn-y seg))))
        ((string=? key RIGHT) (make-posn (add10 (posn-x seg))
                                         (posn-y seg)))
        ((string=? key LEFT)  (make-posn (sub10 (posn-x seg))
                                         (posn-y seg)))))

; Snake Direction -> Snake
; moves the Snake in the Direction
(define (move-snake snake direction)
  (make-snake (move-segment (snake-head snake) direction)
              (append (rest (snake-tail snake)) (list (snake-head snake)))))

; Direction Direction -> Boolean
; checks whether two Directions are opposite to each other
(check-expect (opposite-directions? RIGHT UP) #false)
(check-expect (opposite-directions? RIGHT LEFT) #true)
(check-expect (opposite-directions? DOWN UP) #true)
(check-expect (opposite-directions? LEFT UP) #false)
(check-expect (opposite-directions? UP DOWN) #true)
(define (opposite-directions? dir1 dir2)
  (or
   (and (string=? dir1 LEFT) (string=? dir2 RIGHT))
   (and (string=? dir1 RIGHT) (string=? dir2 LEFT))
   (and (string=? dir1 UP) (string=? dir2 DOWN))
   (and (string=? dir1 DOWN) (string=? dir2 UP))))

; Direction -> Boolean
; checks whether an input key is permitted
(check-expect (permitted-key? UP) #true)
(check-expect (permitted-key? LEFT) #true)
(check-expect (permitted-key? "k") #false)
(check-expect (permitted-key? "x") #false)
(define (permitted-key? key)
  (or (string=? key UP)
      (string=? key DOWN)
      (string=? key LEFT)
      (string=? key RIGHT)))

; State Direction -> State
; prevents that the Snake Head moves in the opposite direction
(define (key state a-key)
  (cond ((opposite-directions? a-key (game-state-next-direction state)) state)
        ((permitted-key? a-key) (make-game-state (game-state-snake state)
                                                 a-key
                                                 (game-state-next-direction state)
                                                 (game-state-candy state)
                                                 (game-state-score state)))
        (else state)))


; Score Field -> Field
; Prints the Score on the Field
(define (print-score score field)
  (place-image
   (text (string-append "Score: " (number->string score)) 12 YELLOW)
   50 (/ MENU-HEIGHT 2) 
   field))

; Candy Field -> Field
; Prints the Candy on the Field
(define (print-candy candy field)
  (underlay/xy
   field
   (- (posn-x candy) SEGMENT-SIZE)
   (- (posn-y candy) (/ SEGMENT-SIZE 2) 10)
   CANDY-SEGMENT))

; Segment Field -> Field
; Prints the head of the Snake on the Field
(define (print-head head field)
  (place-image
   HEAD-SEGMENT
   (- (posn-x head) (/ SEGMENT-SIZE 2))
   (- (posn-y head) (/ SEGMENT-SIZE 2))
   field))

; Segment Field -> Field
; Prints a Segment of the tail of the Snake on the Field
(define (print-tail-elem elem field)
  (place-image
   TAIL-SEGMENT
   (- (posn-x elem) (/ SEGMENT-SIZE 2))
   (- (posn-y elem) (/ SEGMENT-SIZE 2))
   field))

; (list-of Segment) Field -> Field
; Prints the tail of the Snake on the Field
(define (print-tail tail field)
  (cond ((empty? tail) field)
        (else (print-tail (rest tail) (print-tail-elem (first tail) field)))))

; State -> Image
; render the state to the screen
(define (draw state)
  (print-tail (snake-tail (game-state-snake state))
              (print-head (snake-head(game-state-snake state))
                          (print-candy (game-state-candy state)
                                       (print-score (game-state-score state) BACKGROUND)))))


; Snake -> Boolean
; Checks whether the Snake died in this tick
(define (died? snake)
  (or (not (inside? (snake-head snake)))
      (bite? snake)))

; Snake Candy -> Boolean
; Checks whether the Snake eats a Candy
(define (collected-candy? snake candy)
  (location=? candy (snake-head snake)))


; State -> State
; update the state if candy was eaten
(define (tick-after-eated-candy state)
  (make-game-state (move-snake (make-snake (snake-head (game-state-snake state))
                                           (append (snake-tail (game-state-snake state))
                                                   (list (snake-head (game-state-snake state)))))
                               (game-state-next-direction state))
                   (game-state-next-direction state)
                   (game-state-next-direction state)
                   (make-posn (* 10 (random (/ WIDTH 10)))
                              (+ MENU-HEIGHT (* 10 (random (/ (- HEIGHT MENU-HEIGHT) 10)))))
                   (add1 (game-state-score state))))

; State -> State
; update the state if no candy was eaten
(define (tick-normal state)
  (make-game-state (move-snake (game-state-snake state)
                               (game-state-next-direction state))
                   (game-state-next-direction state)
                   (game-state-next-direction state)
                   (game-state-candy state)
                   (game-state-score state)))

; State -> State
; update the state when time passes
(define (tick state)
  (cond ((died? (game-state-snake state)) INITIAL-STATE)
        ((collected-candy? (game-state-snake state) (game-state-candy state)) (tick-after-eated-candy state))
        (else (tick-normal state))))


; State -> State
; runs the circle game
(define (circle-game initial-state)
  (big-bang initial-state
            [to-draw draw]
            [on-tick tick STANDARD-SPEED]
            [on-key key]
            ))


(circle-game INITIAL-STATE)