# criss-cross


(require "puzlib.rkt")


;;
;;*****************************************
;; Vibhanshu Bhardwaj
;; 20607705/v9bhardw
;; CS 135, Assignment 10 Question-2
;;*****************************************
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz02 (read-puzzle "puzzle02.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define puzz05 (read-puzzle "puzzle05.txt"))
(define puzz06 (read-puzzle "puzzle06.txt"))
(define puzz07 (read-puzzle "puzzle07.txt"))
(define puzz08 (read-puzzle "puzzle08.txt"))
(define puzz09 (read-puzzle "puzzle09.txt"))
(define puzz10 (read-puzzle "puzzle10.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid-a '((#\A) (#\X)))
(define grid-b '((#\A #\B) (#\X #\Y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose my-grid) takes in a grid (my-grid) & produces its transpose
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose empty) empty)
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))

(define (transpose my-grid)  
  (cond
    [(empty? my-grid) empty]
    [(empty? (first my-grid)) empty]
    [else (cons (map first my-grid)
                (transpose (map rest my-grid)))]))

;; Tests:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose empty) empty)


;;(find-wpos-1 lochar n-row p q) takes in lochar, n-row, p and q
;; and returns valid wpos's
;;find-wpos-1: (listof Char) Nat Nat Nat -> (listof WPos)
;;
;;Examples:
(check-expect (find-wpos-1 (string->list "#") 0 0 0 ) empty)
(check-expect (find-wpos-1 (string->list "") 0 0 0 ) empty)

(define (find-wpos-1 lochar n-row p q)  
  (cond
    [(and (empty? lochar) (> p 1))
     (cons (make-wpos n-row (- q p) true p) empty)]
    [(and (empty? lochar) (not (> p 1)) ) empty]
    [(equal? #\# (first lochar))
     (find-wpos-1 (rest lochar) n-row (add1 p) (add1 q))]
    [(> p 1) (cons (make-wpos n-row (- q p) true p)
                   (find-wpos-1 (rest lochar) n-row 0
                                (add1 q)))]
    [else (find-wpos-1 (rest lochar) n-row 0 (add1 q))]))

;;Tests:
(check-expect (find-wpos-1 (string->list "###.###") 0 0 0 )
              (list (make-wpos 0 0 #true 3) (make-wpos 0 4 #true 3)))
(check-expect (find-wpos-1 (string->list "...") 0 0 0) empty)
(check-expect (find-wpos-1 (string->list "#.#.#.#.#") 0 0 0)
              empty)

;; (find-wpos loc row) consumes row of chars (row and loc)
;; and produces list of valid wpos.
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "#") 0) empty)
(check-expect(find-wpos (string->list "") 0) empty)
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))

(define (find-wpos loc row)
  (find-wpos-1 loc row 0 0))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "...") 0) empty)
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))


;; the order does not matter: here is an example
;; that uses lists-equiv?

(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)

(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))true)



;;(find-all-wpos my-grid x) takes in a grid (my-grid) and x
;; and returns list of all valid wpos.
;;find-all-wpos (listof (listof Char)) Nat -> (listof WPos)
;;Examples:
(check-expect(find-all-wpos (list (list #\# #\# #\#))0)
             (list (make-wpos 0 0 #true 3)))
(check-expect (find-all-wpos (list (string->list "....") (string->list "...."))
                             0)
              empty)


(define (find-all-wpos my-grid x)
  (cond
    [(= x (length my-grid)) empty]
    [else (append (find-wpos (list-ref my-grid x) x)
                  (find-all-wpos my-grid (add1 x)))]))

;;Tests:
(check-expect (find-all-wpos (list (string->list "....") (string->list "...."))
                             0)
              empty)
(check-expect(find-all-wpos (list (list #\# #\# #\#))0)
             (list (make-wpos 0 0 #true 3)))


;;(all-flipped lo-wpos) flips all the wpos in lo-wpos.
;;all-flipped  (listof WPos) -> (listof WPos)
;;Examples:
(check-expect (all-flipped (list (make-wpos 1 5 #true 3)))
              (list (make-wpos 5 1 #false 3)))
(check-expect(all-flipped (list (make-wpos 0 0 true 0)))
             (list (make-wpos 0 0 false 0)))

(define (all-flipped lo-wpos)  
  (cond
    [(empty? lo-wpos) empty]
    [else (cons (flip (first lo-wpos))
                (all-flipped (rest lo-wpos)))]))

;;Tests:
(check-expect (all-flipped (list (make-wpos 1 5 #true 3)))
              (list (make-wpos 5 1 #false 3)))
(check-expect(all-flipped (list (make-wpos 0 0 true 0)))
             (list (make-wpos 0 0 false 0)))

;; (initial-state my-puzzle) produces starting state of puzzle.
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state puzz05)
              (make-state
               (list
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.)
                (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
                (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
                (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
                (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
                (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
                (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
                (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
                (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
                (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
                (list #\. #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\# #\#))
               (list
                (make-wpos 0 0 true 7)(make-wpos 1 6 true 7)
                (make-wpos 2 0 true 7)
                (make-wpos 3 6 true 7)(make-wpos 4 0 true 7)
                (make-wpos 5 6 true 7)
                (make-wpos 6 0 true 7)(make-wpos 7 7 true 7)
                (make-wpos 8 1 true 7)
                (make-wpos 9 7 true 7)(make-wpos 10 1 true 7)
                (make-wpos 11 7 true 7)
                (make-wpos 12 1 true 7)(make-wpos 13 7 true 7)
                (make-wpos 0 0 false 7)
                (make-wpos 6 1 false 7)(make-wpos 0 2 false 7)
                (make-wpos 6 3 false 7)
                (make-wpos 0 4 false 7)(make-wpos 6 5 false 7)
                (make-wpos 0 6 false 7)
                (make-wpos 7 7 false 7)(make-wpos 1 8 false 7)
                (make-wpos 7 9 false 7)
                (make-wpos 1 10 false 7)(make-wpos 7 11 false 7)
                (make-wpos 1 12 false 7)
                (make-wpos 7 13 false 7))
               (list
                "ABALONE" "ACREAGE" "ADMIRER" "AIRHEAD" "APPOINT" "AWKWARD"
                "DYNASTY"
                "EARLOBE" "EXTRACT" "EXTREME" "EYEBROW" "FANFARE"
                "FRECKLE""INNARDS""MAESTRO""PATTERN""RAFTERS""RAPPORT""REFEREE"
                "REMORSE"
                "RESTART""ROTUNDA""SMOLDER""SUNSPOT" "TERRACE" "THEATRE"
                "TORNADO"
                "TRAMWAY" "YARDAGE")))

(define (initial-state my-puzzle) 
  (local   
    [(define my-grid (map string->list (first my-puzzle)))]
    (make-state my-grid (append (find-all-wpos my-grid 0)
                                (all-flipped
                                 (find-all-wpos (transpose my-grid) 0)))
                (second my-puzzle))))

;; Tests:

(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state puzz05)
              (make-state
               (list
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.)
                (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
                (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
                (list #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\# #\.)
                (list #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\# #\.)
                (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
                (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
                (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
                (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
                (list #\. #\# #\. #\# #\. #\# #\. #\# #\# #\# #\# #\# #\# #\#)
                (list #\. #\# #\# #\# #\# #\# #\# #\# #\. #\# #\. #\# #\. #\#)
                (list #\. #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\# #\#))
               (list
                (make-wpos 0 0 true 7)(make-wpos 1 6 true 7)
                (make-wpos 2 0 true 7)
                (make-wpos 3 6 true 7)(make-wpos 4 0 true 7)
                (make-wpos 5 6 true 7)
                (make-wpos 6 0 true 7)(make-wpos 7 7 true 7)
                (make-wpos 8 1 true 7)
                (make-wpos 9 7 true 7)(make-wpos 10 1 true 7)
                (make-wpos 11 7 true 7)
                (make-wpos 12 1 true 7)(make-wpos 13 7 true 7)
                (make-wpos 0 0 false 7)
                (make-wpos 6 1 false 7)(make-wpos 0 2 false 7)
                (make-wpos 6 3 false 7)
                (make-wpos 0 4 false 7)(make-wpos 6 5 false 7)
                (make-wpos 0 6 false 7)
                (make-wpos 7 7 false 7)(make-wpos 1 8 false 7)
                (make-wpos 7 9 false 7)
                (make-wpos 1 10 false 7)(make-wpos 7 11 false 7)
                (make-wpos 1 12 false 7)
                (make-wpos 7 13 false 7))
               (list
                "ABALONE" "ACREAGE" "ADMIRER" "AIRHEAD" "APPOINT" "AWKWARD"
                "DYNASTY"
                "EARLOBE" "EXTRACT" "EXTREME" "EYEBROW" "FANFARE"
                "FRECKLE""INNARDS""MAESTRO""PATTERN""RAFTERS""RAPPORT""REFEREE"
                "REMORSE"
                "RESTART""ROTUNDA""SMOLDER""SUNSPOT" "TERRACE" "THEATRE"
                "TORNADO"
                "TRAMWAY" "YARDAGE")))


;;(grid->lochar my-grid n) takes my-grid and produces
;; list of first n characters from start.
;;grid->lochar: Grid Nat -> (listof Char)
;; requires: n <= (length my-grid)
;;Examples:
(check-expect (grid->lochar (string->list "") 23)
              empty)
(check-expect (grid->lochar (string->list "abWVDF!@") 1)
              (list #\a))

(define (grid->lochar my-grid n)
  (cond
    [(or (empty? my-grid)(zero? n)) empty]
    [else (cons (first my-grid)
                (grid->lochar (rest my-grid) (- n 1)))]))

;;Tests:

(check-expect (grid->lochar (string->list "") 7)
              empty)
(check-expect (grid->lochar (string->list "CULT OF PERSIONALITY") 20)
              (string->list "CULT OF PERSIONALITY"))


;;(my-find grid x y final-x final-y n) searches grid and
;; returns the grid as determined by x y final-x final-y, and n.
;;my-find: Grid Nat Nat Nat Nat Nat -> Grid
;;Examples:
(check-expect (my-find empty 0 0 2 2 2) empty)
(check-expect (my-find (list (list #\ )
                             (list #\ )
                             (list #\ )) 0 0 0 0 1)
              (list #\space))

(define (my-find grid x y final-x final-y n)  
  (cond
    [(empty? grid) empty]
    [(= x final-x)
     (cond
       [(= y final-y) (grid->lochar (first grid) n)]
       [else (my-find (map rest grid) x (add1 y) final-x final-y n)])]
    [else (my-find (rest grid) (add1 x) y final-x final-y n)]))      

;;Tests:
(check-expect (my-find (list (list #\.)
                             (list #\.)
                             (list #\.)) 0 0 0 0 1)
              (list #\.))
(check-expect (my-find empty 0 0 2 2 2) empty)


;; (extract-wpos g wp) takes a Grid (g), a WPos (wp) and produces
;; the (listof Char) that matches with word-position in Grid.
;;
;; extract-wpos: Grid WPos -> (listof Char)
;;
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))


(define (extract-wpos g wp)  
  (cond
    [(wpos-horiz? wp) (my-find g 0 0 (wpos-row wp)
                               (wpos-col wp) (wpos-len wp))]
    [else (my-find (transpose g) 0 0 (wpos-col wp)
                   (wpos-row wp) (wpos-len wp))]))

;; Tests:  
(check-expect  (extract-wpos grid-abc (make-wpos 0 1 true 2)) (list #\B #\C))
(check-expect(extract-wpos (list (list #\  #\  #\. #\.)
                                 (list #\# #\# #\# #\#)
                                 (list #\. #\  #\. #\ )) (make-wpos 1 0 true 3))
             (list #\# #\# #\#))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))


;;(edit row x y lochar) swaps lochar with the relevant char in the row as 
;; decided by x and y (related to the position of the characters in case)
;;edit: (listof Char) Nat Nat (listof Char) -> (listof Char)
;;Examples: 
(check-expect (edit (string->list "") 0 0 (string->list ""))
              empty)
(check-expect (edit (string->list "") 2 2 (string->list "xyz"))
              (list #\x #\y #\z))

(define (edit row x y lochar)
  (cond
    [(= x y) (append lochar row)]
    [else (edit (rest row) x (+ 1 y) lochar)]))

;;Tests:
(check-expect (edit (string->list "xxx") 3 0 (string->list "yyy"))
              (list #\y #\y #\y))
(check-expect (edit (string->list "a") 1 0 (string->list "s"))
              (list #\s))


;;(row-iterate grid x y final-x final-y n lochar) iterates through the rows
;;   to find the right row (as determined by x y final-x final-y n & lochar)
;; and then calls column-iterate.
;;row-iterate: (listof (listof Char)) Nat Nat Nat Nat Nat (listof Char)
;;                            -> (listof (listof Char))
;;Examples:
(check-expect (row-iterate empty 0 0 2 2 0 '(#\a #\b #\c #\d)) empty)
(check-expect (row-iterate empty 0 0 3 1 0 empty) empty)

(define (row-iterate grid x y final-x final-y n lochar) 
  (cond
    [(empty? grid) empty]
    [(= x final-x)
     (cons (column-iterate (first grid) y final-y n lochar) (rest grid))]
    [else (cons (first grid)
                (row-iterate (rest grid)(add1 x) y final-x final-y n lochar))]))

;;Tests:
(check-expect(row-iterate (list (list #\# #\. #\. #\.)
                                (list #\# #\# #\. #\.)
                                (list #\# #\. #\. #\.)) 0 0 0 0 1
                                                        (string->list "x"))
             (list (list #\x #\. #\. #\.) (list #\# #\# #\. #\.)
                   (list #\# #\. #\. #\.)))

(check-expect (row-iterate empty 0 0 1 0 2 (string->list "Eminem")) empty)


;;(column-iterate column y final-y n lochar) iterates through the column
;; and replaces with lochar, position as determined by y, final-y and n.
;;column-iterate: (listof Char) Nat Nat Nat (listof Char) -> (listof Char)
;;Examples:
(check-expect (column-iterate (string->list "abCDEf") 0 2 3
                              (string->list "Sky"))
              (list #\a #\b #\S #\k #\y #\f))
(check-expect (column-iterate (string->list "") 0 2 3 (string->list "abc"))
              empty)


(define (column-iterate column y final-y n lochar)
  (cond
    [(empty? column) empty]
    [(= y final-y) (edit column n 0 lochar)]
    [else (cons (first column)
                (column-iterate (rest column) (add1 y) final-y n lochar))])) 

;;Tests:
(check-expect (column-iterate
               (string->list "abCDEf") 0 2 3 (string->list "Sky"))
              (list #\a #\b #\S #\k #\y #\f))
(check-expect (column-iterate (string->list "") 0 2 3 (string->list "abc"))
              empty)

;;(replace-wpos g wp lochar) takes a grid(g) and produces a gird
;; with the word position (wp) replaced by lochar.
;;replace-wpos: Grid WPos (listof Char) -> Grid
;;requires: len in WPos is equal to length of lochar.
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc) 
  (cond
    [(wpos-horiz? wp)
     (row-iterate g 0 0 (wpos-row wp) (wpos-col wp) (wpos-len wp) loc)]
    [else (transpose (row-iterate (transpose g) 0 0
                                  (wpos-col wp) (wpos-row wp)
                                  (wpos-len wp) loc))]))

;; Tests:

(check-expect (replace-wpos grid-b (make-wpos 0 0 true 2) '(#\x #\X))
              (list (list #\x #\X) (list #\X #\Y)))
(check-expect (replace-wpos grid-a (make-wpos 0 0 true 1) '(#\X))
              (list (list #\X) (list #\X)))
(check-expect (replace-wpos grid-b (make-wpos 1 0 true 2) '(#\A #\B))
              (list (list #\A #\B) (list #\A #\B)))




;; (fit? word cell) determines if word can fit inside cell
;; fit?: (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RAT##")) false)

(define (fit? word cell)
  (cond
    [(empty? word) true]
    [(not(= (length word) (length cell))) false]
    [(or (char=? (first word) (first cell)) (char=? (first cell) #\#))
     (fit? (rest word) (rest cell))]
    [else false]))

;; Tests:
(check-expect (fit? (string->list "STARWARS") (string->list "########")) true)
(check-expect (fit? (string->list "X") (string->list "######")) false)
(check-expect (fit? (string->list "STARWARS") (string->list "##")) false)


;;(best-wpos grid lo-wpos wp) takes in a lo-wpos (ls of all wpos's)and produces
;; the most ideal Wpos that can fit in grid. Also takes wp into consideration.
;;best-wpos: (listof (listof Char)) (listof WPos) WPos -> WPos
;;Examples:
(check-expect (best-wpos grid-abc empty (make-wpos 0 0 true 2))
              (make-wpos 0 0 true 2))
(check-expect (best-wpos grid-abc empty (make-wpos 0 0 false 2))
              (make-wpos 0 0 false 2))

(define (best-wpos grid lo-wpos wp) 
  (cond
    [(empty? lo-wpos) wp]
    [(> (length (filter (λ(z) (not(char=? z #\#)))
                        (extract-wpos grid (first lo-wpos))))
        (length (filter (λ(z) (not(char=? z #\#))) (extract-wpos grid wp))))
     (best-wpos grid (rest lo-wpos) (first lo-wpos))]
    [else (best-wpos grid (rest lo-wpos) wp)]))

;;Tests:
(check-expect (best-wpos grid-abc empty (make-wpos 0 0 true 2))
              (make-wpos 0 0 true 2))
(check-expect (best-wpos grid-abc empty (make-wpos 0 0 false 2))
              (make-wpos 0 0 false 2))


;; (next-possible-states grid lo-wpos best wp words) computes next posssible
;; states of grid by taking lo-wpos, best, wp and words into account.
;; next-possible-states: Grid (listof WPos) WPos (listof String)
;;                      (listof String) -> (listof State)
;; Examples:

(define (next-possible-states grid lo-wpos best wp words) 
  (cond
    [(empty? words) empty]
    [(fit? (string->list(first words)) (extract-wpos grid best))
     (cons (make-state (replace-wpos grid best (string->list (first words)))
                       (remove best lo-wpos) (remove (first words) wp))
           (next-possible-states grid lo-wpos best wp (rest words)))]
    [else (next-possible-states grid lo-wpos best wp (rest words))]))


;;Tests:

;; (neighbours given-state) produces list of all next
;; possible states of given-state.
;;neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))

(define (neighbours given-state)
  (next-possible-states
   (state-grid given-state)
   (state-positions given-state)
   
   (best-wpos (state-grid given-state)
              (state-positions given-state)
              (first (state-positions given-state)))
   
   (state-words given-state) (state-words given-state)))


;; Tests:
(check-expect (neighbours(initial-state (list (list "...#" "...#") empty)))
              '())
(check-expect (neighbours(initial-state(list (list "..##""....") empty)))empty)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

;(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt
