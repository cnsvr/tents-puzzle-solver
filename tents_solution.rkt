#lang scheme
;2016400348

; HELPER FUNCTIONS

;This function takes 2 parameters. 
;First parameter will be a function and the second parameter will be a list. 
;This function would apply the given function to each element of the list in order. 
;If the given function returns something that is not false, 
;then this function should return the return value as well. 
;If the given func- tion returns false for each and every element of the list 
;then this function should return false as well.

(define (RETURN-FIRST-NOT-FALSE predicate list )
  (if (null? list)
  #f
  (if (eq? (predicate (car list)) #f )
    (RETURN-FIRST-NOT-FALSE predicate (cdr list))
    (predicate (car list))
  )))

;This function takes 2 parameters. 
;Both parameters will be lists with 2 integers representing a position on a grid. 
;This function should return true if these positions are adjacent, including diago- nals, or the same position. 
;Otherwise it should return false.

(define (ADJACENT p1 p2)
  (if (<= (+ (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))) 2)
    #t
    ;else
    #f  
  )
)

;This function takes a single parameter. 
;It will be a list with 2 integers representing a position on a grid. 
;This function should return a list of neighboring positions, horizontally and vertically. 
;The function can return the neighbors in any order. 
;Even if a position is out of bounds in the context of the tents puzzle this function must put that position in the list that this function returns.

(define (NEIGHBOR-LIST list)
  (cons   
  ; LEFT
  (cons (- (car list) 1) (cons (cadr list) '()))     
  (cons
  ; RIGHT 
  (cons (+ (car list) 1) (cons (cadr list) '()))  
  (cons
  ; UP
  (cons (car list) (cons (- (cadr list) 1) '()))   
  (cons
  ; DOWN
  (cons (car list) (cons (+ (cadr list) 1) '())) '()))))
)

;This function takes 2 parameters. 
;A list containing 2 integers and a list of lists that are containing 2 integers.
;The first parameter represents a position on a grid and each element on the second list also represents a position. 
;This function should return true if the position represented 
;by the first parameter is adjacent to at least one of the positions in the given list (using the same definition for adjacency from the ADJACENT function). 
;Otherwise it should return false


(define (ADJACENT-WITH-LIST list listOfList)
  (if (null? listOfList)
  #f
  (if (ADJACENT list (car listOfList))
  #t
  (ADJACENT-WITH-LIST list (cdr listOfList))
  ))
)

;This function takes 3 parameters. 
;A list, an integer indicating a position on this list and a value. 
;It should return a list that is identical to the given list, except the value in the given position should be replaced with the given value. 
;The given position will always be positive and not bigger than the length of the given list.

(define (REPLACE-NTH list pos val)
  (if (eq? pos 1)
  (cons val (cdr list))
  (cons (car list) (REPLACE-NTH (cdr list) (- pos 1) val ))
  )
)

;This function takes 2 parameters.
;A point of grid and size of puzzle (NxN)
;It returns true if given point is in bound of puzzle, otherwise false.

(define (IS-IN-BOUND point SIZE ) 
  (if (and (and (>= (car point) 1) (<= (car point) SIZE)) (and (>= (cadr point) 1) (<= (cadr point) SIZE)))
  #t
  #f
  )
)

(define (MEMBER list val)
  (if (null? list)
    #f
    (if (eq? (car list) val)
      #t
      (or #f (MEMBER (cdr list) val))
    )
  )
)


(define (LISTMEMBEROFLIST list val)
  (if (null? list)
  #f
  (if (and (eq? (car (car list)) (car val)) (eq? (cadr (car list)) (cadr val)))
  #t
  (or #f (LISTMEMBEROFLIST (cdr list) val))
  ))  
)

(define (ISMEMBEROFLIST list otherList)
  (if (null? list)
    #f
    (if (LISTMEMBEROFLIST otherList (car list))
      #t
      (or #f (ISMEMBEROFLIST (cdr list) otherList))
    )
  )
)

(define (FIND-POSITION val list index)
  (if (null? list)
  '()
  (if (eq? (car list) val)
  (cons index (FIND-POSITION val (cdr list) (+ 1 index)))
  (FIND-POSITION val (cdr list) (+ 1 index))
  )
  )
)

(define (DECREMENT-ELEMENT-OF-LIST list pos index) 
  (if (eq? pos index)
    (cons (- (car list) 1) (cdr list))
    (cons (car list) (DECREMENT-ELEMENT-OF-LIST (cdr list) (- pos 1) index))
  )
)

(define (FILTER-LIST-WITH-ROW-INDEX predicate list treeList rowIndex)
  (if (null? list)
    '()
    (if (or (not (predicate (car (car list)) rowIndex)) (LISTMEMBEROFLIST treeList (car list)))
      (cons (car list) (FILTER-LIST-WITH-ROW-INDEX predicate (cdr list) treeList rowIndex))
      (FILTER-LIST-WITH-ROW-INDEX predicate (cdr list) treeList rowIndex)
    )
  )
)

(define (FILTER-LIST-WITH-COL-INDEX predicate list treeList colIndex)
  (if (null? list)
    '()
    (if (or (not (predicate (cadr (car list)) colIndex)) (LISTMEMBEROFLIST treeList (car list)))
      (cons (car list) (FILTER-LIST-WITH-COL-INDEX predicate (cdr list) treeList colIndex))
      (FILTER-LIST-WITH-COL-INDEX predicate (cdr list) treeList colIndex)
    )
  )
)

(define CHECKINDEX (lambda (val index)
  (if (eq? val index)
    #t 
    #f
  )
))

(define (FILTER-GRID-ROWS rowList colList gridList treeList)
  (if (null? rowList)
    (if (null? colList)
      gridList
      (FILTER-GRID-ROWS rowList (cdr colList) (FILTER-LIST-WITH-COL-INDEX CHECKINDEX gridList treeList (car colList)) treeList)

    )
    (FILTER-GRID-ROWS (cdr rowList) colList (FILTER-LIST-WITH-ROW-INDEX CHECKINDEX gridList treeList (car rowList)) treeList)
  )
)

(define (FILTER-GRID-IMPOSSIBLE-POINTS gridList treeList)
  (if (null? gridList)
    '()
    (if (ISMEMBEROFLIST (NEIGHBOR-LIST (car gridList)) treeList)
      (cons (car gridList) (FILTER-GRID-IMPOSSIBLE-POINTS (cdr gridList) treeList))
      (FILTER-GRID-IMPOSSIBLE-POINTS (cdr gridList) treeList)
    )
  )
)

(define ZERO-INDEX (lambda (rowList) (if (MEMBER rowList 0) (FIND-POSITION 0 rowList 1) #f)) )





(define (CREATE-ROW row col initial)
  (if (eq? col initial)
    (cons (cons row (cons initial '())) '())
    (cons (cons row (cons initial '())) (CREATE-ROW row col (+ 1 initial)))
  )
)


(define (FULL-GRID row col initial) 
  (if (eq? row initial)
    (append (CREATE-ROW row col 1) '())
    (append (FULL-GRID (- row 1) col initial) (CREATE-ROW row col 1) )
  )
)


(define gridList (FULL-GRID 8 8 1))

(define treeList '( (1 3) (1 6) (2 4) (2 8) (3 2) (3 5) (4 3) (4 6) (5 7) (6 4) (7 1) (8 3) ) ) 

(define rowList '(2 1 3 1 3 1 1 0))

(define colList '(1 2 1 3 0 2 1 2))

(define rowZeroList (ZERO-INDEX rowList))

(define colZeroList (ZERO-INDEX colList))


(define filterGrid (FILTER-GRID-ROWS rowZeroList colZeroList gridList treeList))
;filterGrid
'(empty cells) (FILTER-GRID-IMPOSSIBLE-POINTS filterGrid treeList)
'(tree cells) treeList

; IS IT OKEY?


;gridList











