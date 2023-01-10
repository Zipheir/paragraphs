(import (chicken string)
        (srfi 1)
        (srfi 152)
        )

(include "pmatch.scm")

(define goal-width 25)

;;; Utility

(define (sum ks) (fold + 0 ks))

(define (minimum-by proc lis)
  (car
   (fold (lambda (x p)
           (let ((k (proc x)))
             (if (< k (cdr p))
                 (cons x k)
                 p)))
         (cons (car lis) (proc (car lis)))
         (cdr lis))))
              

(define test-text
  (let ((s #<<END
Once more. Say you are in the country; in some high land of lakes.
END
        ))
    (string-split (string-translate s "\n" " ") " ")))

(define-record-type line
  (make-line words-rev width)
  line?
  (words-rev line-words-rev)
  (width line-width))

(define-record-type node
  (make-node lines demerits rest)
  node?
  (lines    node-lines)    ; list of line structures
  (demerits node-demerits) ; total demerits of incomplete solution
  (rest     node-rest))    ; list of remaining words

(define (initial-node words)
  (make-node '() 0 words))

(define (demerits width)
  (let ((k (+ 1 (badness width))))
    (expt k 2)))

(define (badness k)
  (abs (- k goal-width)))

(define (splits-from nd)
  (splits (node-rest nd)))

;; Similar to a zipper on string lists, with the total width of the
;; "top" segment stored instead of a focus.
(define-record-type split
  (make-split top top-width bottom)
  split?  ; soup
  (top       split-top)
  (top-width split-top-width)
  (bottom    split-bottom))

;; (list string) -> (list split)
(define (splits ss)
  (define (build top-w top bot)
    (let ((sp (make-split top top-w bot)))
      (if (null? bot)
          (list sp)
          (let* ((s (car bot))
                 (dw (string-length s)))
            (cons sp
                  (build (+ top-w dw) (cons s top) (cdr bot)))))))

  (build 0 '() ss))

;; Compute the total width of the head of a split when inter-word
;; spaces are added.
(define (top-full-width spl)
  (+ (split-top-width spl)  ; total width of words in top
     (max 0 (- (length (split-top spl)) 1)))) ; inter-word spaces

(define (extend threshold nd)
  (let ((lines (node-lines nd))
        (demr (node-demerits nd))
        (candidates (splits-from nd)))
    (map (lambda (spl)
           (make-node (cons (split-top spl) lines)
                      (+ demr (demerits (top-full-width spl)))
                      (split-bottom spl)))
         (filter (lambda (spl)
                   (cond ((null? (split-bottom spl)))
                         ((null? (split-top spl)) #f)
                         (else
                          (good-enough? threshold
                                        (top-full-width spl)))))
                 candidates))))

(define (good-enough? threshold width)
  (< (demerits width) threshold))

(define (node-active? nd) (pair? (node-rest nd)))

(define (prune act)
  (let-values (((act* inact) (partition node-active? act)))
    (values act* inact)))

;; Test driver.
(define (solutions-bounded text max-iters threshold)
  (let loop ((active (list (initial-node text)))
             (inactive '())
             (k max-iters))
    (print "active: " active)
    (if (or (null? active) (zero? k))
        (list active inactive)
        (let*-values (((ns) (append-map (cut extend threshold <>) active))
                      ((as* ins*) (prune ns)))
          (loop as* (append ins* inactive) (- k 1))))))

(define (optimum-fit fills)
  (minimum-by node-demerits fills))

(define (rejoin lines)
  (string-join (map (cut string-join-reverse <> " ")
                    (reverse lines))
               "\n" 'suffix))

;; To improve.
(define (string-join-reverse ss delim)
  (string-concatenate-reverse
   (intersperse ss delim)))

;(define (intersperse xs obj)
;  (pmatch xs
;    (() '())
;    ((,x) xs)
;    ((,x . ,xs*)
;     (cons x (cons obj (intersperse xs* obj))))))
