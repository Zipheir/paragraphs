(module fill-paragraphs (node-lines solutions optimum-fit best-fit)

(import scheme
        (chicken base)
        (chicken string)
        (srfi 1)
        (srfi 41)
        (srfi 152)
        (prefix (only utf8 string-length) utf8:)
        )

(define default-threshold 100)
(define default-goal-width 70)

;;; Utility

(define (sum ks) (fold + 0 ks))

(define (stream-minimum-by proc stream)
  (let loop ((s (stream-cdr stream))
             (least (stream-car stream))
             (l-val (proc (stream-car stream))))
    (if (stream-null? s)
        least
        (let* ((x (stream-car s))
               (v (proc x))
               (s* (stream-cdr s)))
          (if (< v l-val)
              (loop s* x v)
              (loop s* least l-val))))))

(define (stream-partition pred st)
  (letrec
   ((split
     (lambda (st)
       (if (stream-null? st)
           (values stream-null stream-null)
           (let-values (((x) (stream-car st))
                        ((ins outs) (split (stream-cdr st))))
             (if (pred x)
                 (values (stream-cons x ins) outs)
                 (values ins (stream-cons x outs))))))))
    (split st)))

(define-record-type line
  (make-line words-rev width)
  line?
  (words-rev line-words-rev)
  (width line-width))

(define-record-type node
  (make-node lines demerits rest)
  node?
  (lines    node-lines)    ; stream of line structures
  (demerits node-demerits) ; total demerits of incomplete solution
  (rest     node-rest))    ; stream of remaining words

(define (initial-node words)
  (make-node stream-null 0 words))

(define (demerits goal width)
  (let ((k (+ 1 (badness goal width))))
    (expt k 2)))

(define (badness goal k)
  (abs (- k goal)))

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

;; (stream string) -> (stream split)
(define (splits ss)
  (define build
    (stream-lambda (top-w top bot)
      (let ((sp (make-split top top-w bot)))
        (if (stream-null? bot)
            (stream sp)
            (let* ((s (stream-car bot))
                   (dw (utf8:string-length s)))
              (stream-cons sp
                           (build (+ top-w dw)
                                  (stream-cons s top)
                                  (stream-cdr bot))))))))

  (build 0 stream-null ss))

;; Compute the total width of the head of a split when inter-word
;; spaces are added.
(define (top-full-width spl)
  (+ (split-top-width spl)  ; total width of words in top
     (max 0 (- (stream-length (split-top spl)) 1)))) ; inter-word spaces

(define (extend threshold goal nd)
  (let ((lines (node-lines nd))
        (demr (node-demerits nd)))
    (letrec*
     ((build-node
       (lambda (d spl)
         (make-node (stream-cons (split-top spl) lines)
                    (+ demr d)
                    (split-bottom spl))))
      (exts-stream
       (stream-lambda (spls)
         (stream-match spls
           (() stream-null)
           ((spl . rest)
            (let ((exts (exts-stream rest))) ; recur
              (if (null? (split-top spl))
                  exts  ; ignore empty lines
                  (let ((d (demerits goal (top-full-width spl))))
                    (if (or (null? (split-bottom spl))
                            (< d threshold))
                        (stream-cons (build-node d spl) exts)
                        exts)))))))))

      (exts-stream (splits-from nd)))))

(define (node-active? nd) (pair? (node-rest nd)))

(define (prune act)
  (stream-partition node-active? act))

(define (solutions words . opt)
  (let-optionals opt ((threshold default-threshold)
                      (goal-width default-goal-width))
    (let loop ((active (stream (initial-node words)))
               (inactive stream-null))
      (if (stream-null? active)
          inactive
          (let*-values (((ns)
                         (stream-concat
                          (stream-map (cut extend threshold goal-width <>)
                                      active)))
                        ((as* ins*) (prune ns)))
            (loop as* (stream-append ins* inactive)))))))

(define (optimum-fit fills)
  (stream-minimum-by node-demerits fills))

;; Cheap strategy: Just take the first solution.
(define (best-fit fills)
  (stream-car fills))

)
