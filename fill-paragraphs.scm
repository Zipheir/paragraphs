(module fill-paragraphs (fill-best fill-optimal)

(import scheme
        (chicken base)
        (chicken string)
        (srfi 1)
        (srfi 41)
        (srfi 152)
        (prefix (only utf8 string-length) utf8:)
        )

(define default-threshold 65)
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

(define (string-join-reverse-stream ss-rev sep)
  (letrec
   ((join
     (lambda (acc ss)
       (if (stream-null? (stream-cdr ss))
           (string-concatenate (cons (stream-car ss) acc))
           (join (cons sep (cons (stream-car ss) acc))
                 (stream-cdr ss))))))

    (join '() ss-rev)))

(define-record-type node
  (make-node lines demerits rest)
  node?
  (lines    node-lines)    ; (stream (stream string))
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

;; Similar to a zipper on string streams, with the total width of the
;; "top" segment stored instead of a focus.
(define-record-type split
  (make-split top top-width bottom)
  split?  ; soup
  (top       split-top)
  (top-width split-top-width)
  (bottom    split-bottom))

;; (stream string) -> (stream split)
(define (splits ss)
  (letrec
   ((build
     (stream-lambda (top-w top bot)
       (let ((sp (make-split top top-w bot)))
         (if (stream-null? bot)
             (stream sp)
             (let* ((s (stream-car bot))
                    (dw (utf8:string-length s)))
               (stream-cons sp
                            (build (+ top-w dw)
                                   (stream-cons s top)
                                   (stream-cdr bot)))))))))

    (build 0 stream-null ss)))

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
              (if (stream-null? (split-top spl))
                  exts  ; ignore empty lines
                  (let ((d (demerits goal (top-full-width spl))))
                    (if (or (stream-null? (split-bottom spl))
                            (< d threshold))
                        (stream-cons (build-node d spl) exts)
                        exts)))))))))

      (exts-stream (splits-from nd)))))

(define (node-active? nd) (stream-pair? (node-rest nd)))

(define (prune act)
  (stream-partition node-active? act))

(define (solution-nodes words threshold goal-width)
    (let loop ((active (stream (initial-node words)))
               (inactive stream-null))
      (if (stream-null? active)
          inactive
          (let*-values (((ns)
                         (stream-concat
                          (stream-map (cut extend threshold goal-width <>)
                                      active)))
                        ((as* ins*) (prune ns)))
            (loop as* (stream-append ins* inactive))))))

(define (%solution select words threshold goal-width)
  (format-solution
   (node-lines
    (select
     (solution-nodes words threshold goal-width)))))

;; (stream string) -> string
;; Main interface (exported). Fills *words* using the "optimal fit"
;; algorithm.
(define (fill-optimal words . opt)
  (let-optionals opt ((threshold default-threshold)
                      (goal-width default-goal-width))
    (%solution optimum-fit words threshold goal-width)))

;; (stream string) -> string
;; Main interface (exported). Fills *words* using the "best fit"
;; algorithm.
(define (fill-best words . opt)
  (let-optionals opt ((threshold default-threshold)
                      (goal-width default-goal-width))
    (%solution best-fit words threshold goal-width)))

(define (optimum-fit fills)
  (stream-minimum-by node-demerits fills))

;; Cheap strategy: Just take the first solution.
(define (best-fit fills)
  (stream-car fills))

;;;; Joining up the results

;; format-solution : (stream (stream string)) -> string
;;
;; The input stream and each sub-stream are in reverse order.
(define (format-solution lines-rev)
  (fold-right
   (lambda (line para)
     (string-append (string-join-reverse-stream line " ")
                    "\n"
                    para))
   ""
   (stream->list (stream-reverse lines-rev))))

)
