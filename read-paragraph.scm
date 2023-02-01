;(import (rnrs io ports (6))
;        (srfi :41 streams))
(import (only (srfi 1) remove)
        (rename (chicken io) (read-line get-line))
        (srfi 41)
        (only (srfi 152) string-split))

(define (string-null? s) (equal? s ""))

;; TODO: Write a real implementation.
(define (string-split-stream s delim)
  (list->stream (string-split s delim)))

;; Could be more efficient, but it's harder than you think.
(define (stream-span pred sm)
  (values (stream-take-while pred sm) (stream-drop-while pred sm)))

(define-stream (stream-break-on pred sm)
  (let-values (((in out) (stream-span (lambda (x) (not (pred x))) sm)))
    (if (stream-null? out)
        (stream sm)
        (stream-cons in
                     (stream-break-on pred
                                      (stream-drop-while pred out))))))

(define-stream (line-stream port)
  (let ((line (get-line port)))
    (if (eof-object? line)
        stream-null
        (stream-cons line (line-stream port)))))

(define-stream (words line)
  (stream-filter (complement string-null?)
                 (string-split-stream line " ")))

(define-stream (paragraphs lns)
  (stream-map
   stream-concat
   (stream-break-on
    stream-null?
    (stream-map words lns))))
