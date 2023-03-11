(import scheme
        (srfi 41)
        io-stream
        fill-paragraphs)

(define-stream (stream-intersperse obj stream)
  (stream-match stream
    (() stream-null)
    ((x) stream)
    ((x . stream*)
     (stream-cons x
                  (stream-cons obj
                               (stream-intersperse obj stream*))))))

(let ((paragraphs (port->paragraph-stream (current-input-port))))
  (stream-for-each
   display
   (stream-intersperse "\n"
                       (stream-map fill-optimal paragraphs))))
