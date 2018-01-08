;;; -*- Mode: Scheme; -*-

(define-record-type path
  (%make-path commands)
  path?
  (commands path-commands set-path-commands!))

(define-record-type tag
  (make-tag name attrs body)
  tag?
  (name tag-name set-tag-name!)
  (attrs tag-attrs set-tag-attrs!)
  (body tag-body set-tag-body!))

(define-record-type attr
  (make-attr name value)
  attr?
  (name attr-name set-attr-name!)
  (value attr-value set-attr-value!))

(define (display-attr attr)
  ;; Attr -> IO!
  (if (not (attr? attr))
      (error "DISPLAY-ATTR: Expected type 'attr', got " attr))
  (let ((name (attr-name attr))
        (value (attr-value attr)))
    (format " ~a=\"~a\"" name value)))

(define (display-tag tag)
  ;; String Tag -> IO!
  (define (display-name name closing?)
    ;; String Boolean -> IO!
    (begin
      (display #\<)
      (if closing? (display #\/))
      (display name)
      (display #\>)))
  (let ((name (tag-name tag))
        (attrs (tag-attrs tag))
        (body (tag-body tag)))
    (begin
      (display #\<)
      (display name)
      (map display-attr attrs)
      (display #\>)
      (cond ((tag? body)
             (display-tag body))
            ((list? body)
             (map display-tag body))
            (else (display body)))
      (display-name name #t))))

(define (circle x y r fill)
  ;; Int Int Int Sym -> Tag
  (let ((x* (number->string x))
        (y* (number->string y))
        (r* (number->string r))
        (fill* (symbol->string fill)))
    (make-tag 'circle (list (make-attr 'cx x*)
                            (make-attr 'cy y*)
                            (make-attr 'r r*)
                            (make-attr 'fill fill*)) "")))

(define (text x y font-size anchor fill s)
  (let ((x* (number->string x))
        (y* (number->string y))
        (font-size* (number->string font-size))
        (fill* (symbol->string fill))
        (anchor* (symbol->string anchor)))
    (make-tag 'text
              (list (make-attr 'x  x*)
                    (make-attr 'y y*)
                    (make-attr 'fill fill*)
                    (make-attr 'anchor anchor*)
                    (make-attr 'font-size font-size*))
              s)))

(define (rect w h fill)
  (let ((w* (string-append (number->string w) "%"))
        (h* (string-append (number->string h) "%"))
        (fill* (symbol->string fill)))
    (make-tag 'rect (list (make-attr 'width w*)
                          (make-attr 'height h*)
                          (make-attr 'fill fill*)) "")))

(define (make-points points)
  ;; List[Int] -> Attr
  (let* ((nums (map number->string points))
         (appended (mapconcat nums " ")))
    (make-attr 'points appended)))

(define (move-to x y)
  (let ((x* (number->string x))
        (y* (number->string y)))
    (list "M" x* y*)))

(define (line-to x y)
  (let ((x* (number->string x))
        (y* (number->string y)))
    (list "L" x* y*)))

(define (horizontal-line x)
  (let ((x* (number->string x)))
    (list "h" x*)))

(define (vertical-line x)
  (let ((x* (number->string x)))
    (list "v" x*)))

(define-syntax with-attrs
  (syntax-rules ()
    ((with-attrs ?attrs ?tag ?body)
     (make-tag (quote ?tag)
               (map (lambda (pair) (make-attr (first pair) (second pair))) ?attrs)
               ?body))))

(define-syntax make-path
  (syntax-rules ()
    ((make-path ?commands)
     (let* ((path (%make-path (list ?commands)))
            (cmds (path-commands path))
            (cmds* (flatten cmds))
            (cmds** (mapconcat cmds* " ")))
       (make-tag 'path (list (make-attr 'd cmds**)) "")))))

(define-syntax svg
  (syntax-rules ()
    ((svg)
     (make-tag 'svg (list (make-attr 'xmlns "http://www.w3.org/2000/svg")
                          (make-attr 'version "1.1")
                          (make-attr 'baseProfile "full")) ""))
    ((svg form . forms)
     (make-tag 'svg (list (make-attr "xmlns" "http://www.w3.org/2000/svg")
                          (make-attr 'version "1.1")
                          (make-attr 'baseProfile "full"))
               (begin form . forms)))))



;;; Writing to files

'(with-output-to-file "C:/Users/rml/Desktop/foo.svg"
   (lambda ()
     (display-tag
      (make-tag 'svg (list (make-attr "xmlns" "http://www.w3.org/2000/svg")
                           (make-attr "xmlns:xlink" "http://www.w3.org/1999/xlink"))
                (list (rect 100 100 'red)
                      (circle 150 100 80 'green)
                      (text 150 125 60 'middle 'white "SVG"))))))

'(with-output-to-file "C:/Users/rml/Desktop/path.svg"
   (lambda ()
     (display-tag 
      (svg (make-path (list (move-to 10 10)
                            (horizontal-line 90)
                            (vertical-line 90)
                            (horizontal-line -90)
                            (line-to 10 10)))))))

'(with-output-to-file "C:/Users/rml/Desktop/circles.svg"
   (lambda ()
     (display-tag 
      (svg (list (circle 10 10 10 'green)
                 (circle 100 100 10 'red)
                 (circle 500 500 10 'yellow))))))

'(with-output-to-file "C:/Users/rml/Desktop/lines.svg"
   (lambda ()
     (display-tag
      (svg (list (circle 700 700 40 'orange)
                 (make-path (list (move-to 500 500)
                                  (horizontal-line 100)
                                  (vertical-line 100)
                                  (horizontal-line -100)
                                  (line-to 500 500))))))))

'(with-output-to-file "C:/Users/rml/Desktop/bar.svg"
   (lambda ()
     (display-tag
      (svg
       (make-tag 'polygon
                 (list
                  (make-points '(50 160 55 180 70 180 60 190 65 205 50 195 35 205 40 190 30 180 45 180))
                  (make-attr 'stroke "green")) "")))))
