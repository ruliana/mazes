(define-module (render))
(export ->svg)
(use-modules (cairo))

(define cell-size 12)

(define (matches-wall wall cell)
  (logtest cell wall))

(define north #b1000)
(define south #b0100)
(define west  #b0010)
(define east  #b0001)

(define (->svg file-name rows cols cells)
  (define width (* cols cell-size))
  (define height (* rows cell-size))
  (define surface (cairo-svg-surface-create width height file-name))
  (define context (cairo-create surface))
  (define (draw-square context cell)
    (cairo-move-to context 0.5 0)
    (if (matches-wall north cell) (cairo-line-to context 1 0) (cairo-move-to context 1 0))
    (if (matches-wall east cell)  (cairo-line-to context 1 1) (cairo-move-to context 1 1))
    (if (matches-wall south cell) (cairo-line-to context 0 1) (cairo-move-to context 0 1))
    (if (matches-wall west cell)  (cairo-line-to context 0 0) (cairo-move-to context 0 0))
    (if (matches-wall north cell) (cairo-line-to context 0.5 0) (cairo-move-to context 0.5 0))

    (let ([state (cairo-get-matrix context)])
      ;; Reset scale, so the lines are not distorted by
      ;; the width being different from the height
      (cairo-identity-matrix context)

      (cairo-set-line-join context 'bevel)
      (cairo-set-line-cap context 'round)
      (cairo-set-line-width context 4)
      (cairo-stroke context)

      ;; Restore the previous state, so we can continue
      ;; drawing in the same scale
      (cairo-set-matrix context state)))

  (cairo-scale context (/ width cols) (/ height rows))

  ;; White background
  (cairo-set-source-rgb context 1 1 1)
  (cairo-paint context)

  ;; Black pen
  (cairo-set-source-rgb context 0 0 0)

  ;; Draw grid
  (let loop ([r 0] [c 0] [cells cells])
    (cond [(null? cells) #f]
          [(> r rows) #f]
          [(>= c cols)
           (cairo-translate context (- cols) 1)
           (loop (1+ r) 0 cells)]
          [else
           (draw-square context (car cells))
           (cairo-translate context 1 0)
           (loop r (1+ c) (cdr cells))]))

  (cairo-surface-destroy surface)
  (cairo-destroy context))
