(define-module (render))
(export ->svg)

(use-modules (cairo)
             (srfi srfi-1))

;; Do we need to define this?
(define pi 3.141592654)

(define cell-size 12)

(define (matches-wall wall cell)
  (logtest cell wall))

(define north #b1000)
(define south #b0100)
(define west  #b0010)
(define east  #b0001)

(define* (->svg file-name rows cols cells optional: colors root-row root-col)
  (define width (* cols cell-size))
  (define height (* rows cell-size))
  (define surface (cairo-svg-surface-create width height file-name))
  (define context (cairo-create surface))

  (define (fill-square context color)
    (let ([source (cairo-get-source context)])
      (cairo-move-to context 0.5 0)
      (cairo-line-to context 1 0)
      (cairo-line-to context 1 1)
      (cairo-line-to context 0 1)
      (cairo-line-to context 0 0)
      (cairo-line-to context 0.5 0)
      (cairo-close-path context)

      (if color
        (cairo-set-source-rgb context color (+ 0.5 (* 0.5 color)) color)
        (cairo-set-source-rgb context 1 1 1))

      (cairo-fill context)

      (cairo-set-source context source)))

  (define (draw-square context cell)
    (let ([matrix (cairo-get-matrix context)])
      (cairo-move-to context 0.5 0)
      (if (matches-wall north cell) (cairo-line-to context 1 0) (cairo-move-to context 1 0))
      (if (matches-wall east cell)  (cairo-line-to context 1 1) (cairo-move-to context 1 1))
      (if (matches-wall south cell) (cairo-line-to context 0 1) (cairo-move-to context 0 1))
      (if (matches-wall west cell)  (cairo-line-to context 0 0) (cairo-move-to context 0 0))
      (if (matches-wall north cell) (cairo-line-to context 0.5 0) (cairo-move-to context 0.5 0))

      ;; Reset scale, so the lines are not distorted by
      ;; the width being different from the height
      (cairo-identity-matrix context)

      (cairo-set-line-join context 'bevel)
      (cairo-set-line-cap context 'round)
      (cairo-set-line-width context 4)
      (cairo-stroke context)

      ;; Restore the previous state, so we can continue
      ;; drawing in the same scale
      (cairo-set-matrix context matrix)))

  (define (draw-grid context draw-proc cells)
    (let loop ([r 0] [c 0]
               [cells cells])
      (cond [(null? cells) #f]
            [(> r rows) #f]
            [(>= c cols)
             (cairo-translate context (- cols) 1)
             (loop (1+ r) 0 cells)]
            [else
             (draw-proc context (car cells))
             (cairo-translate context 1 0)
             (loop r (1+ c) (cdr cells))])))

  (cairo-scale context (/ width cols) (/ height rows))

  ;; White background
  (cairo-set-source-rgb context 1 1 1)
  (cairo-paint context)

  (let ([matrix (cairo-get-matrix context)])
    ;; Fill grid
    (if colors (draw-grid context fill-square colors))

    ;; Draw grid
    (cairo-set-matrix context matrix)
    (cairo-set-source-rgb context 0 0 0)
    (draw-grid context draw-square cells)
    (cairo-set-matrix context matrix))

  (when (and root-col root-row)
    (cairo-arc context (+ root-col 0.5) (+ root-row 0.5) 0.25 0 (* 2 pi))
    (cairo-fill context))

  (cairo-surface-destroy surface)
  (cairo-destroy context))
