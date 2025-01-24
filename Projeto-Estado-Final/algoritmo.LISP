(defun valor (estado)
  (let ((pecas-jogador1 (reduce #'+ (first estado)))
        (pecas-jogador2 (reduce #'+ (second estado)))
       )
    (- pecas-jogador2 pecas-jogador1)
  )
)

(defun minimax-iterativo (estado profundidade-max alfa beta maximizando-jogador)
  (cond 
    (
     (or (tabuleiro-vaziop estado) (<= profundidade-max 0))
     (let ( (valor (valor estado)) )
       (format t "Estado: ~a, Valor: ~a~%" estado valor)
       valor))
    (t
     (let (
           (jogadas (jogadas-validas estado
                     (if maximizando-jogador 0 1)
                    )
           )
          )
       (if maximizando-jogador
           (maximizar-valor jogadas estado profundidade-max alfa beta)
           (minimizar-valor jogadas estado profundidade-max alfa beta)
       )
     )
    )
  )
)

(defun maximizar-valor (jogadas estado profundidade-max alfa beta)
  (cond
    (
     (null jogadas) 
     alfa)
    (t
     (let* ((jogada (first jogadas))
            (resto (rest jogadas))
            (novo-estado (aplicar-jogada estado jogada))
            (novo-valor (if novo-estado (minimax-iterativo novo-estado (1- profundidade-max) alfa beta nil) alfa))
           )
       (if (>= novo-valor beta)
           beta
           (maximizar-valor resto estado profundidade-max (max alfa novo-valor) beta)
       )
     )
    )
  )
)

(defun minimizar-valor (jogadas estado profundidade-max alfa beta)
  (cond
    ((null jogadas) beta)
    (t
     (let* ((jogada (first jogadas))
            (resto (rest jogadas))
            (novo-estado (aplicar-jogada estado jogada))
            (novo-valor (if novo-estado (minimax-iterativo novo-estado (1- profundidade-max) alfa beta t) beta))
            )
       (if (<= novo-valor alfa)
           alfa
           (minimizar-valor resto estado profundidade-max alfa (min beta novo-valor))
       )
     )
    )
  )
)

(defun jogar (estado profundidade-max)
  (let ( (jogadas (jogadas-validas estado 0)) )
    (jogar-aux jogadas estado profundidade-max nil -999999)
   )
)

(defun jogar-aux (jogadas estado profundidade-max melhor-jogada melhor-valor)
  (cond
    (
     (null jogadas)
     (list melhor-jogada melhor-valor)
    )
    (t
     (let* ((jogada (first jogadas))
            (novo-estado (aplicar-jogada estado jogada))
            (valor (if novo-estado (minimax-iterativo novo-estado profundidade-max -999999 999999 nil) -999999)))
       (format t "Jogada: ~a, Valor: ~a~%" jogada valor)
       (if (> valor melhor-valor)
           (jogar-aux (rest jogadas) estado profundidade-max jogada valor)
           (jogar-aux (rest jogadas) estado profundidade-max melhor-jogada melhor-valor)
       )
     )
    )
  )
)