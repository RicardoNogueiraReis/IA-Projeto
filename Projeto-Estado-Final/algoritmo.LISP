;;; algoritmo.lisp

(defun valor (estado)
  ;; Função de avaliação do estado do tabuleiro
  (let ((pecas-jogador1 (reduce #'+ (first estado)))
        (pecas-jogador2 (reduce #'+ (second estado))))
    (- pecas-jogador1 pecas-jogador2)))

(defun minimax (estado profundidade alfa beta maximizando-jogador)
  (if (or (tabuleiro-vaziop estado) (<= profundidade 0))
      (valor estado)
      (if maximizando-jogador
          (minimax-max estado profundidade alfa beta)
          (minimax-min estado profundidade alfa beta))))

(defun minimax-max (estado profundidade alfa beta)
  (minimax-max-aux estado profundidade alfa beta -999999 (jogadas-validas estado)))

(defun minimax-max-aux (estado profundidade alfa beta valor-max jogadas)
  (if (null jogadas)
      valor-max
      (let* ((jogada (car jogadas))
             (novo-estado (aplicar-jogada estado jogada))
             (novo-valor (minimax novo-estado (1- profundidade) alfa beta nil)))
        (if (>= (max valor-max novo-valor) beta)
            (max valor-max novo-valor)
            (minimax-max-aux estado profundidade (max alfa novo-valor) beta (max valor-max novo-valor) (cdr jogadas))))))

(defun minimax-min (estado profundidade alfa beta)
  (minimax-min-aux estado profundidade alfa beta 999999 (jogadas-validas estado)))

(defun minimax-min-aux (estado profundidade alfa beta valor-min jogadas)
  (if (null jogadas)
      valor-min
      (let* ((jogada (car jogadas))
             (novo-estado (aplicar-jogada estado jogada))
             (novo-valor (minimax novo-estado (1- profundidade) alfa beta t)))
        (if (<= (min valor-min novo-valor) alfa)
            (min valor-min novo-valor)
            (minimax-min-aux estado profundidade alfa (min beta novo-valor) (min valor-min novo-valor) (cdr jogadas))))))

(defun jogar (estado tempo)
  (jogar-aux estado (jogadas-validas estado) -999999 nil))

(defun jogar-aux (estado jogadas melhor-valor melhor-jogada)
  (if (null jogadas)
      (list melhor-jogada (aplicar-jogada estado melhor-jogada))
      (let* ((jogada (car jogadas))
             (novo-estado (aplicar-jogada estado jogada))
             (valor-jogada (minimax novo-estado 4 -999999 999999 nil)))  ;; Profundidade inicial pode ser ajustada conforme necessário
        (if (> valor-jogada melhor-valor)
            (jogar-aux estado (cdr jogadas) valor-jogada jogada)
            (jogar-aux estado (cdr jogadas) melhor-valor melhor-jogada)))))