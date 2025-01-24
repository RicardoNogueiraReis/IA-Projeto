;;; algoritmo.lisp

(defun valor (estado)
  "Função de avaliação do estado do tabuleiro.
   Calcula a diferença entre as peças do jogador 1 e do jogador 2."
  (let ((pecas-jogador1 (reduce #'+ (first estado)))
        (pecas-jogador2 (reduce #'+ (second estado))))
    (- pecas-jogador1 pecas-jogador2)))

(defun minimax-iterativo (estado profundidade-max alfa beta maximizando-jogador)
  "Implementação do Minimax com poda Alfa-Beta."
  (if (or (tabuleiro-vaziop estado) (<= profundidade-max 0))
      (valor estado)
      (let ((melhor-valor (if maximizando-jogador -999999 999999)))
        (dolist (jogada (jogadas-validas estado (if maximizando-jogador 0 1)))
          (let ((novo-estado (aplicar-jogada estado jogada)))
            (when novo-estado
              (let ((valor (minimax-iterativo novo-estado (1- profundidade-max)
                                              alfa beta (not maximizando-jogador))))
                (if maximizando-jogador
                    (setf melhor-valor (max melhor-valor valor)
                          alfa (max alfa valor))
                    (setf melhor-valor (min melhor-valor valor)
                          beta (min beta valor)))
                (when (>= alfa beta)
                  (return melhor-valor))))))
        melhor-valor)))

(defun jogar (estado profundidade-max)
  "Executa o algoritmo Alfa-Beta e retorna a melhor jogada com o valor associado."
  (let ((melhor-jogada nil)
        (melhor-valor -999999))
    (dolist (jogada (jogadas-validas estado 0))
      (let ((novo-estado (aplicar-jogada estado jogada)))
        (when novo-estado
          (let ((valor (minimax-iterativo novo-estado profundidade-max -999999 999999 nil)))
            (when (> valor melhor-valor)
              (setf melhor-valor valor
                    melhor-jogada jogada))))))
    (list melhor-jogada melhor-valor)))