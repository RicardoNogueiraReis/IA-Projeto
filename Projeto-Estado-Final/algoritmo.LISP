;;; algoritmo.lisp

(defun valor (estado)
  ;; Função de avaliação do estado do tabuleiro
  (let ((pecas-jogador1 (reduce #'+ (first estado)))
        (pecas-jogador2 (reduce #'+ (second estado))))
    (- pecas-jogador1 pecas-jogador2)))

(defun minimax-iterativo (estado profundidade-max alfa beta maximizando-jogador)
  (let ((start-time (get-internal-real-time))
        (stack (list (list estado 0 alfa beta maximizando-jogador))))
    (loop while stack
          for (estado profundidade alfa beta maximizando-jogador) = (pop stack)
          do (progn
               ;; Verificar limite de tempo
               (when (> (- (get-internal-real-time) start-time) tempo-max)
                 (format t "Tempo excedido~%")
                 (return nil))
               (if (or (tabuleiro-vaziop estado) (>= profundidade profundidade-max))
                   (return (valor estado))
                   (if maximizando-jogador
                       (dolist (jogada (reverse (jogadas-validas estado)))
                         (let ((novo-estado (aplicar-jogada estado jogada)))
                           (if (null novo-estado)
                               (format t "Erro: estado gerado por aplicar-jogada é nulo.~%")
                             (push (list novo-estado (1+ profundidade) alfa beta (not maximizando-jogador)) stack))))
                       (dolist (jogada (reverse (jogadas-validas estado)))
                         (let ((novo-estado (aplicar-jogada estado jogada)))
                           (push (list novo-estado (1+ profundidade) alfa beta t) stack)))))))))


(defun jogar (estado tempo)
  (let ((jogadas-validas (jogadas-validas estado))
        (melhor-valor -999999)
        (melhor-jogada nil))
    (dolist (jogada jogadas-validas)
      (let ((novo-estado (aplicar-jogada estado jogada))
            (valor-jogada (minimax-iterativo novo-estado 3 -999999 999999 nil)))  ;; Profundidade inicial pode ser ajustada conforme necessário
        (when (> valor-jogada melhor-valor)
          (setf melhor-valor valor-jogada)
          (setf melhor-jogada jogada))))
    (list melhor-jogada (aplicar-jogada estado melhor-jogada))))