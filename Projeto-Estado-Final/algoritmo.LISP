(defun valor (estado)
  "Função de avaliação do estado do tabuleiro.
   Calcula a diferença entre as peças do jogador 1 e do jogador 2."
  (let ((pecas-jogador1 (reduce #'+ (first estado)))
        (pecas-jogador2 (reduce #'+ (second estado))))
    (- pecas-jogador1 pecas-jogador2)))

(defun minimax-iterativo (estado profundidade-max alfa beta maximizando-jogador)
  "Implementação do Minimax com poda Alfa-Beta."
  (cond 
    ((or (tabuleiro-vaziop estado) (<= profundidade-max 0)) (valor estado))
    (t
     (let ((jogadas (jogadas-validas estado (if maximizando-jogador 0 1))))
       (minimax-aux jogadas estado profundidade-max alfa beta maximizando-jogador)))))

(defun minimax-aux (jogadas estado profundidade-max alfa beta maximizando-jogador)
  (cond
    ((null jogadas) (if maximizando-jogador -999999 999999))
    (t
     (let* ((jogada (first jogadas))
            (resto (rest jogadas))
            (novo-estado (aplicar-jogada estado jogada)))
       (if novo-estado
           (let ((valor (minimax-iterativo novo-estado (1- profundidade-max) alfa beta (not maximizando-jogador))))
             (if maximizando-jogador
                 (let ((novo-alfa (max alfa valor)))
                   (if (>= novo-alfa beta)
                       novo-alfa
                       (minimax-aux resto estado profundidade-max novo-alfa beta maximizando-jogador)))
                 (let ((novo-beta (min beta valor)))
                   (if (>= alfa novo-beta)
                       novo-beta
                       (minimax-aux resto estado profundidade-max alfa novo-beta maximizando-jogador)))))
           (minimax-aux resto estado profundidade-max alfa beta maximizando-jogador))))))

(defun jogar (estado profundidade-max)
  "Executa o algoritmo Alfa-Beta e retorna a melhor jogada com o valor associado."
  (let ((jogadas (jogadas-validas estado 0)))
    (jogar-aux jogadas estado profundidade-max nil -999999)))

(defun jogar-aux (jogadas estado profundidade-max melhor-jogada melhor-valor)
  (cond
    ((null jogadas) (list melhor-jogada melhor-valor))
    (t
     (let* ((jogada (first jogadas))
            (novo-estado (aplicar-jogada estado jogada))
            (valor (if novo-estado (minimax-iterativo novo-estado profundidade-max -999999 999999 nil) -999999)))
       (if (> valor melhor-valor)
           (jogar-aux (rest jogadas) estado profundidade-max jogada valor)
           (jogar-aux (rest jogadas) estado profundidade-max melhor-jogada melhor-valor))))))
