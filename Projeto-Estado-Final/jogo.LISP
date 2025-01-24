;;; algoritmo.lisp

(defparameter *profundidade-maxima* 3)  ;; Diminuímos a profundidade máxima para 3

(defun valor (estado)
  ;; Função de avaliação do estado do tabuleiro
  (let ((pecas-jogador1 (reduce #'+ (first estado)))
        (pecas-jogador2 (reduce #'+ (second estado))))
    (- pecas-jogador1 pecas-jogador2)))

(defun minimax (estado profundidade alfa beta maximizando-jogador)
  (if (or (tabuleiro-vaziop estado) (>= profundidade *profundidade-maxima*))
      (valor estado)
      (if maximizando-jogador
          (minimax-max estado profundidade alfa beta)
          (minimax-min estado profundidade alfa beta))))

(defun minimax-max (estado profundidade alfa beta)
  (minimax-max-aux estado (1+ profundidade) alfa beta -999999 (jogadas-validas estado)))

(defun minimax-max-aux (estado profundidade alfa beta valor-max jogadas)
  (if (null jogadas)
      valor-max
      (let* ((jogada (car jogadas))
             (novo-estado (aplicar-jogada estado jogada))
             (novo-valor (minimax novo-estado profundidade alfa beta nil)))
        (if (>= (max valor-max novo-valor) beta)
            (max valor-max novo-valor)
            (minimax-max-aux estado profundidade (max alfa novo-valor) beta (max valor-max novo-valor) (cdr jogadas))))))

(defun minimax-min (estado profundidade alfa beta)
  (minimax-min-aux estado (1+ profundidade) alfa beta 999999 (jogadas-validas estado)))

(defun minimax-min-aux (estado profundidade alfa beta valor-min jogadas)
  (if (null jogadas)
      valor-min
      (let* ((jogada (car jogadas))
             (novo-estado (aplicar-jogada estado jogada))
             (novo-valor (minimax novo-estado profundidade alfa beta t)))
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
             (valor-jogada (minimax novo-estado 1 -999999 999999 nil)))  ;; Inicia com profundidade 1
        (if (> valor-jogada melhor-valor)
            (jogar-aux estado (cdr jogadas) valor-jogada jogada)
            (jogar-aux estado (cdr jogadas) melhor-valor melhor-jogada)))))

(defun iniciar ()
  (let ((tabuleiros '(((0 0 0 0 0 2) (0 0 0 0 4 0))
                      ((2 2 2 2 2 2) (2 2 2 2 2 2))
                      ((0 3 0 3 0 3) (3 0 3 0 3 0))
                      ((1 2 3 4 5 6) (6 5 4 3 2 1))
                      ((2 4 6 8 10 12) (12 10 8 6 4 2))
                      ((48 0 0 0 0 0) (0 0 0 0 0 48))
                      ((8 8 8 8 8 8) (8 8 8 8 8 8))
                      ((1 8 8 5 8 7) (3 8 6 3 8 2)))))
    (format t "Selecione um tabuleiro inicial:~%")
    (loop for tabuleiro in tabuleiros
          for index from 1
          do (format t "~a: ~a~%" index tabuleiro))
    (let ((escolha-tabuleiro (read)))
      (if (and (integerp escolha-tabuleiro)
               (>= escolha-tabuleiro 1)
               (<= escolha-tabuleiro (length tabuleiros)))
          (let ((estado-inicial (nth (1- escolha-tabuleiro) tabuleiros)))
            (format t "Que algoritmo quer usar para procurar?~%")
            (format t "1- Procura na largura (BFS)~%")
            (format t "2- Procura na profundidade (DFS)~%")
            (format t "3- Procura do Melhor Primeiro (A*)~%")
            (format t "4- Algoritmo Alfa-Beta~%") ; Adicionada a opção Alfa-Beta
            (let ((escolha-algoritmo (read)))
              (cond
                ((= escolha-algoritmo 1)
                 (format t "Executando BFS...~%")
                 ;; Adicione a chamada para a função BFS aqui
                 )
                ((= escolha-algoritmo 2)
                 (format t "Executando DFS...~%")
                 ;; Adicione a chamada para a função DFS aqui
                 )
                ((= escolha-algoritmo 3)
                 (format t "Executando A*...~%")
                 ;; Adicione a chamada para a função A* aqui
                 )
                ((= escolha-algoritmo 4)
                 (format t "Executando Alfa-Beta...~%")
                 ;; Adicione a chamada para o algoritmo Alfa-Beta aqui
                 (jogar estado-inicial 5000)) ; Exemplo: passando 5000ms como limite de tempo
                (t
                 (format t "Opção inválida.~%")))))
          (format t "Opção de tabuleiro inválida.~%")))))