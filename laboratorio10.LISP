(defun cria-tabuleiro ()
  (list (list nil nil nil) (list nil nil nil) (list nil nil nil)))

(defun imprime-linha (linha)
  (cond ((null linha) (format t "~%"))
        (t (format t "~a " (or (car linha) "-"))
           (imprime-linha (cdr linha)))))

(defun imprime-tabuleiro (tabuleiro)
  (unless (null tabuleiro)
    (imprime-linha (car tabuleiro))
    (imprime-tabuleiro (cdr tabuleiro))))

(defun substitui-em-tabuleiro (tabuleiro linha coluna valor)
  (mapcar (lambda (l idx)
            (if (= idx linha)
                (mapcar (lambda (c jdx)
                          (if (= jdx coluna) valor c))
                        l '(0 1 2))
                l))
          tabuleiro '(0 1 2)))

(defun jogada-humano (tabuleiro jogador)
  (format t "Jogador ~a, insira a linha e coluna (0, 1 ou 2): " jogador)
  (let ((linha (parse-integer (read-line)))
        (coluna (parse-integer (read-line))))
    (if (or (< linha 0) (> linha 2) (< coluna 0) (> coluna 2)
            (nth coluna (nth linha tabuleiro)))
        (begin
          (format t "Jogada inválida. Tente novamente.~%")
          (jogada-humano tabuleiro jogador))
        (substitui-em-tabuleiro tabuleiro linha coluna jogador))))

(defun linha-preenchida-p (linha)
  (if (null linha) t
      (and (car linha) (linha-preenchida-p (cdr linha)))))

(defun tabuleiro-preenchido-p (tabuleiro)
  (if (null tabuleiro) t
      (and (linha-preenchida-p (car tabuleiro))
           (tabuleiro-preenchido-p (cdr tabuleiro)))))

(defun verifica-linha (linha)
  (and (car linha) (every (lambda (x) (eql x (car linha))) linha)))

(defun verifica-colunas (tabuleiro col)
  (if (= col 3) nil
      (or (verifica-linha (mapcar (lambda (linha) (nth col linha)) tabuleiro))
          (verifica-colunas tabuleiro (1+ col)))))

(defun verifica-diagonais (tabuleiro)
  (or (verifica-linha (mapcar (lambda (i) (nth i (nth i tabuleiro))) '(0 1 2)))
      (verifica-linha (mapcar (lambda (i) (nth (- 2 i) (nth i tabuleiro))) '(0 1 2)))))

(defun tabuleiro-solucao (tabuleiro)
  (or (some #'verifica-linha tabuleiro)
      (verifica-colunas tabuleiro 0)
      (verifica-diagonais tabuleiro)))

(defun melhor-jogada-rec (tabuleiro i j melhor-avaliacao melhor-movimento)
  (if (= i 3)
      melhor-movimento
      (if (= j 3)
          (melhor-jogada-rec tabuleiro (1+ i) 0 melhor-avaliacao melhor-movimento)
          (if (nth j (nth i tabuleiro))
              (melhor-jogada-rec tabuleiro i (1+ j) melhor-avaliacao melhor-movimento)
              (let ((novo-tabuleiro (substitui-em-tabuleiro tabuleiro i j 'X)))
                (let ((avaliacao (minimax novo-tabuleiro 0 nil)))
                  (if (> avaliacao melhor-avaliacao)
                      (melhor-jogada-rec tabuleiro i (1+ j) avaliacao (list i j))
                      (melhor-jogada-rec tabuleiro i (1+ j) melhor-avaliacao melhor-movimento))))))))

(defun melhor-jogada (tabuleiro)
  (or (melhor-jogada-rec tabuleiro 0 0 -2 nil) '(0 0))) ;; Evita NIL

(defun jogada-computador (tabuleiro)
  (let* ((movimento (melhor-jogada tabuleiro))
         (linha (first movimento))
         (coluna (second movimento)))
    (substitui-em-tabuleiro tabuleiro linha coluna 'X)))  ;; Certifique-se de retornar o tabuleiro

(defun jogo-do-galo ()
  (let ((tabuleiro (cria-tabuleiro)))
    (jogo-do-galo-rec tabuleiro 'X)))

(defun jogo-do-galo-rec (tabuleiro jogador)
  (imprime-tabuleiro tabuleiro)
  (if (or (tabuleiro-preenchido-p tabuleiro) (tabuleiro-solucao tabuleiro))
      (if (tabuleiro-solucao tabuleiro)
          (format t "Jogador ~a venceu!~%" (if (eql jogador 'X) 'O 'X))
          (format t "Empate!~%"))
      (jogo-do-galo-rec (if (eql jogador 'X)
                            (jogada-computador tabuleiro)
                            (jogada-humano tabuleiro jogador))
                        (if (eql jogador 'X) 'O 'X))))

;; MINMAX
(defun minimax (tabuleiro profundidade maximizando-jogador)
  (let ((vencedor (tabuleiro-solucao tabuleiro)))
    (cond
     (vencedor (if (eql vencedor 'X) 1 -1))
     ((tabuleiro-preenchido-p tabuleiro) 0)
     (t
      (if maximizando-jogador
          (maximiza tabuleiro profundidade)
          (minimiza tabuleiro profundidade))))))

(defun maximiza (tabuleiro profundidade)
  (maximiza-rec tabuleiro profundidade 0 0 -2))

(defun maximiza-rec (tabuleiro profundidade i j melhor-avaliacao)
  (if (= i 3)
      melhor-avaliacao
      (if (= j 3)
          (maximiza-rec tabuleiro profundidade (1+ i) 0 melhor-avaliacao)
          (if (nth j (nth i tabuleiro))
              (maximiza-rec tabuleiro profundidade i (1+ j) melhor-avaliacao)
              (let* ((novo-tabuleiro (substitui-em-tabuleiro tabuleiro i j 'X))
                     (avaliacao (minimax novo-tabuleiro (1- profundidade) nil)))
                (maximiza-rec tabuleiro profundidade i (1+ j) (max melhor-avaliacao avaliacao)))))))

(defun minimiza (tabuleiro profundidade)
  (minimiza-rec tabuleiro profundidade 0 0 2))

(defun minimiza-rec (tabuleiro profundidade i j melhor-avaliacao)
  (if (= i 3)
      melhor-avaliacao
      (if (= j 3)
          (minimiza-rec tabuleiro profundidade (1+ i) 0 melhor-avaliacao)
          (if (nth j (nth i tabuleiro))
              (minimiza-rec tabuleiro profundidade i (1+ j) melhor-avaliacao)
              (let* ((novo-tabuleiro (substitui-em-tabuleiro tabuleiro i j 'O))
                     (avaliacao (minimax novo-tabuleiro (1- profundidade) t)))
                (minimiza-rec tabuleiro profundidade i (1+ j) (min melhor-avaliacao avaliacao)))))))

(defun teste-minimax ()
  (let* ((tabuleiro (cria-tabuleiro))
         (tabuleiro1 (substitui-em-tabuleiro tabuleiro 0 0 'X))
         (tabuleiro2 (substitui-em-tabuleiro tabuleiro1 1 1 'O))
         (tabuleiro3 (substitui-em-tabuleiro tabuleiro2 0 1 'X)))
    (imprime-tabuleiro tabuleiro3)
    ;; Computador faz a próxima jogada
    (let ((tabuleiro4 (jogada-computador tabuleiro3)))
      (imprime-tabuleiro tabuleiro4))))


;; (teste-minimax)