(defun cria-tabuleiro ()
  (list (list nil nil nil) (list nil nil nil) (list nil nil nil)))

(defun imprime-linha (linha)
  (if (null linha)
      (format t "~%")
      (progn
        (format t "~a " 
                (if (car linha) (car linha) "-") 
        )
        (imprime-linha (cdr linha))
       )
  )
)

(defun imprime-tabuleiro (tabuleiro)
  (if (null tabuleiro)
      (format t "")
      (progn
        (imprime-linha (car tabuleiro))
        (imprime-tabuleiro (cdr tabuleiro))
     )
  )
)

(defun verifica-linha (linha)
  (and (not (null (car linha))) (every (lambda (x) (and x (equal x (car linha)))) linha)))

(defun verifica-colunas (tabuleiro col)
  (if (= col 3)
      nil
      (or (verifica-linha (mapcar (lambda (linha) (nth col linha)) tabuleiro))
          (verifica-colunas tabuleiro (1+ col)))))

(defun verifica-diagonais (tabuleiro)
  (or (verifica-linha (mapcar (lambda (i) (nth i (nth i tabuleiro))) '(0 1 2)))
      (verifica-linha (mapcar (lambda (i) (nth (- 2 i) (nth i tabuleiro))) '(0 1 2)))))

(defun tabuleiro-solucao (tabuleiro)
  (or (some #'verifica-linha tabuleiro)
      (verifica-colunas tabuleiro 0)
      (verifica-diagonais tabuleiro)
  )
)

(defun tabuleiro-solucao (tabuleiro)
  (or (some #'verifica-linha tabuleiro)
      (verifica-colunas tabuleiro 0)
      (verifica-diagonais tabuleiro)))

(defun linha-preenchida-p (linha)
  (if (null linha)
      t
      (and (car linha) (linha-preenchida-p (cdr linha)))))

(defun tabuleiro-preenchidop (tabuleiro)
  (if (null tabuleiro)
      t
      (and (linha-preenchida-p (car tabuleiro)) (tabuleiro-preenchidop (cdr tabuleiro)))))

(defun avaliar-linha (linha jogador)
  (cond
    ((and (equal (nth 0 linha) jogador) (equal (nth 1 linha) jogador) (equal (nth 2 linha) jogador)) 100)
    ((and (equal (nth 0 linha) jogador) (equal (nth 1 linha) jogador) (null (nth 2 linha))) 10)
    ((and (equal (nth 0 linha) jogador) (null (nth 1 linha)) (equal (nth 2 linha))) 10)
    ((and (null (nth 0 linha)) (equal (nth 1 linha) jogador) (equal (nth 2 linha))) 10)
    ((and (equal (nth 0 linha) jogador) (null (nth 1 linha)) (null (nth 2 linha))) 1)
    ((and (null (nth 0 linha)) (equal (nth 1 linha) jogador) (null (nth 2 linha))) 1)
    ((and (null (nth 0 linha)) (null (nth 1 linha)) (equal (nth 2 linha) jogador)) 1)
    (t 0)))

(defun avaliar-no (tabuleiro jogador)
  (let ((max 0) (min 0))
    (dolist (linha tabuleiro)
      (incf max (avaliar-linha linha 'X))
      (incf min (avaliar-linha linha 'O)))
    (dolist (coluna (loop for i from 0 to 2 collect (mapcar (lambda (linha) (nth i linha)) tabuleiro)))
      (incf max (avaliar-linha coluna 'X))
      (incf min (avaliar-linha coluna 'O)))
    (dolist (diagonal (list (mapcar (lambda (i) (nth i (nth i tabuleiro))) '(0 1 2))
                            (mapcar (lambda (i) (nth (- 2 i) (nth i tabuleiro))) '(0 1 2))))
      (incf max (avaliar-linha diagonal 'X))
      (incf min (avaliar-linha diagonal 'O)))
    (if (equal jogador 'X)
        (- max min)
        (- min max))))

(defun alfabeta (tabuleiro profundidade alfa beta maximizando-jogador)
  (let ((vencedor (tabuleiro-solucao tabuleiro)))
    (cond
      (vencedor (if (equal vencedor 'X) 100 -100))
      ((tabuleiro-preenchidop tabuleiro) 0)
      ((= profundidade 0) (avaliar-no tabuleiro (if maximizando-jogador 'X 'O)))
      (t
       (if maximizando-jogador
           (alfabeta-max tabuleiro profundidade alfa beta)
           (alfabeta-min tabuleiro profundidade alfa beta))))))

(defun alfabeta-max (tabuleiro profundidade alfa beta)
  (alfabeta-max-rec tabuleiro profundidade alfa beta 0 0 -1000))

(defun alfabeta-max-rec (tabuleiro profundidade alfa beta i j max-avaliacao)
  (cond
    ((= i 3) max-avaliacao)
    ((= j 3) (alfabeta-max-rec tabuleiro profundidade alfa beta (1+ i) 0 max-avaliacao))
    ((nth j (nth i tabuleiro)) (alfabeta-max-rec tabuleiro profundidade alfa beta i (1+ j) max-avaliacao))
    (t
     (setf (nth j (nth i tabuleiro)) 'X)
     (let ((avaliacao (alfabeta tabuleiro (1- profundidade) alfa beta nil)))
       (setf max-avaliacao (max max-avaliacao avaliacao))
       (setf alfa (max alfa avaliacao)))
     (setf (nth j (nth i tabuleiro)) nil)
     (if (>= alfa beta)
         max-avaliacao
         (alfabeta-max-rec tabuleiro profundidade alfa beta i (1+ j) max-avaliacao)))))

(defun alfabeta-min (tabuleiro profundidade alfa beta)
  (alfabeta-min-rec tabuleiro profundidade alfa beta 0 0 1000))

(defun alfabeta-min-rec (tabuleiro profundidade alfa beta i j min-avaliacao)
  (cond
    ((= i 3) min-avaliacao)
    ((= j 3) (alfabeta-min-rec tabuleiro profundidade alfa beta (1+ i) 0 min-avaliacao))
    ((nth j (nth i tabuleiro)) (alfabeta-min-rec tabuleiro profundidade alfa beta i (1+ j) min-avaliacao))
    (t
     (setf (nth j (nth i tabuleiro)) 'O)
     (let ((avaliacao (alfabeta tabuleiro (1- profundidade) alfa beta t)))
       (setf min-avaliacao (min min-avaliacao avaliacao))
       (setf beta (min beta avaliacao)))
     (setf (nth j (nth i tabuleiro)) nil)
     (if (<= alfa beta)
         min-avaliacao
         (alfabeta-min-rec tabuleiro profundidade alfa beta i (1+ j) min-avaliacao)))))

(defun melhor-jogada (tabuleiro)
  (melhor-jogada-rec tabuleiro 0 0 -1000 nil))

(defun melhor-jogada-rec (tabuleiro i j melhor-avaliacao melhor-movimento)
  (cond
    ((= i 3) melhor-movimento)
    ((= j 3) (melhor-jogada-rec tabuleiro (1+ i) 0 melhor-avaliacao melhor-movimento))
    ((nth j (nth i tabuleiro)) (melhor-jogada-rec tabuleiro i (1+ j) melhor-avaliacao melhor-movimento))
    (t
     (setf (nth j (nth i tabuleiro)) 'X)
     (let ((avaliacao (alfabeta tabuleiro 3 -1000 1000 nil)))
       (if (> avaliacao melhor-avaliacao)
           (setf melhor-avaliacao avaliacao
                 melhor-movimento (list i j))))
     (setf (nth j (nth i tabuleiro)) nil)
     (melhor-jogada-rec tabuleiro i (1+ j) melhor-avaliacao melhor-movimento))))

(defun jogada-computador (tabuleiro)
  (let ((movimento (melhor-jogada tabuleiro)))
    (setf (nth (second movimento) (nth (first movimento) tabuleiro)) 'X))
  tabuleiro)

(defun jogo-do-galo ()
  (let ((tabuleiro (cria-tabuleiro)))
    (jogo-do-galo-rec tabuleiro 'X)))

(defun jogo-do-galo-rec (tabuleiro jogador)
  (imprime-tabuleiro tabuleiro)
  (if (or (tabuleiro-preenchidop tabuleiro) (tabuleiro-solucao tabuleiro))
      (if (tabuleiro-solucao tabuleiro)
          (format t "Jogador ~a venceu!~%" (if (eql jogador 'X) 'O 'X))
          (format t "Empate!~%"))
      (if (eql jogador 'X)
          (jogo-do-galo-rec (jogada-computador tabuleiro) 'O)
          (jogo-do-galo-rec (jogada-humano tabuleiro jogador) 'X))))

(defun teste-alfabeta ()
  (let ((tabuleiro (cria-tabuleiro)))
    ;; Simula algumas jogadas
    (setf (nth 0 (nth 0 tabuleiro)) 'X)
    (setf (nth 1 (nth 1 tabuleiro)) 'O)
    (setf (nth 1 (nth 0 tabuleiro)) 'X)
    (imprime-tabuleiro tabuleiro)
    ;; Computador faz a próxima jogada
    (setf tabuleiro (jogada-computador tabuleiro))
    (imprime-tabuleiro tabuleiro)))