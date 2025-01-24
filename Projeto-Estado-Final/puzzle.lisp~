;;; puzzle.lisp

#| Tabuleiro =================================== |#

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '(
     (0 0 0 0 0 2)
     (0 0 0 0 4 0)
   )
)

(defun tabuleiro-vazio-teste ()
  "Retorna um tabuleiro de teste 2x6 vazio (todos os valores a 0)."
  '(
     (0 0 0 0 0 0)
     (0 0 0 0 0 0)
   )
)

#| Seletores =================================== |#

(defun linha (indice tabuleiro)
  (nth indice tabuleiro)
)

(defun celula (indiceLinha posicao tabuleiro)
  (nth posicao (linha indiceLinha tabuleiro))
)

(defun tamanho-linha (linha tabuleiro)
  (length (nth linha tabuleiro))
)

(defun linha-vazia-p (linhaTabuleiro)
  (cond
   ((null linhaTabuleiro) t)
   ((= (first linhaTabuleiro) 0) (linha-vazia-p (cdr linhaTabuleiro)))
   (t nil)))

(defun tabuleiro-vaziop (tabuleiro)
  "Recebe um tabuleiro e verifica se o mesmo se encontra vazio, ou seja, tem todas as células com o valor 0."
  (let ((linha-1 (linha-vazia-p (linha 0 tabuleiro)))
        (linha-2 (linha-vazia-p (linha 1 tabuleiro))))
    (and linha-1 linha-2)))

(defun substituir-posicao (posicao linhaTabuleiro &optional (novo-valor 0))
  "Substitui o valor de uma linha do tabuleiro por um valor a especificar."
  (cond
    ((null linhaTabuleiro) nil)
    ((> posicao 0)
     (append (list (car linhaTabuleiro)) (substituir-posicao (1- posicao) (cdr linhaTabuleiro) novo-valor)))
    (t
     (append (list novo-valor) (cdr linhaTabuleiro)))))

(defun substituir (indiceLinha posicao tabuleiro &optional (novo-valor 0))
  (cond
    ((= indiceLinha 0)
     (list (substituir-posicao posicao (linha indiceLinha tabuleiro) novo-valor) (linha 1 tabuleiro)))
    (t
     (list (linha 0 tabuleiro) (substituir-posicao posicao (linha indiceLinha tabuleiro) novo-valor)))))

(defun incrementar (posicao linhaTabuleiro)
  (cond
    ((null linhaTabuleiro) nil)
    ((> posicao 0)
     (append (list (car linhaTabuleiro)) (incrementar (1- posicao) (cdr linhaTabuleiro))))
    (t
     (append (list (1+ (car linhaTabuleiro))) (cdr linhaTabuleiro)))))

(defun incrementar-posicao (indiceLinha posicao tabuleiro)
  (cond
    ((= indiceLinha 0)
     (list (incrementar posicao (linha indiceLinha tabuleiro)) (linha 1 tabuleiro)))
    (t
     (list (linha 0 tabuleiro) (incrementar posicao (linha indiceLinha tabuleiro))))))

(defun distribuir-pecas-aux (n indiceLinha indiceColuna linhaOriginal colunaOriginal &optional (tabuleiro (tabuleiro-vazio)))
  "Distribui N peças no tabuleiro a partir da posição indicada."
  (cond
    ((<= n 0) nil)  ;; Condição de término: sem peças para distribuir
    ((not (and (>= indiceLinha 0) (< indiceLinha 2)
               (>= indiceColuna 0) (< indiceColuna (length (first tabuleiro)))))
     ;; Condição de erro: coordenadas fora do tabuleiro
     nil)
    ((and (= indiceLinha linhaOriginal) (= indiceColuna colunaOriginal))
     ;; Ignorar a posição de origem
     (distribuir-pecas-aux n (if (= indiceLinha 0) (1+ indiceLinha) (1- indiceLinha)) colunaOriginal linhaOriginal colunaOriginal tabuleiro))
    ((= indiceLinha 0)
     ;; Linha superior (sentido anti-horário)
     (if (>= indiceColuna 0)
         (cons
          (list indiceLinha indiceColuna)
          (distribuir-pecas-aux (1- n) indiceLinha (1- indiceColuna) linhaOriginal colunaOriginal tabuleiro))
         (distribuir-pecas-aux n (1+ indiceLinha) 0 linhaOriginal colunaOriginal tabuleiro)))
    ((= indiceLinha 1)
     ;; Linha inferior (sentido horário)
     (if (< indiceColuna (length (second tabuleiro)))
         (cons
          (list indiceLinha indiceColuna)
          (distribuir-pecas-aux (1- n) indiceLinha (1+ indiceColuna) linhaOriginal colunaOriginal tabuleiro))
         (distribuir-pecas-aux n (1- indiceLinha) (1- (length (first tabuleiro))) linhaOriginal colunaOriginal tabuleiro)))))

(defun distribuir-pecas (n linha coluna &optional (tabuleiro (tabuleiro-vazio)))
  (if (<= n 0)
      nil
      (distribuir-pecas-aux n linha coluna linha coluna tabuleiro)))

(defun percorrer-posicoes (coordenadas tabuleiro)
  "Percorre as coordenadas e atualiza o tabuleiro."
  (cond
    ((null coordenadas)
     tabuleiro)
    (t
     (let ((linha (first (car coordenadas)))
           (coluna (second (car coordenadas))))
       (if (and (>= linha 0) (< linha 2)  ; Certifique-se de que está dentro dos limites do tabuleiro
                (>= coluna 0) (< coluna 6))
           (percorrer-posicoes (cdr coordenadas) (incrementar-posicao linha coluna tabuleiro))
           nil)))))

;; FASE 2

(defun jogadas-validas (tabuleiro linha-jogador)
  "Retorna uma lista de jogadas válidas para o jogador atual.
   Cada jogada é representada como uma lista (linha coluna)."
  (let ((jogadas '()))
    (dotimes (coluna (length (nth linha-jogador tabuleiro)))
      (when (> (nth coluna (nth linha-jogador tabuleiro)) 0)
        (push (list linha-jogador coluna) jogadas)))
    (reverse jogadas)))

(defun aplicar-jogada (estado jogada)
  (let* ((linha (first jogada))
         (coluna (second jogada))
         (pecas (celula linha coluna estado)))
    (cond
      ((or (zerop pecas) (null estado))
       estado)
      (t
       (let ((novo-estado (percorrer-posicoes
                           (distribuir-pecas pecas linha coluna estado)
                           (substituir linha coluna estado 0))))
         (or novo-estado estado))))))

(defun testar-alfa-beta ()
  "Testa o algoritmo Alfa-Beta com um exemplo de tabuleiro."
  (let* ((tabuleiro '((0 0 2) (0 3 0)))
         (melhor-valor -999999)
         (melhor-jogada nil))
    (dolist (jogada (jogadas-validas tabuleiro))
      (let* ((novo-estado (aplicar-jogada tabuleiro jogada))
             (valor-jogada (minimax-iterativo novo-estado 3 -999999 999999 t)))
        (when (> valor-jogada melhor-valor)
          (setf melhor-valor valor-jogada)
          (setf melhor-jogada jogada))))
    (format t "Melhor jogada: ~a com valor ~a~%" melhor-jogada melhor-valor)
    (list melhor-jogada melhor-valor)))