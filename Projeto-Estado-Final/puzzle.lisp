;;; puzzle.lisp

#| Tabuleiro =================================== |#

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  '(
     (0 0 0 0 0 2)
     (0 0 0 0 4 0)
   )
)

(defun tabuleiro-vazio-teste ()
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
  (let ((linha-1 (linha-vazia-p (linha 0 tabuleiro)))
        (linha-2 (linha-vazia-p (linha 1 tabuleiro))))
    (and linha-1 linha-2)))

(defun substituir-posicao (posicao linhaTabuleiro &optional (novo-valor 0))
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
  (cond
    ((<= n 0) nil)
    ((not (and (>= indiceLinha 0) (< indiceLinha 2)
               (>= indiceColuna 0) (< indiceColuna (length (first tabuleiro)))))
     nil)
    ((and (= indiceLinha linhaOriginal) (= indiceColuna colunaOriginal))
     (distribuir-pecas-aux n (if (= indiceLinha 0) (1+ indiceLinha) (1- indiceLinha)) colunaOriginal linhaOriginal colunaOriginal tabuleiro))
    ((= indiceLinha 0)
     (if (>= indiceColuna 0)
         (cons
          (list indiceLinha indiceColuna)
          (distribuir-pecas-aux (1- n) indiceLinha (1- indiceColuna) linhaOriginal colunaOriginal tabuleiro))
         (distribuir-pecas-aux n (1+ indiceLinha) 0 linhaOriginal colunaOriginal tabuleiro)))
    ((= indiceLinha 1)
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
  (cond
    ((null coordenadas)
     tabuleiro)
    (t
     (let ((linha (first (car coordenadas)))
           (coluna (second (car coordenadas))))
       (if (and (>= linha 0) (< linha 2)
                (>= coluna 0) (< coluna 6))
           (percorrer-posicoes (cdr coordenadas) (incrementar-posicao linha coluna tabuleiro))
           nil)))))

;; FASE 2

(defun jogadas-validas (tabuleiro linha-jogador)
  (labels ( (validar-coluna (colunas linha coluna-atual)
             (cond
               (
                (null colunas)
                nil
               )
               ( (> 
                  (first colunas)
                  0
                 )
                (cons (list linha coluna-atual)
                      ( validar-coluna (rest colunas) linha (1+ coluna-atual) )
                )
               )
               (t 
                (validar-coluna (rest colunas) linha (1+ coluna-atual))
               )
              )
            )
           )
    ( validar-coluna (nth linha-jogador tabuleiro) linha-jogador 0 )
  )
)

(defun aplicar-jogada (estado jogada)
  (let* ((linha (first jogada))
         (coluna (second jogada))
         (pecas (celula linha coluna estado)))
    (if (or 
         (zerop pecas)
         (null estado)
        )
        estado
        (let (
              (novo-estado (percorrer-posicoes
                            (distribuir-pecas pecas linha coluna estado)
                            (substituir linha coluna estado 0)
                           )
              )
             )
         novo-estado
        )
    )
  )
)

(defun testar-alfa-beta ()
  (let (
        ( tabuleiro '((0 0 2) (0 3 0)) )
       )
    (labels ((avaliar-jogadas (jogadas estado melhor-jogada melhor-valor)
               (if (null jogadas)
                   (list melhor-jogada melhor-valor)
                   (let* ((jogada (first jogadas))
                          (novo-estado (aplicar-jogada estado jogada))
                          (valor-jogada (minimax-iterativo novo-estado 3 -999999 999999 t)))
                     (if (> valor-jogada melhor-valor)
                         (avaliar-jogadas (rest jogadas) estado jogada valor-jogada)
                         (avaliar-jogadas (rest jogadas) estado melhor-jogada melhor-valor))))))
      (let ((resultado (avaliar-jogadas (jogadas-validas tabuleiro 0) tabuleiro nil -999999)))
        (format t "Melhor jogada: ~a com valor ~a~%" (first resultado) (second resultado))
        resultado
      )
    )
  )
)