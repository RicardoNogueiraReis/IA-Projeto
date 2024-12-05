;;;; laboratorio7.lisp
;;;; Ficha de LaboratÃ³rio nÂº7 - Apoio ao 1Âº Projeto
;;;; Autor: 


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '(
     (1 2 3 4 5 6)
     (6 5 4 3 2 1)
   )
)


;;; Exercicios

;; (linha 0 (tabuleiro-vazio)) => (0 0 0 0 0 0)
(defun linha (indice tabuleiro)
  (nth indice tabuleiro)
)


;; (celula 0 1 (tabuleiro-teste))
;;                y           x
(defun celula (indiceLinha posicao tabuleiro)
  (nth posicao (linha indiceLinha tabuleiro))
)

(defun tamanho-linha (linha tabuleiro)
  (length (nth linha tabuleiro))
)

;; Função auxiliar a tabuleiro-vaziop
(defun linha-vazia-p (linhaTabuleiro)
  (cond
   (
     (null linhaTabuleiro)
     t
   )
   (
     (= (first linhaTabuleiro) 0)
     (linha-vazia-p (cdr linhaTabuleiro))
   )
   (
     t
     nil
   )
  )
)

(defun tabuleiro-vaziop (tabuleiro)
"Recebe um tabuleiro e verifica se o mesmo se encontra vazio, ou seja, tem todas as células com o valor 0."
  (let
   (
     (linha-1 (linha-vazia-p (linha 0 tabuleiro)))
     (linha-2 (linha-vazia-p (linha 1 tabuleiro)))
   )

   (and linha-1 linha-2)
  )

)

;; (substituir-posicao 0 (linha 0 (tabuleiro-vazio)) 8)
(defun substituir-posicao (posicao linhaTabuleiro &optional (novo-valor 0))
"Substitui o valor de uma linha do tabuleiro por um valor a especificar."
  (cond
    (
      (null linhaTabuleiro)
      nil
    )
    (
      (> posicao 0)
      (append (list (car linhaTabuleiro)) (substituir-posicao (1- posicao) (cdr linhaTabuleiro) novo-valor))
    )
    (
      t
      (append (list novo-valor) (cdr linhaTabuleiro))
    )
  )
)


;;                      y         x
(defun substituir (indiceLinha posicao tabuleiro &optional (novo-valor 0))
  (cond
    ( (= indiceLinha 0)
      (list (substituir-posicao posicao (linha indiceLinha tabuleiro) novo-valor) (linha 1 tabuleiro))
    )
    ( t
      (list (linha 0 tabuleiro) (substituir-posicao posicao (linha indiceLinha tabuleiro) novo-valor))
    )
  )
)

;; aux da incrementar-posicao
(defun incrementar (posicao linhaTabuleiro)
    (cond
    (
      (null linhaTabuleiro)
      nil
    )
    (
      (> posicao 0)
      (append (list (car linhaTabuleiro)) (incrementar (1- posicao) (cdr linhaTabuleiro)))
      )
    (
      t
      (append (list (1+ (car linhaTabuleiro))) (cdr linhaTabuleiro))
    )
  )
)

(defun incrementar-posicao (indiceLinha posicao tabuleiro)
  (cond
    ( (= indiceLinha 0)
      (list (incrementar posicao (linha indiceLinha tabuleiro)) (linha 1 tabuleiro))
    )
    ( t
      (list (linha 0 tabuleiro) (incrementar posicao (linha indiceLinha tabuleiro)))
    )
  )

)


(defun distribuir-pecas-aux (n indiceLinha indiceColuna linhaOriginal colunaOriginal &optional (tabuleiro (tabuleiro-vazio)))
  (cond
     ((<= n 0) nil)
     ( (and 
         (= indiceLinha 0)
         (> indiceColuna 0)
         (< indiceColuna (length (first tabuleiro)))
       ) 
       (cond
         ( (and
              (= indiceLinha linhaOriginal)
              (= indiceColuna colunaOriginal))

            (distribuir-pecas-aux n indiceLinha (1- indiceColuna) linhaOriginal colunaOriginal)
         )
         (t

          (cons
            (list indiceLinha indiceColuna)
            (distribuir-pecas-aux (1- n) indiceLinha (1- indiceColuna) linhaOriginal colunaOriginal)
          )
         )
       )
     )
     ( (and
         (= indiceLinha 0)
         (= indiceColuna 0)
       )
       (cond
         ( (and 
             (= indiceLinha linhaOriginal)
             (= indiceColuna colunaOriginal)
           )
           
           (distribuir-pecas-aux n (1+ indiceLinha) indiceColuna linhaOriginal colunaOriginal)
         )

         (t
           (cons
             (list indiceLinha indiceColuna)
             (distribuir-pecas-aux (1- n) (1+ indiceLinha) indiceColuna linhaOriginal colunaOriginal)
           )
         )
       )
     )
	 
     ( (and
         (= indiceLinha 1)
         (>= indiceColuna 0)
         (< indiceColuna (1- (length (second tabuleiro))))
       )
       (cond
         ( (and 
             (= indiceLinha linhaOriginal)
             (= indiceColuna colunaOriginal)
           )

           (distribuir-pecas-aux n indiceLinha (1+ indiceColuna) linhaOriginal colunaOriginal)
         )
         (t
           (cons
             (list indiceLinha indiceColuna)
             (distribuir-pecas-aux (1- n) indiceLinha (1+ indiceColuna) linhaOriginal colunaOriginal)
           )
         )
       )
     )
     ( (and
         (= indiceLinha 1)
         (= indiceColuna (1- (length (second tabuleiro))))
       )
       (cond
         ( (and 
             (= indiceLinha linhaOriginal)
             (= indiceColuna colunaOriginal)
           )

           (distribuir-pecas-aux n (1- indiceLinha) indiceColuna linhaOriginal colunaOriginal)
         )
         (t
           (cons
             (list indiceLinha indiceColuna)
             (distribuir-pecas-aux (1- n) (1- indiceLinha) indiceColuna linhaOriginal colunaOriginal)
           )
         )
       )
     )
   )
)

(defun distribuir-pecas (n indiceLinha indiceColuna &optional (tabuleiro (tabuleiro-vazio)))
  (distribuir-pecas-aux n indiceLinha indiceColuna indiceLinha indiceColuna tabuleiro)
)

(defun operador (indiceLinha indiceColuna &optional (tabuleiro (tabuleiro-teste)))
  (cond
     ((or (null indiceLinha) (null indiceColuna)) nil)
     (t
       (incrementar
         (distribuir-pecas (celula indiceLinha indiceColuna tabuleiro) indiceLinha indiceColuna tabuleiro) tabuleiro))
  )
)

