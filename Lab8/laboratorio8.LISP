;;; Construtor
(defun cria-no (vasilhas &optional (g 0) (heuristica 0) (pai nil))
  "Cria um nó com estado das vasilhas, profundidade, heurística e ponteiro para o nó pai."
  (list vasilhas g heuristica pai))

;;; Funções Seletores
(defun no-estado (no)
  "Retorna o estado do nó."
  (first no)
)

(defun no-profundidade (no)
  "Retorna a profundidade do nó."
  (second no)
)

(defun no-heuristica (no)
  "Retorna o valor da heurística do nó."
  (third no)
)

(defun no-pai (no)
  "Retorna o nó pai."
  (fourth no)
)

(defun no-custo (no)
  "Calcula o custo do nó (profundidade + heurística)."
  (+ (no-profundidade no) (no-heuristica no))
)

;; Teste no Listener
(let ( (no (cria-no '(2 2) 3 2 nil)) )
  (format t "Estado: ~a~%" (no-estado no))
  (format t "Profundidade: ~a~%" (no-profundidade no))
  (format t "Heurística: ~a~%" (no-heuristica no))
  (format t "Custo: ~a~%" (no-custo no))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica (estado)
  "Calcula o valor da heurística com base no estado das vasilhas."
  (let ((va (first estado))
        (vb (second estado)))
    (cond
      ((or (= va 1) (= vb 1)) 0)
      ((= va vb) 1)
      (t 2)
    )
  )
)

(defun novo-sucessor (no operador)
  "Gera um novo sucessor com a heurística calculada."
  (let ( (novo-estado (funcall operador (no-estado no))) )
    (when novo-estado
      (cria-no novo-estado
               (1+ (no-profundidade no))
               (heuristica novo-estado)
               no)
      )
  )
)

;; Teste no Listener
(format t "Heurística de (1 3): ~a~%" (heuristica '(1 3))) ; Resultado esperado: 0
(format t "Heurística de (2 2): ~a~%" (heuristica '(2 2))) ; Resultado esperado: 1
(format t "Heurística de (3 4): ~a~%" (heuristica '(3 4))) ; Resultado esperado: 2

(defun ordenar-nos (lista-nos)
  "Ordena uma lista de nós pelo custo (profundidade + heurística)."
  (sort lista-nos #'(lambda (n1 n2)
                      ( < (no-custo n1) (no-custo n2)) )
  )
)

(defun escreve-no (no)
  "Escreve no terminal o estado, profundidade, heurística e custo do nó."
  (format t "| Estado: ~a | Profundidade: ~a | Heurística: ~a | Custo: ~a | Pai: ~a~%"
          (no-estado no)
          (no-profundidade no)
          (no-heuristica no)
          (no-custo no)
          (if (no-pai no) (no-estado (no-pai no)) "NIL")
  )
)

(defun escreve-lista-nos (lista)
  "Escreve no terminal uma lista de nós."
  (dolist (no lista)
    (escreve-no no))
)

;; Teste no Listener
(let ( (no1 (cria-no '(2 2) 1 2))
       (no2 (cria-no '(1 3) 0 0))
       (no3 (cria-no '(3 4) 2 2)) )
  ( escreve-lista-nos (ordenar-nos (list no1 no2 no3)) )
)

(defun colocar-sucessores-em-abertos (abertos sucessores)
  "Combina sucessores com a lista de abertos e ordena pelo custo total."
  (ordenar-nos (append abertos sucessores))
)

;; Teste no Listener
(let ( (abertos (list (cria-no '(2 2) 1 2)
                      (cria-no '(3 4) 2 2)) )
      ( sucessores (list (cria-no '(1 3) 0 0)
                        (cria-no '(0 1) 1 1)) )
     )
  ( escreve-lista-nos (colocar-sucessores-em-abertos abertos sucessores) )
)

;; Implementação do algoritmo A*
(defun no-solucaop (no)
  "Verifica se o estado do nó é uma solução."
  (let ( (estado (no-estado no)) )
    ( or (= (first estado) 1)
        (= (second estado) 1) )
  )
)

(defun sucessores (no operadores funcao-heuristica)
  "Gera a lista de sucessores para um nó, calculando a heurística."
  (remove-if
   #'null
   (mapcar
    #'(lambda (operador)
        (let ( (novo-estado (funcall operador (no-estado no))) )
          (when novo-estado
            (cria-no novo-estado
                     ( 1+ (no-profundidade no) )
                     (funcall funcao-heuristica novo-estado)
                     no)
           )
         )
       )
    operadores)
   )
)

(defun a* (abertos fechados funcao-objetivo funcao-sucessores operadores funcao-heuristica nos-gerados nos-expandidos)
  "Algoritmo A* implementado de forma recursiva."
  (cond
    ;; Caso base: lista de abertos vazia, falha.
    ((null abertos)
     (format t "Falha: solução não encontrada.~%")
     nil)

    ;; Verifica o primeiro nó em abertos.
    (t
     (let* ((n (first abertos))
            (resto-abertos (rest abertos))
            (nos-sucessores (funcall funcao-sucessores n operadores funcao-heuristica))
            (nos-filtrados
             ;; Remove sucessores redundantes.
             (remove-if
              #'(lambda (sucessor)
                  (some #'(lambda (f)
                            (and (equal (no-estado sucessor) (no-estado f))
                                 (<= (no-custo f) (no-custo sucessor))))
                        fechados))
              nos-sucessores)))

       ;; Atualiza contadores.
       (incf nos-expandidos)
       (incf nos-gerados (length nos-sucessores))

       ;; Verifica se há um nó objetivo nos sucessores.
       (let ((objetivo (find-if funcao-objetivo nos-sucessores)))
         (if objetivo
             ;; Caso objetivo encontrado.
             (progn
               (format t "Solução encontrada!~%")
               (list :solucao objetivo
                     :nos-gerados nos-gerados
                     :nos-expandidos nos-expandidos))

           ;; Continua recursivamente com os novos abertos e fechados.
           (a*
            (colocar-sucessores-em-abertos resto-abertos nos-filtrados)
            (cons n fechados)
            funcao-objetivo
            funcao-sucessores
            operadores
            funcao-heuristica
            nos-gerados
            nos-expandidos)))))))

;; Teste no Listener
