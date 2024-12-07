;;; Construtor
(defun cria-no (vasilhas &optional (g 0) (heuristica 0) (pai nil))
  "Cria um nó com estado das vasilhas, profundidade, heurística e ponteiro para o nó pai."
  (list vasilhas g heuristica pai))

;;; Funções Seletores
(defun no-estado (no)
  "Retorna o estado do nó."
  (first no))

(defun no-profundidade (no)
  "Retorna a profundidade do nó."
  (second no))

(defun no-heuristica (no)
  "Retorna o valor da heurística do nó."
  (third no))

(defun no-pai (no)
  "Retorna o nó pai."
  (fourth no))

(defun no-custo (no)
  "Calcula o custo do nó (profundidade + heurística)."
  (+ (no-profundidade no) (no-heuristica no)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica (estado)
  "Calcula o valor da heurística com base no estado das vasilhas."
  (let ((va (first estado))
        (vb (second estado)))
    (cond
      ((or (= va 1) (= vb 1)) 0)
      ((= va vb) 1)
      (t 2))))

(defun novo-sucessor (no operador)
  "Gera um novo sucessor com a heurística calculada."
  (let ((novo-estado (funcall operador (no-estado no))))
    (when novo-estado
      (cria-no novo-estado
               (1+ (no-profundidade no))
               (heuristica novo-estado)
               no))))

(defun ordenar-nos (lista-nos)
  "Ordena uma lista de nós pelo custo (profundidade + heurística)."
  (sort lista-nos #'(lambda (n1 n2)
                      (< (no-custo n1) (no-custo n2)))))
