;;; procura.lisp

#| Tipo abstrato de dados ====================== |#

(defun cria-no (tabuleiro &optional (g 0) (h 0) (pai nil))
  "Cria um nó com estado do tabuleiro, profundidade (g), heurística e ponteiro para o nó pai."
  (list tabuleiro g h pai)
)

(defun no-teste ()
  (cria-no (tabuleiro-teste))
)

;;; Funções Seletores
(defun no-estado (no)
  "Retorna o estado do nó (tabuleiro)."
  (first no)
)

(defun no-profundidade (no)
  "Retorna a profundidade do nó (g)."
  (second no)
)

(defun no-heuristica (no)
  "Retorna o valor da heurística do nó."
  (third no)
)

(defun no-custo (no)
  "Calcula o custo do nó (profundidade + heurística)."
  (+ (no-profundidade no) (no-heuristica no))
)

(defun no-pai (no)
  "Retorna o nó pai."
  (fourth no)
)

#| Output - escrita do estado do problema ====== |#

(defun escrever-no (no &optional (g t))
"Permite escrever um no, por defeito no ecra."
  (format g "~A" no))

 
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
  (cond
    ( (null lista)
      nil
    )
    ( t
      (escreve-no (car lista))
      (escreve-lista-nos (cdr lista))
    )
  )
)

#| Operador  ================================= |#

(defun operador (indiceLinha indiceColuna &optional (tabuleiro (tabuleiro-teste)))
  (cond
    ( (or (null indiceLinha) (null indiceColuna))
      nil
    )
    ( t
      (percorrer-posicoes
        (distribuir-pecas (celula indiceLinha indiceColuna tabuleiro)
                          indiceLinha
                          indiceColuna
                          tabuleiro)
        (substituir
          indiceLinha
          indiceColuna
          tabuleiro 0)
      )
    )
  )
)


(defun jogadas ()
  '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (1 0) (1 1) (1 2) (1 3) (1 4) (1 5))
)

#| outros  ================================= |#

;; no-solucaop
;; Exemplo: (no-solucaop (cria-no (tabuleiro-vazio-teste))) ;; T
;; Exemplo: (no-solucaop (cria-no (tabuleiro-teste)))       ;; NIL

(defun no-solucaop (no)
  (tabuleiro-vaziop (no-estado no))
)


;;; sucessores
;; teste: (novo-sucessor (no-teste) '(1 1))
;; resultado: ???

(defun novo-sucessor (no jogadas)
  "Gera um novo sucessor com a heurística calculada."
  (cond
   ( (null no)
     nil
   )
   (t
      (funcall #'cria-no
               (operador (first jogadas) (second jogadas) (no-estado no)) 
               (1+ (no-profundidade no))
               (heuristica (no-estado no))
               no  
      )
    )
      
   )
)


;; teste: (sucessores (no-teste) (operadores) 'bfs)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))



(defun sucessores (no jogadas algoritmo &optional (dfs-profundidade 10))
  (cond
    ((null jogadas) nil)
    ( (and (equal algoritmo "dfs") (>= (no-profundidade no) dfs-profundidade)) nil)
    ( t
      (remove-if 'null
                 (cons (novo-sucessor no (car jogadas))
                       (sucessores no (cdr jogadas) algoritmo dfs-profundidade)
                 )
      )
    )
  )
  
)

;; Funções Auxiliares ao BFS

(defun abertos-bfs (abertos nos-sucessores fechados)
"Função responsável por adicionar nos sucessores na lista de abertos de acordo com o algoritmo de procura em largura.
Verifica se já existe um dos na lista de abertos e na lista de fechados."
  (cond
     ( (null nos-sucessores) abertos)
     (
       (and
         (null (no-existe-p (car nos-sucessores) abertos "bfs"))
         (null (no-existe-p (car nos-sucessores) fechados "bfs"))
       )
       (abertos-bfs
         (append abertos (list (car nos-sucessores)))
         (cdr nos-sucessores)
         fechados
       )
      
     )
     (t (abertos-bfs abertos (cdr nos-sucessores) fechados))
   )
)

(defun abertos-dfs (abertos nos-sucessores fechados)
  "Fun responsável por adicionar nos sucessores na lista de abertos de acordo com o algoritmo de procura em profundidade.
  Verifica se já xiste um dos na lista de abertos e na lista de fechados."
  (cond
    ( (null nos-sucessores) abertos)
    (
      (and
        (null (no-existe-p (car nos-sucessores) abertos "dfs"))
        (null (no-existe-p (car nos-sucessores) fechados "dfs"))
      )
      (abertos-dfs
        (append (list (car nos-sucessores)) abertos)
        (cdr nos-sucessores)
        fechados
      )
    )
    (t (abertos-dfs abertos (cdr nos-sucessores) fechados))
  )
)


(defun no-existe-p (no lista-nos algoritmo)
  (cond
    ((null lista-nos) nil)
    (
     (and (equal (no-estado no) (no-estado (car lista-nos))) (equal algoritmo "bfs"))
     t
    )
    (
     (and (equal (no-estado no) (no-estado (car lista-nos))) (equal algoritmo "dfs"))
      (cond
       ((> (no-profundidade no) (no-profundidade (car lista-nos))) t)
       (t nil)
      )
    )
    (t (no-existe-p no (cdr lista-nos) algoritmo))
   )
)


(defun retorna-no-objetivo (lista-sucessores funcaoObjetivo)
  (cond
   ( (null lista-sucessores)
     nil
   )
   ( (null (funcall funcaoObjetivo (car lista-sucessores)))
     (retorna-no-objetivo (cdr lista-sucessores) funcaoObjetivo)
   )
   ( t
     (car lista-sucessores)
   )
  )

)

#| Algoritmos =====================|#
;; procura na largura
;; teste: (bfs 'no-solucaop 'sucessores (jogadas) (list (no-teste)) nil)

;; resultado: ((3 1) 1 ((2 2) 0 NIL))

(defun bfs (funcaoObjetivo funcaoSucessores jogadas &optional (abertos nil) (fechados nil))
  (cond
     (
       (null abertos)
       nil
     )
     (t
       (let*
        (
          (n (car abertos))

           ; Expande o nó n, Colocar os sucessores no fim de ABERTOS, 
           ; colocando os ponteiros para n. (novo-sucessor)
          (nos-sucessores (remove nil (funcall funcaoSucessores n jogadas "bfs")))

          ;; (a (print nos-sucessores))
           ; Remove o primeiro nó de ABERTOS e coloca os sucessores no fim da lista de abertos
          (novos-abertos (remove nil (abertos-bfs (cdr abertos) nos-sucessores fechados)))

           ; e coloca-o em FECHADOS
          (novos-fechados (cons n fechados))

           ; verifica se existe um nó objetivo
          (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo))
        )
        (cond
          (
            (not (null no-objetivo))
            no-objetivo
          )
          (
            t
            (bfs funcaoObjetivo funcaoSucessores jogadas novos-abertos novos-fechados)
          )
        )
       )
     )
  )
)


;; procura na profundidade
;; teste: (dfs 'no-solucaop 'sucessores (jogadas) 10 (list (no-teste)))
;; resultado: ((3 1) 1 ((2 2) 0 NIL))

(defun dfs (funcaoObjetivo funcaoSucessores jogadas profundidadeMax &optional (abertos nil) (fechados nil))
  (cond
     (
       (null abertos)
       nil
     )
     (t
       (let*
        (
          (n (car abertos))

           ; Expande o nó, colocar os sucessores no fim de ABERTOS, 
           ; colocando os ponteiros para n. (novo-sucessor)
          (nos-sucessores (remove nil (funcall funcaoSucessores n jogadas "dfs" profundidadeMax)))

           ;; (a (print nos-sucessores))
           ; Remove o primeiro nó de ABERTOS e coloca os sucessores no fim da lista de abertos
          (novos-abertos (remove nil (abertos-dfs (cdr abertos) nos-sucessores fechados)))

           ; e coloca-o em FECHADOS
          (novos-fechados (cons n fechados))

           ; verifica se existe um nó objetivo
          (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo))
        )
        (cond
          (
            (not (null no-objetivo))
            no-objetivo
          )
          (
            t
            (dfs funcaoObjetivo funcaoSucessores jogadas profundidadeMax novos-abertos novos-fechados)
          )
        )
       )
     )
  )

)

;;; A*

(defun novo-sucessor-heuristica (no jogada)
  "Gera um novo sucessor com a heurística calculada."
  (cond
   ( (null no)
     nil
   )
   (t
      (funcall #'cria-no
               (operador (first jogada) (second jogada) (no-estado no))
               (1+ (no-profundidade no))
               (heuristica (no-estado no))
               no
      )
    )
  )
)

(defun sucessores-heuristica (no jogadas funcao-heuristica heuristica-no-inicial)
  "Gera a lista de sucessores para um nó, aplicando corretamente a heurística e gravando a referência ao nó pai."
  (cond
    ((null jogadas) nil)
    (t
      (remove-if 'null
                 (cons (novo-sucessor-heuristica no (car jogadas))
                       (sucessores-heuristica no (cdr jogadas) funcao-heuristica heuristica-no-inicial)
                 )
      )
    )
  )
)

(defun no-redundante-p (sucessor lista)
  "Verifica se o sucessor já existe na lista de abertos/fechados."
  (cond
    ( (null lista) nil)
    ( (and
        (equal (no-estado sucessor) (no-estado (car lista)))
        (<= (no-custo (car lista)) (no-custo sucessor))
      )
      t
    )
    (t (no-redundante-p sucessor (cdr lista)))
  )
)


(defun ordenar-nos (lista-nos)
  "Ordena uma lista de nós pelo custo (profundidade + heurística)."
  (sort lista-nos #'(lambda (n1 n2)
                      ( < (no-custo n1) (no-custo n2)) )
  )
)

(defun colocar-sucessores-em-abertos (abertos sucessores)
  "Combina sucessores com a lista de abertos e ordena pelo custo total."
  (ordenar-nos (append abertos sucessores))
)

#| Heuristica |#
;; (format t "~a~%" lista-jogadas)

(defun o-aux (linha)
  (let
    ((pecas (car linha)))
    
    (cond
      ( (null linha) 0)
      ( t
        (+ pecas (o-aux (cdr linha)))
      )
    )
  )
)

;; O - número de peças a capturar no tabuleiro 
(defun o (no-estado) ;; todo: fazer com o let
  (+ (o-aux (first no-estado)) (o-aux (second no-estado)) )
)

;; Número de peças a capturar, peças do tabuleiro inicial - peças atuais


;; C - número de peças já capturadas no tabuleiro 
(defun c (estado-no valor-heuristico-no-inicial)
  (-
    valor-heuristico-no-inicial
    (o estado-no)
  )
)

(defun heuristica (no-estado &optional (heuristica-no-inicial 0))
   (+ (o no-estado) (c no-estado heuristica-no-inicial))
)

(defun a* (funcaoObjetivo funcao-sucessores jogadas funcao-heuristica
           &optional (abertos (list (no-teste))) (fechados nil) (nos-gerados 0) (nos-expandidos 0)
                     (tabuleiro-inicial (no-estado (car abertos)))
          )
  "Algoritmo A* implementado de forma recursiva, garantindo a gravaÃ§Ã£o da heurÃ­stica e do pai."
  (cond
    ;; Caso base: lista de abertos vazia
    ((null abertos) nil)

    (t
     (let*
       (
         (n (car abertos))  ;; Pega o nó atual para expandir
         ;; Incrementa o contador de nós expandidos
         (nos-expandidos-novos (1+ nos-expandidos))

         ;; Gerar sucessores do nó atual
         (nos-sucessores (remove nil (funcall funcao-sucessores n jogadas funcao-heuristica tabuleiro-inicial)))

         ;; Filtra sucessores redundantes
         (nos-filtrados
           (remove-if #'(lambda (sucessor) (no-redundante-p sucessor fechados)) nos-sucessores))

         ;; Incrementa o contador de nós gerados
         (nos-gerados-novos (+ nos-gerados (length nos-filtrados))) ;; Incrementa com base nos sucessores válidos

         ;; Adiciona sucessores a "abertos"
         (novos-abertos (colocar-sucessores-em-abertos (cdr abertos) nos-filtrados))
         (novos-fechados (cons n fechados)) ;; Atualiza lista de fechados

         ;; Verifica se algum dos sucessores é uma solução
         (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo)) ;; Verifica se algum sucessor é objetivo
       )

       ;; Verifica se o nó atual é solução
       (cond
         ( (not (null no-objetivo))
            no-objetivo
         )
         (t
           (a* funcaoObjetivo
               funcao-sucessores
               jogadas
               funcao-heuristica
               novos-abertos
               novos-fechados
               nos-gerados-novos
               nos-expandidos-novos
               tabuleiro-inicial
           )
         )
       )
      )
    )
  )
)


;; (a* #'no-solucaop #'sucessores-heuristica (jogadas) #'heuristica (list (no-teste)))