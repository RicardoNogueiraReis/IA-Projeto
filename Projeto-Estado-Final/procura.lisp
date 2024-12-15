;;; procura.lisp

#| Tipo abstrato de dados ====================== |#

(defun cria-no (tabuleiro &optional (g 0) (heuristica 0) (pai nil))
  "Cria um nó com estado do tabuleiro, profundidade (g), heurística e ponteiro para o nó pai."
  (list tabuleiro g heuristica pai)
)

(defun no-teste-vasilhas ()
"Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
   (cria-no '(2 0) 0 (heuristica '(2 0)))
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

#||#

;; TODO: Alterar para a heuristica do projeto
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

#| Operadores  ================================= |#

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


;; Função aux para operador
(defun percorrer-posicoes (coordenadas tabuleiro)
"coordenadas de ma lista de posição (linha coluna) a percorrer para incrementar o número de "
  (cond
    ((null tabuleiro) nil)
    ((null coordenadas) tabuleiro)
      (t 
      (let
        (
          (novo-tabuleiro (incrementar-posicao (first (car coordenadas)) (second (car coordenadas)) tabuleiro))
        )
          (cond
            ( (and
                (= (length coordenadas) 1)
                (or
                  (= (celula (first (car coordenadas)) (second (car coordenadas)) novo-tabuleiro) 1) 
                  (= (celula (first (car coordenadas)) (second (car coordenadas)) novo-tabuleiro) 3)
                  (= (celula (first (car coordenadas)) (second (car coordenadas)) novo-tabuleiro) 5)
                )
              )
              (substituir (first (car coordenadas)) (second (car coordenadas)) novo-tabuleiro)
            )
              (t (percorrer-posicoes (cdr coordenadas) novo-tabuleiro))
          )
      )
     )
  )
)


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

;; TODO: 
(defun no-solucaop (no)
  (cond
   (
    (or
      (= (vasilha-a-conteudo no) 1)
      (= (vasilha-b-conteudo no) 1)
    )
    t

   )
   (
    t
    nil
   )
  )
)


;;; sucessores
;; teste: (novo-sucessor (no-teste) '(1 1))
;; resultado: ???

(defun novo-sucessor (no operador)
  "Gera um novo sucessor com a heurística calculada."
  (let
    (
      (novo-estado (funcall operador (no-estado no)))
    )
    (cond
      ( (null novo-estado)
        nil
      )
      (t
        (cria-no novo-estado
               (1+ (no-profundidade no))
               (heuristica novo-estado)
               no)
      )
      
    )
  )
)


;; teste: (sucessores (no-teste) (operadores) 'bfs)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))

(defun sucessores-aux (no operadores algoritmo &optional (dfs-profundidade 10))
  (cond 
   (
    (null operadores)
     nil
   )
   (
    (and (equal algoritmo "dfs") (= (second no) dfs-profundidade))  ;; (second no) => ((2 2) «0» nil) «Profundidade»
     nil
   )
   (
    t
    (cons (novo-sucessor no (car operadores)) (sucessores no (cdr operadores) "dfs" dfs-profundidade))
   )
  )
 
)

(defun sucessores (no operadores algoritmo &optional (dfs-profundidade 10))
  (remove-if 'null (sucessores-aux no operadores algoritmo dfs-profundidade))
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

(defun bfs (funcaoObjetivo funcaoSucessores operadores &optional (abertos nil) (fechados nil))
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
          (nos-sucessores (remove nil (funcall funcaoSucessores n operadores "bfs")) )

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
            (bfs funcaoObjetivo funcaoSucessores operadores novos-abertos novos-fechados)
          )
        )
       )
     )
  )
)


;; procura na profundidade
;; teste: (dfs 'no-solucaop 'sucessores (operadores) 10 (list (no-teste)))
;; resultado: ((3 1) 1 ((2 2) 0 NIL))

(defun dfs (funcaoObjetivo funcaoSucessores operadores profundidadeMax &optional (abertos nil) (fechados nil))
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
          (nos-sucessores (remove nil (funcall funcaoSucessores n operadores "dfs" profundidadeMax)))

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
            (dfs funcaoObjetivo funcaoSucessores operadores profundidadeMax novos-abertos novos-fechados)
          )
        )
       )
     )
  )

)

;;; A*

(defun sucessores-heuristica (no operadores funcao-heuristica)
  "Gera a lista de sucessores para um nó, aplicando corretamente a heurística e gravando a referência ao nó pai."
  (cond
    ((null operadores) nil)
    (t
     (let ((novo-estado (funcall (car operadores) (no-estado no))))
       (if novo-estado
           ;; Cria o nó sucessor com heurística e pai
           (cons
            (cria-no
             novo-estado
             (1+ (no-profundidade no))
             (funcall funcao-heuristica novo-estado)
             no) ;; Referência ao nó pai
            (sucessores-heuristica no (cdr operadores) funcao-heuristica))
           ;; Continua com o próximo operador
           (sucessores-heuristica no (cdr operadores) funcao-heuristica))
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

(defun a* (abertos fechados funcaoObjetivo funcao-sucessores operadores funcao-heuristica &optional (nos-gerados 0) (nos-expandidos 0))
  "Algoritmo A* implementado de forma recursiva, garantindo a gravação da heurística e do pai."
  (cond
    ((null abertos) nil)
    (t
     (let* ((n (car abertos))
            ;; Gerar sucessores com heurística e pai
            (nos-sucessores (remove nil (funcall funcao-sucessores n operadores funcao-heuristica)))

            ;; Remover redundâncias e adicionar novos sucessores a abertos
            (nos-sucessores-filtrados-fechados (remove-if
                             #'(lambda (sucessor) (no-redundante-p sucessor fechados))
                             nos-sucessores)
            )
            (nos-sucessores-filtrados (remove-if
                             #'(lambda (sucessor) (no-redundante-p sucessor abertos))
                             nos-sucessores-filtrados-fechados)
            )
            (novos-abertos (colocar-sucessores-em-abertos (cdr abertos) nos-sucessores-filtrados))

            (novos-fechados (cons n fechados))
            (nos-expandidos-novos (1+ nos-expandidos))
            (nos-gerados-novos (+ nos-gerados (length nos-sucessores)))
            (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo))
           )

       (cond
         (no-objetivo ;; Solução encontrada
          no-objetivo)
         (t ;; Continuar a procura
          (a* novos-abertos
              novos-fechados
              funcaoObjetivo
              funcao-sucessores
              operadores
              funcao-heuristica
              nos-gerados-novos
              nos-expandidos-novos)
         )
       )
     )
    )
  )
)