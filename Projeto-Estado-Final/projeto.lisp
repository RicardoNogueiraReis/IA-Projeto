;; Projeto.lisp

(load "procura.lisp")
(load "puzzle.lisp")

#| Input |#

;; Ler tabuleiros do ficheiro problemas.dat
;; Exemplo: (ler-problemas "problemas.dat")

(defun ler-problemas-recursivo (stream)
  "Lê recursivamente os problemas de um ficheiro aberto e devolve uma lista de tabuleiros."
  (let
    ((problema (read stream nil nil)))

    (cond
     ( (not (null problema))
      (cons problema (ler-problemas-recursivo stream))
     )
     ( t
       nil
     )
    )
  )
)

(defun ler-problemas (ficheiro)
  "Lê todos os problemas do ficheiro .dat utilizando a recursão."
  (with-open-file (stream ficheiro :direction :input)
    (ler-problemas-recursivo stream)
  )
)

#| Iniciar |#


(defun iniciar ()
  "Permite iniciar o programa, lendo o tabuleiro, o algoritmo e executando a procura."
  (let*
    (
      (tabuleiros (ler-problemas "problemas.dat"))
      (tabuleiro (escolher-tabuleiro tabuleiros))
      (algoritmo (ler-algoritmo))
      (profundidade (if (eql algoritmo 'dfs) (ler-profundidade) nil))
    )
    (executar-procura algoritmo tabuleiro profundidade)
  )
)

(defun mostrar-tabuleiros (tabuleiros)
  "Mostra os tabuleiros numerados."
  (loop for tabuleiro in tabuleiros
        for index from 0
        do (format t "~d: ~a~%" (1+ index) tabuleiro))
)

(defun escolher-tabuleiro (tabuleiros)
  "Mostra os tabuleiros e permite ao utilizador escolher um."
  (mostrar-tabuleiros tabuleiros)

  (let ((escolha (read)))


    (nth escolha tabuleiros)
  )
)

(defun ler-algoritmo ()
  "Permite escolher o algoritmo de procura a utilizar."
  (progn

    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura (BFS)~%")
    (format t "2- Procura na profundidade (DFS)~%")
    (format t "3- Procura do Melhor Primeiro (A*)~%")

    (let ((resposta (read)))
      (cond
        ((= resposta 1) 'bfs)
        ((= resposta 1) 'dfs)
        ((= resposta 1) 'a*)
      )
    )
  )
)

(defun ler-profundidade ()
  "Pergunta a profundidade limite para DFS."
  (format t "Qual a profundidade limite para o DFS?~%")
  (read)
)

(defun executar-procura (algoritmo tabuleiro profundidade)
  "Executa o algoritmo de procura correspondente."
  (cond
    ( (equal algoritmo 'bfs)
      (format t "Executando BFS no tabuleiro: ~a~%" tabuleiro)
    )
    ( (equal algoritmo 'dfs)
      (format t "Executando DFS no tabuleiro: ~a com profundidade ~d~%" tabuleiro profundidade)
    )
    ( (equal algoritmo 'a*)
      (format t "Executando A* no tabuleiro: ~a~%" tabuleiro)
    )
  )
)
