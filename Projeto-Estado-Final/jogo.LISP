;;; jogo.lisp

(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/algoritmo.lisp")
(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/puzzle.lisp")

(defun ler-problemas-recursivo (stream)
  "Lê recursivamente os problemas de um ficheiro aberto e devolve uma lista de tabuleiros."
  (let ((problema (read stream nil nil)))
    (if problema
        (cons problema (ler-problemas-recursivo stream))
        nil)))

(defun ler-problemas (ficheiro)
  "Lê todos os problemas do ficheiro .dat utilizando a recursão."
  (with-open-file (stream ficheiro :direction :input)
    (ler-problemas-recursivo stream)))

(defun mostrar-tabuleiros (tabuleiros)
  "Mostra os tabuleiros numerados."
  (loop for tabuleiro in tabuleiros
        for index from 0
        do (format t "~d: ~a~%" (1+ index) tabuleiro)))

(defun escolher-tabuleiro (tabuleiros)
  "Mostra os tabuleiros e permite ao utilizador escolher um."
  (mostrar-tabuleiros tabuleiros)
  (nth (1- (read)) tabuleiros))

(defun ler-algoritmo ()
  "Permite escolher o algoritmo de procura a utilizar."
  (format t "Que algoritmo quer usar para procurar? ~%")
  (format t "1- Procura na largura (BFS)~%")
  (format t "2- Procura na profundidade (DFS)~%")
  (format t "3- Procura do Melhor Primeiro (A*)~%")
  (format t "4- AlfaBeta~%")
  (case (read)
    (1 'bfs)
    (2 'dfs)
    (3 'a*)
    (4 'alfabeta)))

(defun ler-profundidade ()
  "Pergunta a profundidade limite para DFS."
  (format t "Qual a profundidade limite para o DFS?~%")
  (read))

(defun escrever-log (resultado algoritmo)
  "Escreve os resultados da procura no arquivo log.dat."
  (with-open-file (stream "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/log.dat" :direction :output :if-exists :append)
    (format stream "Algoritmo: ~a~%Resultado: ~a~%~%" algoritmo resultado)))

(defun executar-procura (algoritmo tabuleiro profundidade)
  "Executa o algoritmo de procura correspondente, exibe a solução e retorna o resultado."
  (let ((resultado (case algoritmo
                     (bfs (bfs #'no-solucaop #'sucessores (jogadas) (list (cria-no tabuleiro))))
                     (dfs (dfs #'no-solucaop #'sucessores (jogadas) profundidade (list (cria-no tabuleiro))))
                     (a* (a* #'no-solucaop #'sucessores-heuristica (jogadas) #'heuristica (list (cria-no tabuleiro))))
                     (alfabeta (jogar tabuleiro 20)))))
    (mostrar-solucao resultado)
    resultado))

(defun iniciar ()
  "Permite iniciar o programa, lendo o tabuleiro, o algoritmo e executando a procura."
  (let* ((tabuleiros (ler-problemas "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/problemas.dat"))
         (tabuleiro (escolher-tabuleiro tabuleiros))
         (algoritmo (ler-algoritmo))
         (profundidade (if (eql algoritmo 'dfs) (ler-profundidade) nil))
         (resultado (executar-procura algoritmo tabuleiro profundidade)))
    (escrever-log resultado algoritmo)
    resultado))