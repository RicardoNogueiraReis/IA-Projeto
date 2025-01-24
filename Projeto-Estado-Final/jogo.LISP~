;;; jogo.lisp

(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/algoritmo.lisp")
(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/puzzle.lisp")

(defparameter *profundidade-maxima* 3) ;; Profundidade default para o Alfa-Beta

(defun ler-tabuleiros ()
  "Lê o problemas.dat e retorna uma lista com os tabuleiros todos"
  (with-open-file (ficheiro "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/problemas.dat"
                          :direction :input :if-does-not-exist :error)
    (let ((problemas))
      (loop
         for problema = (read ficheiro nil 'eof) 
         while (not (eq problema 'eof))       
         do (push problema problemas))
      (nreverse problemas)))
)

(defun escolher-tabuleiro (tabuleiros)
  "Permite ao utilizador escolher um tabuleiro de entre uma lista."
  (format t "Escolha um tabuleiro:~%")
  (loop for i from 1 for tabuleiro in tabuleiros do
        (format t "~a: ~a~%" i tabuleiro))
  (let ((opcao (read)))
    (nth (1- opcao) tabuleiros)))

(defun escrever-log (resultado algoritmo)
  "Escreve os resultados de execução no ficheiro de log."
  (with-open-file (stream "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "Algoritmo: ~a~%" algoritmo)
    (format stream "Resultado: ~a~%" resultado)
    (format stream "----------------------~%")))

(defun ler-profundidade ()
  "Lê a profundidade máxima para algoritmos de procura em profundidade."
  (format t "Insira a profundidade máxima: ")
  (read))

(defun ler-algoritmo ()
  "Permite escolher o algoritmo de procura a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura (BFS)~%")
    (format t "2- Procura na profundidade (DFS)~%")
    (format t "3- Procura do Melhor Primeiro (A*)~%")
    (format t "4- Alfa-Beta~%")
    (let ((resposta (read)))
      (cond
        ((= resposta 1) 'bfs)
        ((= resposta 2) 'dfs)
        ((= resposta 3) 'a*)
        ((= resposta 4) 'alfabeta)
        (t (format t "Opção inválida.~%") nil)))))

(defun iniciar ()
  "Inicia o programa, lendo os tabuleiros de problemas.dat e executando a procura."
  (let* ((tabuleiros (ler-problemas "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/problemas.dat"))
         (tabuleiro (escolher-tabuleiro tabuleiros))
         (algoritmo (ler-algoritmo))
         (profundidade (if (equal algoritmo 'dfs) (ler-profundidade) nil)))
    (cond
      ((equal algoritmo 'bfs)
       (escrever-log (executar-procura algoritmo tabuleiro profundidade) algoritmo))
      ((equal algoritmo 'dfs)
       (escrever-log (executar-procura algoritmo tabuleiro profundidade) algoritmo))
      ((equal algoritmo 'a*)
       (escrever-log (executar-procura algoritmo tabuleiro profundidade) algoritmo))
      ((equal algoritmo 'alfabeta)
       (let ((resultado (jogar tabuleiro *profundidade-maxima*)))
         (escrever-log resultado 'alfabeta)
         resultado))
      (t
       (format t "Algoritmo inválido. Tente novamente.~%")))))