;;; jogo.lisp

(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/algoritmo.lisp")
(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/puzzle.lisp")

(defparameter *profundidade-maxima* 3) ;; Profundidade default Alfa-Beta

(defun ler-problemas-recursivo (stream)
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

(defun ler-tabuleiros ()
  (with-open-file (ficheiro "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/problemas.dat"
                          :direction :input :if-does-not-exist :error)
    (let ((problemas))
      (loop
         for problema = (read ficheiro nil 'eof) 
         while (not (eq problema 'eof))     
         do (push problema problemas)
       )
      (nreverse problemas)
    )
  )
)

(defun escolher-tabuleiro (tabuleiros)
  (format t "Escolha um tabuleiro:~%")
  (loop for i from 1 for tabuleiro in tabuleiros do
        (format t "~a: ~a~%" i tabuleiro)
  )
  (let ((opcao (read)))
    (nth (1- opcao) tabuleiros)
  )
)

(defun escrever-log (resultado algoritmo)
  (with-open-file (stream "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "Algoritmo: ~a~%" algoritmo)
    (format stream "Resultado: ~a~%" resultado)
    (format stream "----------------------~%")
  )
)

(defun ler-profundidade ()
  (format t "Insira a profundidade máxima: ")
  (read)
)

(defun ler-algoritmo ()
  (progn
    ;; (format t "Que algoritmo quer usar para procurar? ~%")
    ;; (format t "1- Procura na largura (BFS)~%")
    ;; (format t "2- Procura na profundidade (DFS)~%")
    ;; (format t "3- Procura do Melhor Primeiro (A*)~%")
    (format t "4- Alfa-Beta~%")
    (let ( (resposta (read)) )
      (cond
        ((= resposta 1) 'bfs)
        ((= resposta 2) 'dfs)
        ((= resposta 3) 'a*)
        ((= resposta 4) 'alfabeta)
        (t 
         (format t "Opção inválida.~%") 
         nil
        )
      )
    )
  )
)

(defun iniciar ()
  (let* ((tabuleiros (ler-problemas "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/problemas.dat"))
         (tabuleiro (escolher-tabuleiro tabuleiros))
         (algoritmo (ler-algoritmo))
         (profundidade 
          (if (equal algoritmo 'dfs) 
              (ler-profundidade) 
              nil
          )
         )
        )
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
         resultado)
       )
      (t
       (format t "Algoritmo inválido. Tente novamente.~%")
      )
    )
  )
)