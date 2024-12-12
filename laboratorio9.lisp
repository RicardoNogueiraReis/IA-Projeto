;; LABORATÓRIO 9
(defun escrever-no (estado-inicial solucao nos-gerados nos-expandidos tempo-execucao comprimento-caminho)
  "Escreve os resultados da busca em log.dat, incluindo métricas adicionais."
  (let ((pen (penetrancia comprimento-caminho nos-gerados))
        (fator (fator-ramificacao comprimento-caminho nos-gerados)))
    (with-open-file (stream "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/log.dat" 
                            :direction :output 
                            :if-does-not-exist :create 
                            :if-exists :append)
      (format stream "Estado Inicial: ~a~%" estado-inicial)
      ( if solucao
          (format stream "Solução: ~a~%" (no-estado solucao))
          (format stream "Solução: Não encontrada~%") )
      (format stream "Comprimento do Caminho: ~a~%" comprimento-caminho)
      (format stream "Nós Gerados: ~a~%" nos-gerados)
      (format stream "Nós Expandidos: ~a~%" nos-expandidos)
      (format stream "Tempo de Execução: ~a segundos~%" tempo-execucao)
      (format stream "Penetrância: ~a~%" pen)
      (format stream "Fator de Ramificação Média: ~a~%" fator)
      (format stream "-----------------------------------~%")
    )
  )
)

;; Caminho para substituir na "stream"
;; Caminho PC Rodrigo -> C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/log.dat
;; Caminho PC Ricardo -> 

(defun a*-com-tempo (abertos fechados predicado-sucesso func-sucessores operadores heuristica nos-gerados nos-expandidos)
  "Versão do algoritmo A* que mede o tempo de execução."
  (let* ((inicio-tempo (get-internal-real-time)))
    (print "Início do A*")
    (print abertos)
    (let ((resultado (a* abertos fechados predicado-sucesso func-sucessores operadores heuristica nos-gerados nos-expandidos)))
      (print "Fim do A*")
      (print resultado)
      (let* ((fim-tempo (get-internal-real-time))
             (tempo-execucao (/ (- fim-tempo inicio-tempo) internal-time-units-per-second)))
        ;; Adiciona o tempo ao resultado
        (setf (getf resultado :tempo-execucao) tempo-execucao)
        resultado)))
)

(defun penetrancia (comprimento-caminho nos-gerados)
  "Calcula a penetrância da árvore de busca."
  ( if (and (numberp comprimento-caminho) (numberp nos-gerados) (> nos-gerados 0) )
      (/ comprimento-caminho nos-gerados)
      (error "Os parâmetros devem ser números, e o número de nós gerados deve ser maior que 0.")
  )
)

(defun bisseccao (funcao a b epsilon)
  "Resolve uma equação utilizando o método de bisseção (recursivo)."
  (let ((fa (funcall funcao a))
        (fb (funcall funcao b)))
    (cond
      ;; Caso base: Solução exata encontrada
      ((or (zerop fa) (zerop fb)) (if (zerop fa) a b))
      ;; Validação: O intervalo deve conter uma mudança de sinal
      ((not (< (* fa fb) 0))
       (error "A função deve mudar de sinal no intervalo dado."))
      (t
       ;; Passo recursivo
       (let* ((c (/ (+ a b) 2))
              (fc (funcall funcao c)))
         (if (< (abs fc) epsilon)
             c ;; Retorna a solução
             ;; Escolhe o subintervalo onde há mudança de sinal
             (if (< (* fa fc) 0)
                 (bisseccao funcao a c epsilon)
                 (bisseccao funcao c b epsilon))
             )
        )
       )
      )
    )
)

;; Teste no Listener
(bisseccao (lambda (x) (- (* x x) 4)) 1 3 0.001) ;; Resultado esperado: 2

(defun fator-ramificacao (comprimento-caminho nos-gerados)
  "Calcula o fator de ramificação médio resolvendo b^(n+1) - 1 = T * (b - 1)."
  (let ((n comprimento-caminho)
        (total-nos nos-gerados))
    (bisseccao (lambda (b)
                 (- (/ (- (expt b (1+ n)) 1) (- b 1)) total-nos))
               1.01 ;; Limite inferior para evitar divisão por zero
               10    ;; Limite superior para garantir convergência
               0.0001)
  )
)

;; Teste no Listener
(fator-ramificacao 3 13) ;; Resultado esperado: ~2

(defun mostrar-solucao (solucao comprimento-caminho nos-gerados nos-expandidos tempo-execucao)
  "Mostra detalhes da solução, incluindo penetrância, fator de ramificação e tempo de execução."
  (let ((pen (penetrancia comprimento-caminho nos-gerados))
        (fator (fator-ramificacao comprimento-caminho nos-gerados)))
    (format t "Solução: ~a~%" (if solucao (no-estado solucao) "Não encontrada"))
    (format t "Comprimento do caminho: ~a~%" comprimento-caminho)
    (format t "Nós gerados: ~a~%" nos-gerados)
    (format t "Nós expandidos: ~a~%" nos-expandidos)
    (format t "Tempo de execução: ~a segundos~%" tempo-execucao)
    (format t "Penetrância: ~a~%" pen)
    (format t "Fator de ramificação média: ~a~%" fator)
  )
)

(defun ler-estado-inicial (file)
  "Lê o estado inicial a partir de um file"
  (with-open-file (stream file :direction :input :if-does-not-exist :error)
    (read stream))
)

(defun iniciar-procura ()
  "Inicia a procura com leitura do estado inicial e cálculo das métricas."
  (let* ((estado-inicial (ler-estado-inicial "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/problemas.dat"))
         (resultado (a*-com-tempo (list (cria-no estado-inicial 0 (heuristica estado-inicial)))
                                  nil
                                  #'no-solucaop
                                  #'sucessores
                                  (operadores)
                                  #'heuristica
                                  0
                                  0)
         )
        )
    (when resultado
      (let ((solucao (getf resultado :solucao))
            (nos-gerados (getf resultado :nos-gerados))
            (nos-expandidos (getf resultado :nos-expandidos))
            (tempo-execucao (getf resultado :tempo-execucao))
            (comprimento-caminho (if solucao (no-profundidade solucao) 0))
           )
        ;; Mostrar solução no terminal
        (mostrar-solucao solucao comprimento-caminho nos-gerados nos-expandidos tempo-execucao)
        ;; Escrever solução no arquivo
        (escrever-no estado-inicial solucao nos-gerados nos-expandidos tempo-execucao comprimento-caminho)
      )
    )
  )
)