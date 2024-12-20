# **Manual Técnico** - Projeto 1

## **Inteligência Artificial - Escola Superior de Tecnologia de Setúbal**
![[Manual_tecnico/imgs/logo.png]]
- **2024/2025**

- **Docentes:**
  - Filipe Mariano
  - Joaquim Filipe
- **Alunos:**
  - 202000806 Ricardo Reis
  - 202001352 Rodrigo Nogueira

<style>
  .capa {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 150px;
  }

  .capa > .imagem {
    width: 300px;
  }

  .pageBreak {
    page-break-after: always;
  }
</style>

<div class="pageBreak"></div>

## **Índice**

1. [Arquitetura do sistema](#1)
2. [Entidades e sua implementação](#2)
3. [Algoritmos e sua implementação](#3)
4. [Descrição das opções tomadas](#4)
5. [Limitações técnicas e ideias para desenvolvimento futuro](#5)

<div class="pageBreak"></div>

## **1. Arquitetura do sistema** <a name="1"></a>
O sistema foi concebido de forma modular, com três componentes principais interligados: `procura.lisp`, `puzzle.lisp` e `projeto.lisp`. A comunicação entre os módulos ocorre por meio de funções definidas e estruturadas para facilitar a manutenção do programa.
## 1.1 Identificação dos Módulos
#### 1.1.1 procura.lisp
Este módulo implementa os algoritmos de procura, BFS, DFS e A*, de maneira genérica, ou seja, é independente do domínio de aplicação. O módulo oferece funções que podem ser reutilizadas para resolver qualquer problema que envolva a procura em espaço de estados. Este módulo é responsável por:
- Criar e manipular nós que representam os estados do problema.
- Gerir listas de estados abertos e fechados durante a procura.
- Calcular métricas como número de nós gerados e expandidos.
- Implementar as heurísticas, especialmente no algoritmo A*.
#### 1.1.2 puzzle.lisp
Este módulo encapsula o domínio específico do problema do projeto, Adji-boto*. Ele define:
- O formato do tabuleiro como uma lista de listas.
- Funções para manipular o tabuleiro, como verificar se está vazio e distribuir peças entre os buracos.
- Operadores que implementam as regras do jogo, permitindo transitar de um estado para outro.
#### 1.1.3 projeto.lisp
Este módulo atua como o ponto de entrada para o programa. Ele é responsável por:
- Gerir a interação com o utilizador.
- Ler tabuleiros iniciais a partir de ficheiros.
- Permitir a escolha do algoritmo de procura e os seus parâmetros.
- Executar a procura e apresentar os resultados.
## 1.2 Fluxo de Informação
- O utilizador inicia o programa através do `projeto.lisp`. Este módulo solicita a escolha de um tabuleiro e de um algoritmo de procura.
- Uma vez escolhido o algoritmo, as funções de procura do módulo `procura.lisp` são chamadas, com o tabuleiro inicial fornecido pelo módulo `puzzle.lisp`.
- Durante a execução, o módulo `procura.lisp` utiliza as funções de manipulação de tabuleiro fornecidas por `puzzle.lisp` para criar sucessores e verificar os estados objetivos.
- No final da procura, o módulo `projeto.lisp` apresenta os resultados, como a solução encontrada e as métricas de desempenho.
## 1.3 Fluxo Geral
1. **Entrada**:
    - Tabuleiro inicial lido de um ficheiro.
    - Escolha do algoritmo de procura.
2. **Processamento**:
    - O algoritmo de procura expande sucessores a partir do estado inicial até encontrar uma solução ou esgotar as possibilidades.
3. **Saída**:
    - Solução encontrada - caminho de estados.
    - Métricas de desempenho, como o número de nós gerados, expandidos, tempo de execução, penetrância e fator de ramificação.

Esta arquitetura garante a modularidade e extensibilidade do sistema, permitindo a adição de novos algoritmos ou alterações nas regras do jogo sem impactar os outros módulos significativamente. A separação das responsabilidades dos módulos também facilita o *debug* e manutenção do programa.
## **2. Entidades e sua implementação** <a name="2"></a>
O sistema utiliza várias entidades programáticas para representar o domínio do problema e a lógica de procura. As entidades são modeladas como tipos abstratos de dados e funções que manipulam esses dados. Abaixo estão as principais entidades do sistema e suas implementações.
## 2.1 Representação
O tabuleiro inicial com duas linhas e seis buracos, cada um contendo um número de peças, é definido como:
```
'((8 8 8 8 8 8)
  (8 8 8 8 8 8))
```
## 2.1.1 Funções Associadas
1. **`linha`**: Retorna uma linha específica do tabuleiro
```
(defun linha (indice tabuleiro)
  (nth indice tabuleiro))
```
2. **`celula`**: Retorna o número de peças em uma célula específica.
```
(defun celula (indiceLinha posicao tabuleiro)
  (nth posicao (linha indiceLinha tabuleiro)))
```
3. **`substituir`**: Substitui o valor de uma célula específica no tabuleiro.
```
(defun substituir (indiceLinha posicao tabuleiro &optional (novo-valor 0))`
  (cond
    ((= indiceLinha 0)
       (list (substituir-posicao posicao (linha indiceLinha tabuleiro) novo-valor) (linha 1 tabuleiro)))
    (t
     (list (linha 0 tabuleiro) (substituir-posicao posicao (linha indiceLinha tabuleiro) novo-valor)))))
```
4. **`tabuleiro-vaziop`**: Verifica se o tabuleiro está vazio, que é o objetivo do problema.
```
(defun tabuleiro-vaziop (tabuleiro)
  (and (linha-vazia-p (linha 0 tabuleiro))
       (linha-vazia-p (linha 1 tabuleiro))))
```
## 2.2. Nó
Cada estado do problema é representado como um nó. O nó encapsula o estado atual do tabuleiro, a profundidade no espaço de procura, o valor da heurística e uma referência ao nó pai.
### 2.2.1 Representação de um nó
```
(list tabuleiro profundidade heuristica pai)
```
### 2.2.2 Funções Associadas
1. **`cria-no`**: Cria um novo nó a partir de um estado do tabuleiro.
```
(defun cria-no (tabuleiro &optional (g 0) (h (heuristica tabuleiro)) (pai nil))
  (list tabuleiro g h pai))
```
2. **`no-estado`**: Retorna o estado do tabuleiro contido no nó.
```
(defun no-estado (no)
  (first no))
```
3. **`no-profundidade`**: Retorna a profundidade do nó.
```
(defun no-profundidade (no)
  (second no))
```
4. **`no-heuristica`**: Retorna o valor da heurística associada ao nó.
```
(defun no-heuristica (no)
  (third no))
```
5. **`no-pai`**: Retorna o nó pai.
```
(defun no-pai (no)
  (fourth no))
```
### 2.3 Operadores
Os operadores representam as ações possíveis no jogo, como movimentar peças entre buracos e capturar peças. Cada operador aplica uma regra específica ao tabuleiro e gera um novo estado.
#### Implementação
1. **`operador`**: Define uma ação no tabuleiro, aplicando a regra de movimentação de peças.
```
(defun operador (indiceLinha indiceColuna &optional (tabuleiro (tabuleiro-teste)))
  (percorrer-posicoes
    (distribuir-pecas (celula indiceLinha indiceColuna tabuleiro)
                      indiceLinha
                      indiceColuna
                      tabuleiro)
    (substituir indiceLinha indiceColuna tabuleiro 0)))
```
2. **`jogadas`**: Retorna todas as jogadas possíveis (todos os buracos).
```
(defun jogadas ()
  '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (1 0) (1 1) (1 2) (1 3) (1 4) (1 5)))
```
### 2.4. Heurística
A heurística é uma função que avalia o "custo estimado" para alcançar o estado objetivo. No caso do Adji-boto*, é calculada considerando o número de peças capturadas e as peças restantes no tabuleiro.
#### Funções Associadas
1. **`o`**: Calcula o número de peças restantes no tabuleiro.
```
(defun o (no-estado)
  (+ (o-aux (first no-estado)) (o-aux (second no-estado))))
```
2. **`c`**: Calcula o número de peças já capturadas.
```
(defun c (estado-no valor-heuristico-no-inicial)
  (- valor-heuristico-no-inicial (o estado-no)))
```
3.  **`heuristica`**: Calcula o valor da heurística combinando as funções anteriores.
```
(defun heuristica (no-estado &optional (valor-heuristico-no-inicial (o no-estado)))
  (- (o no-estado) (c no-estado heuristica-no-inicial)))
```

Estas entidades formam a base do sistema, conectando as regras do jogo, os estados intermediários e os algoritmos de procura de maneira lógica. A abstração entre o domínio do problema e a lógica genérica de procura foi essencial para garantir a flexibilidade e robustez do projeto.
## **3. Algoritmos e a sua implementação** <a name="3"></a>
Os algoritmos de procura implementados no projeto são responsáveis por explorar o espaço de estados do problema Adji-boto*, procurando um caminho que leve do estado inicial ao estado final, onde todos os buracos estão vazios. Os métodos de procura utilizados incluem a procura em largura (BFS), procura em profundidade (DFS), e A*. Este capítulo detalha cada algoritmo, a sua implementação e características.
Para os testes realizados para este manual, foi utilizado o mesmo problema, mas resolvido por cada algoritmo, para fins de comparação.
### 3.1 Procura em largura - BFS
A procura em largura explora os estados de forma uniforme, expandindo os nós da árvore de procura nível por nível. Ela garante encontrar a solução mais curta (ótima) se o custo de cada aresta no grafo de estados for igual.
#### 3.1.1 Implementação
A implementação utiliza uma fila, FIFO, para armazenar os nós abertos, garantindo que os nós de profundidade menor sejam expandidos antes dos mais profundos.
```
(defun bfs (funcaoObjetivo funcaoSucessores jogadas &optional (abertos nil) (fechados nil))
  (cond
    ((null abertos) nil)
    (t
     (let* ((n (car abertos))
            (nos-sucessores (remove nil (funcall funcaoSucessores n jogadas "bfs")))
            (novos-abertos (remove nil (abertos-bfs (cdr abertos) nos-sucessores fechados)))
            (novos-fechados (cons n fechados))
            (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo)))
       (cond
         ((not (null no-objetivo)) no-objetivo)
         (t (bfs funcaoObjetivo funcaoSucessores jogadas novos-abertos novos-fechados))))))
```
#### 3.1.2 Características
- **Eficácia**: Encontra sempre a solução, se esta existir.
- **Ótima**: Garante o menor caminho, se todos os operadores tiverem custo uniforme.
- **Complexidade Espacial e Temporal**: Exponencial, pois armazena todos os nós de um nível antes de avançar para o próximo.
##### 3.1.3 Teste
![[Pasted image 20241220113304.png]]
### 3.2 Procura em profundidade - DFS
A procura em profundidade explora os estados indo o mais fundo possível em cada caminho antes de retroceder. Este método pode ser eficiente em termos de espaço, mas não é garantido que encontre a solução ótima.
#### 3.2.1 Implementação
DFS utiliza uma pilha (LIFO) para armazenar os nós abertos, garantindo que o nó mais recentemente descoberto seja expandido primeiro.
```
(defun dfs (funcaoObjetivo funcaoSucessores jogadas profundidadeMax &optional (abertos nil) (fechados nil))
  (cond
    ((null abertos) nil)
    (t
     (let* ((n (car abertos))
            (nos-sucessores (remove nil (funcall funcaoSucessores n jogadas "dfs" profundidadeMax)))
            (novos-abertos (remove nil (abertos-dfs (cdr abertos) nos-sucessores fechados)))
            (novos-fechados (cons n fechados))
            (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo)))
       (cond
         ((not (null no-objetivo)) no-objetivo)
         (t (dfs funcaoObjetivo funcaoSucessores jogadas profundidadeMax novos-abertos novos-fechados))))))
```
#### 3.2.2 Características
- **Eficácia**: Consegue encontrar uma solução, desde que o espaço de estados seja finito e o ramo correto seja explorado. No entanto, a qualidade da solução encontrada depende diretamente da admissibilidade e da consistência da heurística utilizada. Uma heurística admissível garante que a solução encontrada seja ótima, enquanto uma heurística inconsistente pode levar o algoritmo a selecionar caminhos sub-ótimos, mesmo que uma solução seja encontrada.
- **Ótima**: Não garante a solução mais curta.
- **Complexidade Espacial**: Linear em relação à profundidade máxima.
##### 3.2.3 Teste
![[Pasted image 20241220113328.png]]
### 3.3 Procura com A*
O algoritmo A* combina a procura por custo uniforme, utilizando uma função heurística $f(n)=g(n)+h(n)$, onde:
- $g(n)$: Custo acumulado do caminho desde o estado inicial até n.
- $h(n)$: Estimativa do custo do nó atual até o estado objetivo.
#### 3.3.1 Implementação
A lista de abertos é mantida ordenada pelo valor $f(n)$, garantindo que os estados mais promissores sejam expandidos primeiro.
```
(defun a* (funcaoObjetivo funcao-sucessores jogadas funcao-heuristica
           &optional (abertos (list (no-teste))) (fechados nil) (nos-gerados 0) (nos-expandidos 0)
                     (heuristica-tabuleiro-inicial (heuristica (no-estado (car abertos)))))
  (cond
    ((null abertos) nil)
    (t
     (let* ((n (car abertos))
            (nos-sucessores (remove nil (funcall funcao-sucessores n jogadas funcao-heuristica heuristica-tabuleiro-inicial)))
            (nos-filtrados (remove-if #'(lambda (sucessor) (no-redundante-p sucessor fechados)) nos-sucessores))
            (novos-abertos (colocar-sucessores-em-abertos (cdr abertos) nos-filtrados))
            (novos-fechados (cons n fechados))
            (no-objetivo (retorna-no-objetivo nos-sucessores funcaoObjetivo)))
       (cond
         (no-objetivo no-objetivo)
         (t (a* funcaoObjetivo funcao-sucessores jogadas funcao-heuristica
                 novos-abertos novos-fechados nos-gerados-nos-expandidos
                 heuristica-tabuleiro-inicial)))))))
```
#### 3.3.2 Características
- **Eficácia**: Sempre encontra a solução, se a heurística for admissível.
- **Ótima**: Garante o menor caminho, se a heurística for consistente.
- **Complexidade**: Exponencial em termos de tempo e espaço, mas pode ser mais eficiente do que BFS.
#### 3.3.3 Teste
![[Pasted image 20241220113359.png]]
### 3.4 Métricas de Desempenho
Os algoritmos foram avaliados com base nas seguintes métricas:
1. **Número de Nós Gerados**: Quantidade total de estados visitados.
2. **Número de Nós Expandidos**: Estados efetivamente explorados.
3. **Penetrância**: Razão entre o comprimento do caminho e o número de nós gerados.
4. **Fator de Ramificação Médio**: Média de sucessores por nó.
5. **Tempo de Execução**: Medido em segundos.
### 3.5 Análise Comparativa
- BFS é eficiente para garantir soluções ótimas em problemas pequenos, mas consome mais memória.
- DFS requer menos memória, mas pode ser ineficiente para problemas maiores.
- A* apresentou melhor desempenho quando uma heurística consistente foi utilizada.
## **4. Descrição das opções tomadas** <a name="4"></a>
Durante o desenvolvimento do projeto, várias decisões de implementação foram realizadas com base na análise do problema, restrições técnicas, e objetivos de desempenho. Este capítulo apresenta as principais escolhas feitas, as suas justificações e possíveis alternativas que foram descartadas.
### 4.1 Estrutura Modular
Optou-se por dividir o programa em três módulos principais:
1. **`procura.lisp`**: Implementa os algoritmos genéricos de procura.
2. **`puzzle.lisp`**: Contém a modelagem do problema Adji-boto*, incluindo a definição do tabuleiro e operadores.
3. **`projeto.lisp`**: Atua como interface principal do programa, faz a gestão a entrada e saída de dados e a interação com o utilizador.
Essa abordagem modular facilita a manutenção do código, permitindo alterações em um módulo sem impactar diretamente os outros. Também favorece a reutilização de componentes, como os algoritmos de procura, em outros problemas semelhantes.
### 4.2. Representação do Tabuleiro
O tabuleiro foi modelado como uma lista de listas, onde cada linha do tabuleiro corresponde a uma sublista e cada elemento representa um buraco com seu número de peças.
A escolha foi motivada pela forma de manipulação de listas em Lisp e pela compatibilidade com os algoritmos de procura. A representação é intuitiva e reflete diretamente o estado do jogo.
### 4.3. Seleção dos Algoritmos de Procura
Foram implementados BFS, DFS, e A* como métodos principais, dado que:
- **BFS** é completo e garante soluções ótimas para problemas simples.
- **DFS** é eficiente em termos de espaço e permite limitar a profundidade.
- **A**** combina heurísticas informadas para melhorar o desempenho em problemas maiores.
Esses algoritmos são fundamentais para o estudo comparativo de métodos de procura e oferecem uma visão abrangente de suas vantagens e limitações no contexto do projeto.
### 4.4. Escolha da Heurística
A heurística considerada foi a seguinte: é calculada a diferença entre o número de peças restantes a serem capturadas $o(x)$ e o número de peças já capturadas $c(x)$.
A heurística é admissível e consistente, garantindo a corretude do A*. Visa também, a explorar a estratégia do jogo, melhorando o desempenho sem comprometer a solução ótima.
### 4.5. Estrutura dos Operadores
A função operador é chamada pela função sucessores-heuristica, com o auxilio da função novo-sucessor-heuristica e a função jogadas (que retorna as coordenadas dos buracos para tirar as peças) é possível criar nós sucessores, ao iterar a lista das jogadas, representando as possíveis jogadas seguintes por parte do jogador. Ao representar as jogadas seguintes esses nós sucessores demostram o resultado dessa jogada.
- **sucessores-heuristica** - pega em um nó e as possíveis jogadas (todos os buracos do tabuleiro)
- **novo-sucessor-heuristica** - vai iterando as jogadas para representar o resultado de cada jogada
- **operador** - chama a função distribuir-pecas que, dependendo do número de peças presentes no buraco escolhido, retorna as coordenadas dos buracos do tabuleiro onde é possivel distribuir as peças, a função percorrer-posicoes vai a cada buraco do tabuleiro incrementando o número de peças nos buracos que percorre (simulando a distrbuição das peças uma a uma em cada buraco).
#### 4.6. Uso de Estruturas Recursivas
Muitas funções foram implementadas de forma recursiva, refletindo o paradigma funcional de programação em Lisp.
A recursividade é natural para percorrer listas e árvores de procura, o que torna o código mais compacto.
#### 4.7. Criação de Dados de Desempenho
O programa regista métricas como número de nós gerados, nós expandidos, e fator de ramificação médio.
Essas métricas são fundamentais para comparar o desempenho dos algoritmos e validar as implementações.

Em resumo, as decisões tomadas priorizaram a modularidade, e conformidade com os requisitos do projeto, assegurando um equilíbrio entre funcionalidade e complexidade.
## **5. Limitações técnicas e ideias para desenvolvimento futuro** <a name="5"></a>
#### 5.1. Requisitos Não Implementados
Embora o projeto cumpra a maioria dos objetivos definidos, algumas funcionalidades não foram implementadas devido a restrições de tempo e complexidade técnica. Entre elas:
1. **Algoritmos Avançados de Procura**:
    - Métodos como IDA*, SMA*, e RBFS, mencionados como bónus no enunciado, não foram desenvolvidos. Embora a sua implementação fosse interessante, a sua complexidade exigiria mais tempo para validação e desenvolvimento.
#### 5.2. Refactoring Futuro
Durante o desenvolvimento, identificaram-se várias áreas do código que poderiam ser melhoradas para maior eficiência e manutenibilidade:
1. **Modularização do Algoritmo A***:
    - O código atual do A* contém dependências fortes entre a função de sucessores e a heurística. Separar estas responsabilidades simplificaria testes e modificações.
2. **Generalização dos Operadores**:
    - Os operadores são específicos ao domínio do Adji-boto*. Adaptá-los para reutilização em outros problemas baseados em tabuleiros tornaria o módulo mais versátil.
3. **Tratamento de Erros**:
    - A validação de entrada (por exemplo, escolha de tabuleiro ou algoritmo) é limitada. Um sistema com mais robustez de manipulação de erros tornaria o programa mais confiável.
#### 5.3. Melhoramentos Potenciais de Desempenho
Com mais tempo, algumas melhorias de desempenho poderiam ser implementadas:
1. **Optimização da Heurística**:
    - Analisar heurísticas mais avançadas para acelerar a procura de soluções.
2. **Paralelismo**:
    - Em sistemas modernos, dividir a exploração do espaço de estados entre múltiplos threads ou processos permitiria resolver problemas maiores em menos tempo.
#### 5.5. Limitações Técnicas
1. **Restrições de Memória do LispWorks**:
    - Apenas o primeiro e o terceiro problema puderam ser resolvidos dentro dos limites de memória disponíveis no ambiente LispWorks. Os outros problemas são demasiado grandes para serem processados devido ao limite de memória, o que impede a exploração completa do espaço de estados necessário para encontrar uma solução.
2. **Escalabilidade dos Algoritmos**:
    - Mesmo nos problemas resolvidos, o desempenho de algoritmos como BFS e DFS é fortemente impactado pelo crescimento do espaço de busca. Esta limitação é agravada pela falta de otimizações específicas no tratamento de sucessores e na validação de estados visitados.