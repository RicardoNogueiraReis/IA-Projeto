# **Manual de Utilizador** - Projeto 1

## **Inteligência Artificial - Escola Superior de Tecnologia de Setúbal**
![[Manual_utilizador/imgs/logo.png]]
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

1. [Acrónimos e Convenções usadas](#1)
2. [Instalação e utilização](#2)
3. [Input/Output](#3)
4. [Exemplo de aplicação](#4)

<div class="pageBreak"></div>

## **1. Acrónimos e Convenções Utilizadas** <a name="1"></a>
Neste manual, são utilizadas algumas abreviaturas e convenções para facilitar a leitura e garantir a consistência na apresentação da informação. Estas definições são descritas abaixo:
### 1.1 Acrónimos
- **IA**: Inteligência Artificial
- **DFS**: Depth-First Search, Pesquisa em Profundidade
- **BFS**: Breadth-First Search, Pesquisa em Largura
- **IDE**: Integrated Development Environment, Ambiente de Desenvolvimento Integrado
## **2. Introdução** <a name="2"></a>
Este programa foi concebido para quem procura resolver problemas de pesquisa em espaços de estados utilizando algoritmos, como **DFS** (Pesquisa em Profundidade), **BFS** (Pesquisa em Largura) e A*. É ideal para quem deseja compreender o comportamento, as vantagens e as limitações de cada algoritmo, seja em termos de desempenho e/ou eficácia.
Serve para resolver problemas de pesquisa que envolvem a transição entre estados até atingir um objetivo, como jogos de tabuleiro.
## **3. Instalação e Utilização** <a name="3"></a>
#### 3.1 Instalação do Programa
1. **Requisitos de Software**:  
    O programa foi desenvolvido em **Lisp**, utilizando o ambiente **LispWorks**. Certifique-se de que o LispWorks está instalado na máquina antes de prosseguir.
2. **Passos para Instalar**:
    - **Passo 1**: Faça o download dos ficheiros do projeto, que devem incluir o código-fonte.
    - **Passo 2**: Extraia os ficheiros para uma pasta local na máquina.
    - **Passo 3**: Abra o LispWorks e configure o ambiente de trabalho para apontar para a pasta do projeto.
3. **Configuração Inicial**:
	Antes de executar o programa, certifique-se de que o no início do ficheiro `projeto.lisp`) está com o caminho correto para carregar os outros 2 ficheiros do projeto, `procura.lisp` e `puzzle.lisp`:
```
(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/procura.lisp")

(load "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/puzzle.lisp")	
```
4. **Ainda no `projeto.lisp`**:
	Utilize o mesmo caminho para o configurar os ficheiros problemas.dat e log.dat. *CTRL + F* para encontrar a localização destes ficheiros dentro do projeto.lisp e adicione o caminho:
```
...

((tabuleiros (ler-problemas "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/problemas.dat"))

...

(with-open-file (stream "C:/Users/rodri/Desktop/Eng.Info/IA/IA-Projeto/Projeto-Estado-Final/log.dat" :direction :output :if-exists :append)

...
```
#### 3.2 Utilização do Programa
1. **Iniciar o Programa**:  
    Abra o Listener no LispWorks, certifique-se de que todos os ficheiros necessários estão carregados e compilados e inicie o programa executando a função principal:
    `(iniciar)`
2.  **Selecionar um tabuleiro:**
	Irá ser demonstrado no Listener diversos tabuleiros e deverá escrever o número correspondente ao tabuleiro que pretende solucionar.
	![[Pasted image 20241220180130.png]]
3. **Escolha do algoritmo:**
	O programa, neste ponto, vai pedir para selecionar o algoritmo que pretende utilizar para resolver o tabuleiro escolhido. Deverá escrever o número referente ao algoritmo que pretende utilizar:
	![[Pasted image 20241220182828.png]]
## **4. Input/Output** <a name="4"></a>

### Input
O input do programa é escolher um dos tabuleiros disponíveis. Podem ser adicionados mais tabuleiros, acrescentando no ficheiro `problemas.dat` com o seguinte formato:
```
(
	(1 8 8 5 8 7)
	(3 8 6 3 8 2)
)
```
![[Pasted image 20241220221735.png]]
### Output
O output do programa vai ser o resultado da procura, ou seja, estrutura de dados de um nó -profundidade, heurística(apenas no algoritmo A*) e referência ao nó-pai.
![[Pasted image 20241220222030.png]]
## **5. Exemplo de Aplicação** <a name="5"></a>
#### 5.1 Cenário de Sucesso
Para fins demonstrativos, vamos assumir que o utilizador escolher o tabuleiro `1` e pretende utilizar o algoritmo *BFS*.
1. Terá que executar o comando `(iniciar)` e selecionar o tabuleiro que pretende ser solucionado:
![[Pasted image 20241220222643.png]]
2. Selecionar o algoritmo de Procura em largura (BFS), sendo o número correspondente o número 1:
![[Pasted image 20241220222654.png]]
#### 5.2 Cenário de Insucesso
Para fins demonstrativos, vamos assumir que o utilizador escolher o tabuleiro `3` e pretende utilizar o algoritmo *BFS*.
1.  Terá que executar o comando `(iniciar)` e selecionar o tabuleiro que pretende ser solucionado:
![[Pasted image 20241220222936.png]]
2.  Selecionar o algoritmo de Procura em largura (BFS), sendo o número correspondente o número 1:
![[Pasted image 20241220223004.png]]