      *>-----------------IDENTIFICAÇÃO------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBanko.
       AUTHOR.        WESLEY A. M.
      *>-----------------AMBIENTE----------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-CONTAS ASSIGN TO "contas.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FILE-STATUS-ARQUIVO.
      *>------------------DADOS-------------------------
       DATA DIVISION.

       FILE SECTION.
       FD ARQUIVO-CONTAS.
       01 REGISTRO-ARQUIVO.
           05 ARQ-NUM-CONTA PIC 9(6).
           05 ARQ-NOME      PIC X(30).
           05 ARQ-CPF       PIC 9(11).
           05 ARQ-STATUS    PIC X(7).
           05 ARQ-SALDO     PIC S9(5)V99.
           05 ARQ-DATA      PIC X(14).

       WORKING-STORAGE SECTION.
      *>----------VARIÁVEIS------------
       01 OPCAO-MENU            PIC 9(1).
       01 NOME-CLIENTE          PIC X(30).
       01 CPF                   PIC 9(11).
       01 CONTADOR-CONTA        PIC 9(6) VALUE ZEROS.
       01 DATA-COMPLETA         PIC X(20).
       01 FILE-STATUS-ARQUIVO   PIC XX.
       01 FLAG-CPF-STATUS    PIC X  VALUE "N".
           88 CPF-DUPLICADO      VALUE "S".
           88 CPF-DISPONIVEL     VALUE "N".
       01 FLAG-NOME-STATUS   PIC X  VALUE "N".
           88 NOME-ENCONTRADO     VALUE "S".
       01 CAMPO-BUSCA        PIC X(30).
       01 VALOR-DEP          PIC S9(9)V99.

      *>EOF-FIM DO ARQUIVO
       01 EOF-FLAG              PIC X VALUE "N".
           88 FIM-ARQUIVO        VALUE "S". 
           88 LER-CONTINUAR      VALUE "N".
      *>-------REGISTRO DE DADOS-------
       01 REGISTRO-CONTA.
           05 NUMERO-CONTA PIC 9(6).
           05 DATA-CRIACAO.
              10 DD   PIC 9(2).
              10 MM   PIC 9(2).
              10 YYYY PIC 9(4).
              10 HH   PIC 9(2).
              10 MI   PIC 9(2).
              10 SS   PIC 9(2).
           05 NOME-CADASTRADO PIC X(30).
           05 CPF-CADASTRADO  PIC 9(30).
           05 SITUACAO  PIC X(7).
           05 SALDO        PIC S9(5)V99.

       01 STATUS-CONTA PIC X(7).
            88 ATIVA    VALUE 'ATIVA'.
            88 INATIVA  VALUE 'INATIVA'.

      *>--------------------LÓGICA----------------------
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM MENU-PRINCIPAL UNTIL OPCAO-MENU = 6
           STOP RUN.
      *>------PARAGRAFO MENU--------
       MENU-PRINCIPAL.
           DISPLAY "1 - CRIAR CONTA."
           DISPLAY "2 - CONSULTAR SALDO."
           DISPLAY "3 - SACAR."
           DISPLAY "4 - DEPOSITAR."
           DISPLAY "5 - ENCERRAR CONTA."
           DISPLAY "6 - SAIR."
           DISPLAY "ESCOLHA UMA OPÇÃO (EX: 1): "
           ACCEPT OPCAO-MENU
           EVALUATE OPCAO-MENU
              WHEN 1
                 PERFORM CRIAR-CONTA
              WHEN 2
                 PERFORM CONSULTAR-SALDO
              WHEN 3
                 DISPLAY "SACANDO..."
              WHEN 4
                 PERFORM DEPOSITAR
              WHEN 5
                 DISPLAY "ENCERRANDO CONTA..."
              WHEN 6
                 DISPLAY "SAINDO..."
              WHEN OTHER
                 DISPLAY "OPÇÃO INVALIDA."
           END-EVALUATE.
      *>---PARAGRAFO CRIAR CONTA---
       INICIALIZAR-ARQUIVO.
           OPEN INPUT ARQUIVO-CONTAS
           IF FILE-STATUS-ARQUIVO = "35"
              OPEN OUTPUT ARQUIVO-CONTAS 
              CLOSE ARQUIVO-CONTAS
           ELSE
              CLOSE ARQUIVO-CONTAS
           END-IF.
       CRIAR-CONTA.
           DISPLAY "INSIRA O NOME DO CLIENTE"
           ACCEPT NOME-CLIENTE
           DISPLAY "INSIRA O CPF: "
           ACCEPT CPF
           PERFORM LER-TODOS-REGISTROS
           IF CPF-DUPLICADO
              DISPLAY "ESSE CPF JÁ ESTÁ VINCULADO A UMA CONTA ATIVA"
              EXIT PARAGRAPH
           END-IF.
           MOVE "ATIVA" TO SITUACAO
           PERFORM GERAR-NUMERO-CONTA
           MOVE NOME-CLIENTE TO NOME-CADASTRADO
           MOVE CPF          TO CPF-CADASTRADO
           DISPLAY "CONTA CRIADA PARA " NOME-CLIENTE "COM STATUS "
                                                            SITUACAO
           DISPLAY "DATA DE CRIAÇÃO: " DD "/" MM "/" YYYY "-" HH ":"
                                                          MI ":" SS
           DISPLAY "SALDO: " SALDO
	.

           OPEN EXTEND ARQUIVO-CONTAS
              MOVE CONTADOR-CONTA   TO ARQ-NUM-CONTA
              MOVE NOME-CADASTRADO  TO ARQ-NOME
              MOVE CPF-CADASTRADO   TO ARQ-CPF
              MOVE SITUACAO         TO ARQ-STATUS
              MOVE SALDO            TO ARQ-SALDO
              MOVE DATA-CRIACAO     TO ARQ-DATA
              WRITE REGISTRO-ARQUIVO
           CLOSE ARQUIVO-CONTAS.

       LER-TODOS-REGISTROS.
           MOVE "N" TO FLAG-CPF-STATUS
           MOVE "N" TO EOF-FLAG
           OPEN INPUT ARQUIVO-CONTAS

           PERFORM UNTIL FIM-ARQUIVO OR CPF-DUPLICADO
             READ ARQUIVO-CONTAS
                AT END
                 MOVE "S" TO EOF-FLAG
              NOT AT END
                 IF ARQ-CPF = CPF AND ARQ-STATUS = "ATIVA"
                    MOVE "S" TO FLAG-CPF-STATUS
                 END-IF
             END-READ
           END-PERFORM
           CLOSE ARQUIVO-CONTAS.

       GERAR-NUMERO-CONTA.
           ADD 1 TO CONTADOR-CONTA
           DISPLAY "NÚMERO DA CONTA: " CONTADOR-CONTA
           MOVE FUNCTION CURRENT-DATE TO DATA-COMPLETA
           MOVE DATA-COMPLETA(7:2) TO DD
           MOVE DATA-COMPLETA(5:2) TO MM
           MOVE DATA-COMPLETA(1:4) TO YYYY
           MOVE DATA-COMPLETA(9:2) TO HH
           MOVE DATA-COMPLETA(11:2) TO MI
           MOVE DATA-COMPLETA(13:2) TO SS.

      *>--CONSULTA DE SALDO-----
       CONSULTAR-SALDO.
           DISPLAY "INSIRA NOME OU CPF DO CLIENTE: "
           ACCEPT CAMPO-BUSCA
           
           IF CAMPO-BUSCA IS NUMERIC
              MOVE CAMPO-BUSCA TO CPF 
              PERFORM BUSCAR-POR-CPF
           ELSE
              MOVE CAMPO-BUSCA TO NOME-CLIENTE
              PERFORM BUSCAR-POR-NOME
           END-IF.
       
       BUSCAR-POR-CPF.
           MOVE "N" TO EOF-FLAG
           MOVE "N" TO FLAG-CPF-STATUS  
           OPEN INPUT ARQUIVO-CONTAS 
           PERFORM UNTIL FIM-ARQUIVO OR CPF-DUPLICADO
               READ ARQUIVO-CONTAS 
                  AT END
                     SET FIM-ARQUIVO TO TRUE 
                  NOT AT END
                     IF ARQ-CPF = CPF 
                     *>CARREGA CAMPOS DE ARQUIVO NAS VARIAVEIS DE 
                     *>TRABALHO
                        MOVE ARQ-NOME TO NOME-CADASTRADO 
                        MOVE ARQ-STATUS TO SITUACAO
                        MOVE ARQ-SALDO TO SALDO 
                        MOVE ARQ-DATA TO DATA-CRIACAO
                        SET CPF-DUPLICADO TO TRUE
                     END-IF
               END-READ
           END-PERFORM
           CLOSE ARQUIVO-CONTAS.
           IF FLAG-CPF-STATUS = "S" 
              *>IF SITUACAO = "ATIVA"
                 DISPLAY "SALDO: " SALDO
              *>ELSE
                 *>DISPLAY "CONTA INATIVA"
              *>END-IF
           ELSE
              DISPLAY "CONTA NÃO ENCONTRADA."
           END-IF.
       BUSCAR-POR-NOME.
           MOVE "N" TO EOF-FLAG
           MOVE "N" TO FLAG-NOME-STATUS  
           OPEN INPUT ARQUIVO-CONTAS 
           PERFORM UNTIL FIM-ARQUIVO OR FLAG-NOME-STATUS = "S" 
               READ ARQUIVO-CONTAS 
                  AT END
                     SET FIM-ARQUIVO TO TRUE 
                  NOT AT END
                     IF ARQ-NOME = NOME-CADASTRADO 
                        *>MOVE "S" TO FLAG-NOME-STATUS
                     *>CARREGA CAMPOS DE ARQUIVO NAS VARIAVEIS DE 
                     *>TRABALHO
                        MOVE ARQ-NOME TO NOME-CADASTRADO 
                        MOVE ARQ-STATUS TO SITUACAO
                        MOVE ARQ-SALDO TO SALDO 
                        MOVE ARQ-DATA TO DATA-CRIACAO
                        SET NOME-ENCONTRADO TO TRUE
                     END-IF
               END-READ
           END-PERFORM
           CLOSE ARQUIVO-CONTAS.
           IF FLAG-NOME-STATUS = "S"  
              IF SITUACAO = "ATIVA"
                 DISPLAY "SALDO: " SALDO
              ELSE
                 DISPLAY "CONTA INATIVA"
              END-IF
           ELSE
               DISPLAY "CONTA NÃO ENCONTRADA."
           END-IF.
      *>DEPOSITO---
       DEPOSITAR.
           DISPLAY "INSIRA NOME OU CPF DO CLIENTE: "
           ACCEPT CAMPO-BUSCA
           
           IF CAMPO-BUSCA IS NUMERIC AND SITUACAO = "ATIVA"
              MOVE CAMPO-BUSCA TO CPF
           ELSE
              MOVE CAMPO-BUSCA TO NOME-CLIENTE
           END-IF

           IF SITUACAO = "ATIVA"
              DISPLAY "INSIRA O VALOR DO DEPOSITO: "
              ACCEPT VALOR-DEP
              IF VALOR-DEP > 0
                 ADD VALOR-DEP TO SALDO
                 OPEN EXTEND ARQUIVO-CONTAS
                     MOVE SALDO TO ARQ-SALDO
                     WRITE REGISTRO-ARQUIVO
                 CLOSE ARQUIVO-CONTAS
                 DISPLAY "DEPOSITO REALIZADO COM SUCESSO."
              ELSE  
                 DISPLAY "VALOR INVÁLIDO PARA DEPÓSITO."
              END-IF
           ELSE
              DISPLAY "CONTA INATIVA OU INEXISTENTE."
           END-IF.