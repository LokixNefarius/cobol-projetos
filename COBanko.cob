       *>-----------------IDENTIFICAÇÃO------------------
	IDENTIFICATION DIVISION.
        PROGRAM-ID.    COBanko.
        AUTHOR.        WESLEY A. M.
       *>-----------------AMBIENTE----------------------
        ENVIRONMENT DIVISION.
       *>------------------DADOS-------------------------
        DATA DIVISION.

        WORKING-STORAGE SECTION.
       *>----------VARIÁVEIS------------
        01 OPCAO-MENU            PIC 9(1).
	01 NOME-CLIENTE          PIC X(30).
	01 CONTADOR-CONTA        PIC 9(6) VALUE ZEROS.
	01 DATA-COMPLETA         PIC X(20).
       *>-------REGISTRO DE DADOS-------
        01 REGISTRO-CONTA.
	   05 NUMERO-CONTA PIC 9(6).
	   05 DATA-CRIACAO.
	      10 DD   PIC 9(2).
	      10 MM   PIC 9(2).
	      10 YYYY PIC 9(4).
           05 NOME-CADASTRADO PIC X(30).
	   05 SITUACAO  PIC X(7).
           05 SALDO        PIC S9(5)V99.
       
        01 STATUS-CONTA PIC X(7).
            88 ATIVA    VALUE 'ATIVA'.
            88 INATIVA  VALUE 'INATIVA'.
	
       *>--------------------LÓGICA----------------------
        PROCEDURE DIVISION.
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
                 DISPLAY "CONSULTANDO SALDO"
	      WHEN 3
                 DISPLAY "SACANDO..."
	      WHEN 4
	         DISPLAY "DEPOSITANDO..."
	      WHEN 5
	         DISPLAY "ENCERRANDO CONTA..."
	      WHEN 6
		 DISPLAY "SAINDO..."
	      WHEN OTHER
	         DISPLAY "OPÇÃO INVALIDA."
           END-EVALUATE.
       *>---PARAGRAFO CRIAR CONTA---
	CRIAR-CONTA.
	   DISPLAY "INSIRA O NOME DO CLIENTE"
	   ACCEPT NOME-CLIENTE
	   MOVE "ATIVA" TO SITUACAO
	   PERFORM GERAR-NUMERO-CONTA
	   MOVE NOME-CLIENTE TO NOME-CADASTRADO
	   DISPLAY "CONTA CRIADA PARA " NOME-CLIENTE "COM STATUS " 
							    SITUACAO
           DISPLAY "DATA DE CRIAÇÃO: " DD "/" MM "/" YYYY.
	   DISPLAY "SALDO: " SALDO.

	GERAR-NUMERO-CONTA.
	   ADD 1 TO CONTADOR-CONTA
           DISPLAY "NÚMERO DA CONTA: " CONTADOR-CONTA
	   MOVE FUNCTION CURRENT-DATE TO DATA-COMPLETA
	   MOVE DATA-COMPLETA(7:2) TO DD
	   MOVE DATA-COMPLETA(5:2) TO MM
	   MOVE DATA-COMPLETA(1:4) TO YYYY.
	   
