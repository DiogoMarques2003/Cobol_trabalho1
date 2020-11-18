      ******************************************************************
      * Author: Arianna Cicero NÂº3, Diogo Marques NÂº7
      * Date: Inicio:05/11/2020 Entraga: 19/11/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. trabalho-gestao.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Arquivo clientes
           SELECT OPTIONAL arquivo-cliente
           ASSIGN to "clientes.dat"
           ORGANIZATION IS SEQUENTIAL.
      *    Arquivo produtos
           SELECT OPTIONAL arquivo-produto 
           ASSIGN TO "produtos.dat"
           ORGANIZATION IS SEQUENTIAL.
      *    Arquivo faturas   
           SELECT OPTIONAL arquivo-fatura 
           ASSIGN TO "faturas.dat"
           ORGANIZATION IS SEQUENTIAL.
      *    Arquivo produtos da fatura
           select OPTIONAL arquivo-produto-fatura
           assign to "produto-fatura.dat"
           organization is SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       fd  arquivo-cliente.
       01  registo-cliente.
           05 registo-cliente-id pic 99.
           05 registo-cliente-nome pic x(50).
           05 registo-cliente-morada pic x(50).
           05 registo-cliente-telefone pic x(12).
           05 registo-cliente-nif pic 9(9).
           05 registo-cliente-descricao pic x(50).
       fd  arquivo-produto.    
       01  registo-produto.
           05 registo-produto-id pic 99.
           05 registo-produto-nome pic x(50).
           05 registo-produto-tipo pic x(50).
              88 compota value "c" "C".
              88 marmelada value "m" "M".
              88 licor value "l" "L".
           05 registo-produto-stock pic 9(3).
           05 registo-produto-descricao pic x(50).    
       fd  arquivo-fatura.
       01  registo-fatura.
           05 registo-fatura-id pic 99.
           05 registo-fatura-dia pic 99.
           05 registo-fatura-mes pic 99.
           05 registo-fatura-ano pic 9999.
           05 registo-fatura-id-cliente pic 99.
           05 registo-fatura-n-produtos pic 9.
           05 registo-fatura-descricao pic x(50).
       fd  arquivo-produto-fatura.
       01  registo-produto-fatura.
           05 registo-produto-fatura-id pic 99.
           05 registo-produto-fatura-id-id pic 99.
           05 registo-produto-fatura-id-prod pic 99.
           05 registo-produto-fatura-qt-prod pic 99.
           
       WORKING-STORAGE SECTION.
      *Variavel para ele dizer se quer introduzir mais dados ou voltar para o menu principal
       01  opcao_continuar pic x value space.
           88 sim value "s" "S".
           88 nao value "n" "N".
      *Estrutura da tabela
       01  tabela.
         02 clientes occurs 10 times.
           03 id-cliente pic 99.
           03 nome-cliente pic x(50).
           03 morada-cliente pic x(50).
           03 telefone-cliente pic x(12).
           03 nif-cliente pic 9(9).
           03 descricao-cliente pic x(50).
         02 faturas occurs 10 times.
           03 id-fatura pic 99.
           03 dia-fatura pic 99.
           03 mes-fatura pic 99.
           03 ano-fatura pic 9999.
           03 id-cliente-fatura pic 99.
           03 n-produtos-fatura pic 9.
           03 produtos occurs 5 times indexed by x.
            04 id-produtos-fatura pic 99.
            04 quantidade-produto-fatura pic 99.
           03 descricao-fatura pic x(50).
         02 produtos occurs 10 times.
           03 id-produto pic 99.
           03 nome-produto pic x(50).
           03 tipo-produto pic x(50).
              88 compota value "c" "C".
              88 marmelada value "m" "M".
              88 licor value "l" "L".
           03 stock-produto pic 9(3).
           03 descricao-produto pic x(50).
      *Variavel para guardar o id dos produtos que temos de alterar/apagar
       77  procurar pic 99 value zero.
      *Variavel para o indice de procurar
       77  indice pic 99 value zero.
      *Variavel da opÃ§Ã£o que o usuario escolhe
       77  opcao pic 99 value zero.
      *variavel para terminar o programa
       77  termina pic 99 value zero.
      *variavel de quantos produtos a fatura vai ter
       77  quant_produtos pic 99 value zero.
      *Variavel do indice dos clientes
       77  indice_cliente pic 99 value zero.
      *Variavel do indice dos produtos
       77  indice_produtos pic 99 value zero.
      *Variavel do indice de compras/faturas
       77  indice_faturas pic 99 value zero.
      *variavel para ver se existe algum campo apagado nas tabelas
       77  apagados pic 99 value zero.
      *variavel de indices
       77  y pic 99 value zero.
      *mais uma variavel de indices.
       77  z pic 99 value zero.
      *variavel para ver se ja lemos todos os dados dos ficheiros
       77  final-arquivo pic x.
       PROCEDURE DIVISION.
           perform menu until termina = 1
           STOP RUN.
       menu.
           display "Menu".
           display "1. Introduzir".
           display "2. Consultar".
           display "3. Alterar".
           display "4. Eliminar".
           display "5. Consultar eliminados".
           display "6. Salvar dados nos ficheiros".
           display "7. Carregar dados dos ficheiros".
           display "0. Sair".
           display "opcao: ".
           accept opcao.
           display "-----------------".
           evaluate true
               when opcao = 1
                   perform menu-introduzir
               when opcao = 2
                   perform menu-consultar
               when opcao = 3
                   perform menu-alterar
               when opcao = 4
                   perform menu-eliminar
               when opcao = 5
                   perform menu-consultarapagados
               when opcao = 6
                    perform salvar-dados
               when opcao = 7
                    perform ler-dados
               when opcao = 0
                   move 1 to termina
               when other
                   display "Opcao invalida"
                   display "-----------------"
           end-evaluate.

       menu-introduzir.
           display "-----------------".
           display "Opcoes de introduzir: "
           display "1 - Clientes"
           display "2 - Produtos"
           display "3 - Faturas"
           display "0 - Cancelar"
           display "Quer introduzir o que: "
           accept opcao
           display "-----------------".
           evaluate true
               when opcao = 1
                   if indice_cliente = 10 then
                       display "Limite atingido pela tabela"
                   else
                       perform introduzir-c
               when opcao = 2
                   if indice_produtos = 10 then
                       display "Limite atingido pela tabela"
                   else
                       perform introduzir-p
               when opcao = 3
                   if indice_faturas = 10 then
                       display "Limite atingido pela tabela"
                   else
                       perform introduzir-f
                when opcao = 0
                   perform menu
               when other
                   display "opcao invalida"
           end-evaluate.

       menu-consultar.
           display "-----------------".
           display "Opcoes de consultar: "
           display "1 - Clientes"
           display "2 - Produtos"
           display "3 - Faturas"
           display "0 - Cancelar"
           display "Quer consultar o que: "
           accept opcao
           display "-----------------".
           evaluate true
           when opcao = 1
                if indice_cliente = 0 then
                     display "A tabela cliente esta vazia"
                else
                    move 0 to apagados
                    perform varying indice from 1 by 1 until indice >
                       indice_cliente
                       if descricao-cliente(indice) not equal to
                       "apagado"
                       add 1 to apagados
                    end-perform
                    if apagados > 0
                       perform consultar-c
                    else
                       display "A tabela cliente esta vazia"
                when opcao = 2
                    if indice_produtos = 0
                        display "A tabela produtos esta vazia"
                    else
                     move 0 to apagados
                     perform varying indice from 1 by 1 until indice >
                        indice_produtos
                        if descricao-produto(indice) not equal to
                        "apagado"
                        add 1 to apagados
                     end-perform
                     if apagados > 0
                        perform consultar-p
                     else
                       display "A tabela produtos esta vazia"
                when opcao = 3
                    if indice_faturas = 0
                       display "A tabela faturas esta vazia"
                    else
                    move 0 to apagados
                     perform varying indice from 1 by 1 until indice >
                        indice_faturas
                        if descricao-fatura(indice) not equal to
                        "apagado"
                        add 1 to apagados
                     end-perform
                     if apagados > 0
                        perform consultar-f
                     else
                       display "A tabela faturas esta vazia"
                when opcao = 0
                   perform menu
               when other
                   display "opcao invalida"
           end-evaluate.

       menu-alterar.
           display "-----------------".
           display "Opcoes de alterar: "
           display "1 - Clientes"
           display "2 - Produtos"
           display "3 - Faturas"
           display "0 - Cancelar"
           display "Quer alterar o que: "
           accept opcao
           display "-----------------".
           evaluate true
           when opcao = 1
                if indice_cliente = 0 then
                     display "A tabela cliente esta vazia"
                else
                    perform alterar-c
                when opcao = 2
                    if indice_produtos = 0
                        display "A tabela produtos esta vazia"
                    else
                        perform alterar-p
                when opcao = 3
                    if indice_faturas = 0
                       display "A tabela faturas esta vazia"
                    else
                        perform alterar-f
                when opcao = 0
                   perform menu
               when other
                   display "opcao invalida"
           end-evaluate.

       menu-eliminar.
           display "-----------------".
           display "Opcoes de eleminar: "
           display "1 - Clientes"
           display "2 - Produtos"
           display "3 - Faturas"
           display "0 - Cancelar"
           display "Quer eliminar o que: "
           accept opcao
           display "-----------------".
           evaluate true
           when opcao = 1
                if indice_cliente = 0 then
                     display "A tabela cliente esta vazia"
                else
                    perform eliminar-c
                when opcao = 2
                    if indice_produtos = 0
                        display "A tabela produtos esta vazia"
                    else
                        perform eliminar-p
                when opcao = 3
                    if indice_faturas = 0
                       display "A tabela faturas esta vazia"
                    else
                        perform eliminar-f
                when opcao = 0
                   perform menu
               when other
                   display "opcao invalida"
           end-evaluate.

       menu-consultarapagados.
           display "-----------------".
           display "Opcoes de consultar os apagados: "
           display "1 - Clientes"
           display "2 - Produtos"
           display "3 - Faturas"
           display "0 - Cancelar"
           display "Quer consultar que apagados: "
           accept opcao
           display "-----------------".
           evaluate true
           when opcao = 1
                if indice_cliente = 0 then
                     display "A tabela cliente esta vazia"
                else
                    move 0 to apagados
                    perform varying indice from 1 by 1 until indice >
                       indice_cliente
                       if descricao-cliente(indice) equal to "apagado"
                       add 1 to apagados
                    end-perform
                    if apagados > 0
                       perform consultarapagados-c
                    else
                       display "Nao existe nenhum cliente apagado"
                when opcao = 2
                    if indice_produtos = 0
                        display "A tabela produtos esta vazia"
                    else
                    move 0 to apagados
                    perform varying indice from 1 by 1 until indice >
                       indice_produtos
                       if descricao-produto(indice) equal to "apagado"
                       add 1 to apagados
                    end-perform
                    if apagados > 0
                        perform consultarapagados-p
                    else
                       display "Nao existe nenhum produto apagado"
                when opcao = 3
                    if indice_faturas = 0
                       display "A tabela faturas esta vazia"
                    else
                       move 0 to apagados
                       perform varying indice from 1 by 1 until indice >
                          indice_faturas
                          if descricao-fatura(indice) equal to
                           "apagado"
                          add 1 to apagados
                       end-perform
                       if apagados > 0
                           perform consultarapagados-f
                       else
                           display "Nao existe nenhuma fatura apgada"
                when opcao = 0
                   perform menu
               when other
                   display "opcao invalida"
           end-evaluate.

       introduzir-c.
           add 1 to indice_cliente.
           move indice_cliente to id-cliente(indice_cliente).
           display "-----------------".
           display "Id do cliente: " indice_cliente.
           display "Escreva o nome do cliente: "
           accept nome-cliente(indice_cliente).
           display "Escreva a morada do cliente: ".
           accept morada-cliente(indice_cliente).
           display "Escreva o telemovel do cliente: ".
           accept telefone-cliente(indice_cliente).
           display "Escreva o NIF do cliente: ".
           accept nif-cliente(indice_cliente).
           move space to descricao-cliente(indice_cliente).
           display "Queres introduzir mais algum cliente?(S/N)".
           accept opcao_continuar.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao"
              accept opcao_continuar
           end-perform.
           if sim then
              perform introduzir-c
           end-if.
           display "-----------------".

       introduzir-p.
           add 1 to indice_produtos.
           move indice_produtos to id-produto(indice_produtos).
           display "-----------------".
           display "Id do produto: " indice_produtos.
           display "Escreva o nome do produto: ".
           accept nome-produto(indice_produtos).
           display "Qual e o tipo do produto(l/m/c): ".
           accept tipo-produto(indice_produtos).
           perform until (tipo-produto(indice_produtos) = "l" or
                          tipo-produto(indice_produtos) = "L" or
                          tipo-produto(indice_produtos) = "m" or
                          tipo-produto(indice_produtos) = "M" or
                          tipo-produto(indice_produtos) = "c" or
                          tipo-produto(indice_produtos) = "C")
                display "ERRO - Tipo de produto invalido."
                display "Volta a introduzir o tipo de produto(l/m/c): "
                accept tipo-produto(indice_produtos)
           end-perform.
           display "Qual e o stock do produto?: ".
           accept stock-produto(indice_produtos).
           move space to descricao-produto(indice_produtos).
           display "Queres introduzir mais algum produto?(S/N)".
           accept opcao_continuar.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao"
              accept opcao_continuar
           end-perform.
           if sim then
              perform introduzir-p
           end-if.
           display "-----------------".

       introduzir-f.
           add 1 to indice_faturas.
           move indice_faturas to id-fatura(indice_faturas)
           display "-----------------".
           display "Id da fatura: " indice_faturas.
           display "Escreva o dia da fatura: ".
           accept dia-fatura(indice_faturas).
           perform until (dia-fatura(indice_faturas) > 0 and 
           dia-fatura(indice_faturas) < 32)
              display "Introduziste um dia invalido"
              display "envia 0 para cancelar"
              display "Qual e o dia da fatura?: "
              accept dia-fatura(indice_faturas)
              if dia-fatura(indice_faturas) equal to 0
                compute indice_faturas = indice_faturas - 1
                perform menu
           end-perform.
           display "Escreva o mes da fatura: ".
           accept mes-fatura(indice_faturas).
           perform until (mes-fatura(indice_faturas) > 0 and 
           mes-fatura(indice_faturas) < 13)
              display "O mes que introduziste nao existe"
              display "envia 0 para cancelar"
              display "Qual e o mes da fatura?: "
              accept mes-fatura(indice_faturas)
              if mes-fatura(indice_faturas) equal to 0
                compute indice_faturas = indice_faturas - 1
                perform menu
           END-PERFORM.
           display "Escreva o ano da fatura: ".
           accept ano-fatura(indice_faturas).
           perform until (ano-fatura(indice_faturas) > 2009 and 
           ano-fatura(indice_faturas) < 2031)
              display "O ano que introduziste nao existe"
              display "envia 0 para cancelar"
              display "Qual e o ano da fatura?: "
              accept ano-fatura(indice_faturas)
              if ano-fatura(indice_faturas) equal to 0
                compute indice_faturas = indice_faturas - 1
                perform menu
           END-PERFORM.
           display "Qual e o id do cliente?: ".
           accept id-cliente-fatura(indice_faturas)
           perform until (id-cliente-fatura(indice_faturas) <=
           indice_cliente and descricao-cliente(id-cliente-fatura
           (indice_faturas)) not equal to 'apagado')
              display "Este cliente nao existe"
              display "envia 0 para cancelar"
              display "Qual e o id do cliente da fatura?: "
              accept id-cliente-fatura(indice_faturas)
              if id-cliente-fatura(indice_faturas) equal to 0
                compute indice_faturas = indice_faturas - 1
                perform menu
           end-perform.
           display "Quantos produtos a fatura tem: ".
           accept quant_produtos.
           perform until (quant_produtos >= 1 and quant_produtos <= 5)
              display "Erro - Quantidade de produtos invalida(1-5)."
              display "Envia 0 para cancelar a introducao da fatura"
              display "Volta a introduzir a quantidade de produtos que"
    -        " fatura vai ter:"
              accept quant_produtos
              if quant_produtos equal to 0
                compute indice_faturas = indice_faturas - 1
                perform menu
           end-perform.
           move quant_produtos to n-produtos-fatura(indice_faturas).
           perform varying x from 1 by 1 until x > quant_produtos
              display "Qual e o id do produto?: "
              accept id-produtos-fatura(indice_faturas,x)
              perform until (id-produtos-fatura(indice_faturas,x) <=
                indice_produtos and descricao-produto(id-produtos-fatura
                 (indice_faturas,x)) not equal to 'apagado')
                 display "Esse produto nao existe ou foi apagado"
                 display "Para cancelar envia 0"
                 display "Qual e o id do produto?:"
                 accept id-produtos-fatura(indice_faturas,x)
                 if id-produtos-fatura(indice_faturas,x) equal to 0
                    compute indice_faturas = indice_faturas - 1
                    perform menu
              end-perform
              display "Qual e a quantidade do produto): "
              accept quantidade-produto-fatura(indice_faturas,x)
              perform until (quantidade-produto-fatura(indice_faturas,x)
                            < 0 or stock-produto(id-produtos-fatura
                            (indice_faturas,x)) >=
                            quantidade-produto-fatura(indice_faturas,x))
                 display "Erro - Quantidade de produtos invalida."
                 display "Envia 0 para cancelar a introducao da fatura"
                 display "Volta a introduzir a quantidade de produtos "
                 "que fatura vai ter:"
                 accept quantidade-produto-fatura(indice_faturas,x)
                 if quantidade-produto-fatura(indice_faturas,x)
                    equal to 0
                    compute indice_faturas = indice_faturas - 1
                    perform menu
              end-perform
              compute stock-produto(id-produtos-fatura
              (indice_faturas,x)) = stock-produto(id-produtos-fatura
              (indice_faturas,x)) - quantidade-produto-fatura
              (indice_faturas,x)
           end-perform.
           move space to descricao-produto(indice_produtos).
            display "Queres introduzir mais alguma fatura?(S/N)".
           accept opcao_continuar.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao"
              accept opcao_continuar
           end-perform.
           if sim then
              perform introduzir-f
           end-if.

       consultar-c.
           display "-----------------".
           display "Dados dos clientes: ".
           perform varying indice from 1 by 1 until indice >
                indice_cliente
                if (descricao-cliente(indice) not EQUAL TO "apagado")
                display "Id: " id-cliente(indice)
                display "Nome: " nome-cliente(indice)
                display "Morada: " morada-cliente(indice)
                display "Telefone: " telefone-cliente(indice)
                display "Nif: " nif-cliente(indice)
                display "-----------------"
           end-perform.

       consultar-p.
           display "-----------------".
           display "Dados dos produtos: ".
           perform varying indice from 1 by 1 until indice >
                indice_produtos
                if (descricao-produto(indice) not EQUAL TO "apagado")
                display "Id de produto: " id-produto(indice)
                display "Nome do produto: " nome-produto(indice)
                display "Tipo de produto: " tipo-produto(indice)
                display "Stock do produto: " stock-produto(indice)
                display "-----------------"
           end-perform.

       consultar-f.
           display "-----------------".
           display "Dados das faturas: ".
           perform varying indice from 1 by 1 until indice >
                indice_faturas
                if (descricao-produto(indice) not EQUAL TO "apagado")
                display "Id produto: "id-fatura(indice)
                display "Data da fatura: "dia-fatura(indice)"/"
                mes-fatura(indice)"/" ano-fatura(indice)
                display "Nome do cliente: " nome-cliente(
                id-cliente-fatura(indice))
                display "Morada do cliente: " morada-cliente(
                id-cliente-fatura(indice))
               display "Numero de produtos da fatura: "
                       n-produtos-fatura(indice)
                display "Produtos da fatura: "
                perform varying y from 1 by 1 until y >
                n-produtos-fatura(indice)
                    display "   Nome: " nome-produto(id-produtos-fatura
                    (indice,y))
                    ", Quantidade: "quantidade-produto-fatura(indice,y)
                end-perform
                display "-----------------"
           end-perform.

       alterar-c.
           display "-----------------".
           display "Qual e o id do cliente que quer alterar os dados".
           accept procurar.
           perform until (procurar <= indice_cliente and
                descricao-cliente(procurar) not equal to "apagado")
              display "Erro - esse cliente nao existe ou foi apagado"
              display "Se quiseres cancelar escreve 0"
              display "volta a introduzir outro id de cliente: "
              accept procurar
              if procurar equal to 0
                 perform menu
              end-if
           END-PERFORM.
           display "Insira o nome do cliente: "
           accept nome-cliente(procurar)
           display "Insira a morada do cliente: "
           accept morada-cliente(procurar).
           display "Insira o telemovel no cliente: "
           accept telefone-cliente(procurar).
           display "Insira o NIF do cliente: "
           accept nif-cliente(procurar).
           display "-----------------".

       alterar-p.
           display "-----------------".
           display "Qual e o id do produto que quer alterar os dados".
           accept procurar.
           perform until (procurar <= indice_produtos and
                descricao-produto(procurar) not equal to "apagado")
              display "Erro - esse produto nao existe ou foi apagado"
              display "Se quiseres cancelar escreve 0"
              display "volta a introduzir outro id de produto"
              accept procurar
              if procurar equal to 0
                 perform menu
              end-if
           END-PERFORM.
           display "Escreva o nome do produto: ".
           accept nome-produto(procurar).
           display "Qual e o tipo do produto(l/m/c): ".
           accept tipo-produto(procurar).
           perform until (tipo-produto(procurar) = "l" or
                          tipo-produto(procurar) = "L" or
                          tipo-produto(procurar) = "m" or
                          tipo-produto(procurar) = "M" or
                          tipo-produto(procurar) = "c" or
                          tipo-produto(procurar) = "C")
                display "ERRO - Tipo de produto invalido."
                display "Volta a introduzir o tipo de produto(l/m/c): "
                accept tipo-produto(procurar)
           end-perform.
           display "Qual e o stock do produto?: ".
           accept stock-produto(procurar).
           display "-----------------".

       alterar-f.
           display "-----------------".
           display "Qual e o id da fatura que quer alterar os dados".
           accept procurar.
           perform until (procurar <= indice_faturas and
                descricao-fatura(procurar) not equal to "apagado")
              display "Erro - essa fatura nao existe ou foi apagado"
              display "Se quiseres cancelar escreve 0"
              display "volta a introduzir outro id da fatura"
              accept procurar
              if procurar equal to 0
                 perform menu
              end-if
           END-PERFORM.
           perform varying y from 1 by 1 until y >
                n-produtos-fatura(indice)
                compute stock-produto(id-produtos-fatura(indice,y)) =
                stock-produto(id-produtos-fatura(indice,y)) +
                quantidade-produto-fatura(indice,y)
           end-perform.
           display "Escreva o dia da fatura: ".
           accept dia-fatura(procurar).
           display "Escreva o mes da fatura: ".
           accept mes-fatura(procurar).
           display "Escreva o ano da fatura: ".
           accept ano-fatura(procurar).
           display "Qual e o id do cliente?: ".
           accept id-cliente-fatura(procurar)
           perform until (id-cliente-fatura(procurar) <
           indice_cliente or descricao-cliente(id-cliente-fatura
           (procurar)) not equal to 'apagado')
              display "Este cliente nao existe"
              display "envia 0 para cancelar"
              display "Qual e o cliente da fatura? "
              accept id-cliente-fatura(procurar)
              if id-cliente-fatura(procurar) equal to 0
                perform menu
           end-perform.
           display "Quantos produtos a fatura tem: ".
           accept quant_produtos.
           perform until (quant_produtos >= 1 and quant_produtos <= 5)
              display "Erro - Quantidade de produtos invalida(1-5)."
              display "Envia 0 para cancelar a introducao da fatura"
              display "Volta a introduzir a quantidade de produtos que"
    -        " fatura vai ter:"
              accept quant_produtos
              if quant_produtos equal to 0
                perform menu
           end-perform.
           move quant_produtos to n-produtos-fatura(procurar).
           perform varying x from 1 by 1 until x > quant_produtos
              display "Qual e o id do produto?: "
              accept id-produtos-fatura(procurar,x)
              perform until (id-produtos-fatura(procurar,x) <
                 indice_produtos or descricao-produto(id-produtos-fatura
                 (procurar,x)) not equal to 'apagado')
                 display "Esse produto nÃ£o existe ou foi apagado"
                 display "Para cancelar envia 0"
                 display "Qual e o id do produto?:"
                 accept id-produtos-fatura(procurar,x)
                 if id-produtos-fatura(procurar,x) equal to 0
                    perform menu
              end-perform
              display "Qual e a quantidade do produto): "
              accept quantidade-produto-fatura(procurar,x)
              perform until (quantidade-produto-fatura(procurar,x)
                            < 0 or stock-produto(id-produtos-fatura
                            (procurar,x)) >=
                            quantidade-produto-fatura(procurar,x))
                 display "Erro - Quantidade de produtos invalida."
                 display "Envia 0 para cancelar a introducao da fatura"
                 display "Volta a introduzir a quantidade de produtos "
                 "que fatura vai ter:"
                 accept quantidade-produto-fatura(procurar,x)
                 if quantidade-produto-fatura(procurar,x)
                    equal to 0
                    perform menu
              end-perform
              compute stock-produto(id-produtos-fatura
              (procurar,x)) = stock-produto(id-produtos-fatura
              (procurar,x)) - quantidade-produto-fatura
              (procurar,x)
           end-perform.
           display "-----------------".

       eliminar-c.
           display "-----------------".
           display "Qual e o id do cliente que quer apagar".
           accept procurar.
           perform until (procurar <= indice_cliente and
                descricao-cliente(procurar) not equal to "apagado")
              display "Erro - esse cliente nao existe ou ja foi "
              "apagado"
              display "Se quiseres cancelar escreve 0"
              display "volta a introduzir outro id de cliente"
              accept procurar
              if procurar equal to 0
                 perform menu
              end-if
           END-PERFORM.
           move "apagado" to descricao-cliente(procurar).
           display "Cliente apagado com sucesso"
           display "-----------------".

       eliminar-p.
           display "-----------------".
           display "Qual e o id do produto que quer apagar os dados".
           accept procurar.
           perform until (procurar <= indice_produtos and
                descricao-produto(procurar) not equal to "apagado")
              display "Erro - esse produto nao existe ou ja foi "
              "apagado"
              display "Se quiseres cancelar escreve 0"
              display "volta a introduzir outro id do produto"
              accept procurar
              if procurar equal to 0
                 perform menu
              end-if
           END-PERFORM.
           move "apagado" to descricao-produto(procurar).
           display "Produto apagado com sucesso"
           display "-----------------".

       eliminar-f.
           display "-----------------".
           display "Qual e o id da fatura que quer apagar".
           accept procurar.
           perform until (procurar <= indice_faturas and
                descricao-fatura(procurar) not equal to "apagado")
              display "Erro - essa fatura nao existe ou ja foi apagado"
              display "Se quiseres cancelar escreve 0"
              display "volta a introduzir outro id da fatura"
              accept procurar
              if procurar equal to 0
                 perform menu
              end-if
           END-PERFORM.
           perform varying y from 1 by 1 until y >
                n-produtos-fatura(indice)
                compute stock-produto(id-produtos-fatura(indice,y)) =
                stock-produto(id-produtos-fatura(indice,y)) +
                quantidade-produto-fatura(indice,y)
           end-perform.
           move "apagado" to descricao-fatura(procurar)
           display "Fatura apagada com sucesso"
           display "-----------------".

       consultarapagados-c.
           display "-----------------".
           display "Dados dos clientes apagados: ".
           perform varying indice from 1 by 1 until indice >
                indice_cliente
                if (descricao-cliente(indice) EQUAL TO "apagado")
                display "Id: " id-cliente(indice)
                display "Nome: " nome-cliente(indice)
                display "Morada: " morada-cliente(indice)
                display "Telefone: " telefone-cliente(indice)
                display "Nif: " nif-cliente(indice)
                display "-----------------"
           end-perform.
           display "-----------------".

       consultarapagados-p.
           display "-----------------".
           display "Dados dos produtos apagados: ".
           perform varying indice from 1 by 1 until indice >
                indice_produtos
                if (descricao-produto(indice) EQUAL TO "apagado")
                display "Id de produto: " id-produto(indice)
                display "Nome do produto: " nome-produto(indice)
                display "Tipo de produto: " tipo-produto(indice)
                display "Stock do produto: " stock-produto(indice)
                display "-----------------"
           end-perform.
           display "-----------------".

       consultarapagados-f.
           display "-----------------".
           display "Dados das faturas apagadas: ".
           perform varying indice from 1 by 1 until indice >
                indice_faturas
                if (descricao-produto(indice) not EQUAL TO "apagado")
                display "Id produto: "id-fatura(indice)
                display "Data da fatura: "dia-fatura(indice)"/"
                mes-fatura(indice)"/" ano-fatura(indice)
                display "Nome do cliente: " nome-cliente(
                id-cliente-fatura(indice))
                display "Morada do cliente: " morada-cliente(
                id-cliente-fatura(indice))
               display "Numero de produtos da fatura: "
                       n-produtos-fatura(indice)
                display "Produtos da fatura: "
                perform varying y from 1 by 1 until y >
                n-produtos-fatura(indice)
                    display "   Nome: " nome-produto(id-produtos-fatura
                    (indice,y))
                    ", Quantidade: "quantidade-produto-fatura(indice,y)
                end-perform
                display "-----------------"
           end-perform.
           display "-----------------".

       salvar-dados.
           display "Ao realizar esta opcao os dados contidos nos "
           "ficheiros serao apagados. Queres mesmo realizar esta opcao?"
           "(S/N)"
           accept opcao_continuar
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao"
              accept opcao_continuar
           end-perform.
           if sim then
              delete arquivo-cliente.
              delete arquivo-fatura.
              delete arquivo-produto.
              delete arquivo-produto-fatura.
              display "Salvando clientes...".
              perform salvar-cliente.
              display "Clientes salvos com sucesso".
              display "Salvando produtos...".
              perform salvar-produto.
              display "Produtos salvos com sucesso".
              display "salvando faturas...".
              perform salvar-fatura.
              display "Faturas salvas com sucesso".
              display "Todos os dados foram salvos.".
              display "-----------------".

       ler-dados.    
           display "Carregando os clientes...".
           perform ler-clientes.
           display "Clientes carregados com sucesso".
           display "Carregando os produtos...".
           perform ler-produtos.
           display "Produtos carregados com sucesso".
           display "Carregando a primeira parte das faturas...".
           perform ler-faturas.
           display "Carregando a segunda parte das faturas...".
           perform ler-produtos-faturas.
           display "Faturas carregadas com sucesso".
           display "Todos os dados foram carregados com sucesso".
           display "-----------------".

       salvar-cliente.
           open extend arquivo-cliente.
           perform varying indice from 1 by 1 until indice >
                indice_cliente
                move id-cliente(indice) to registo-cliente-id
                move nome-cliente(indice) to registo-cliente-nome 
                move  morada-cliente(indice) to registo-cliente-morada
                move telefone-cliente(indice) to 
                registo-cliente-telefone 
                move nif-cliente(indice) to registo-cliente-nif
                move descricao-cliente(indice) to 
                registo-cliente-descricao
                write registo-cliente
           end-perform.
           close arquivo-cliente.

       salvar-produto.
           open extend arquivo-produto.
           perform varying indice from 1 by 1 until indice >
                indice_produtos
                move id-produto(indice) to registo-produto-id
                move nome-produto(indice) to registo-produto-nome
                move tipo-produto(indice) to registo-produto-tipo 
                move stock-produto(indice) to registo-produto-stock 
                move descricao-produto(indice) to 
                registo-produto-descricao
                write registo-produto
           end-perform.
           close arquivo-produto.

       salvar-fatura.
           open extend arquivo-fatura.
           perform varying indice from 1 by 1 until indice >
                indice_faturas
                move id-fatura(indice) to registo-fatura-id
                move dia-fatura(indice) to registo-fatura-dia
                move mes-fatura(indice) to registo-fatura-mes
                move ano-fatura(indice) to registo-fatura-ano
                move id-cliente-fatura(indice) to 
                registo-fatura-id-cliente 
                move n-produtos-fatura(indice) to 
                registo-fatura-n-produtos
                open extend arquivo-produto-fatura
                perform varying y from 1 by 1 until y >
                n-produtos-fatura(indice)
                    move id-fatura(indice) to registo-produto-fatura-id
                    move y to registo-produto-fatura-id-id 
                    move id-produtos-fatura(indice,y) to 
                    registo-produto-fatura-id-prod
                    move quantidade-produto-fatura(indice,y) to 
                    registo-produto-fatura-qt-prod
                    write registo-produto-fatura
                end-perform
                close arquivo-produto-fatura
                move descricao-fatura(indice) to 
                registo-fatura-descricao
                write registo-fatura
           end-perform.
           close arquivo-fatura.

       ler-clientes.
           open input arquivo-cliente.
           move "n" to final-arquivo.
           move 0 to y.
           perform leia-proximo-cliente.
           perform exiba-cliente
               until final-arquivo = "s".
           close arquivo-cliente.

       leia-proximo-cliente.
           read arquivo-cliente record at end move "s" to final-arquivo.

       exiba-cliente.
           perform guardar-cliente.
           perform leia-proximo-cliente.

       guardar-cliente.
           add 1 to y.
           move y to indice_cliente.
           move registo-cliente-id  to id-cliente(y).
           move registo-cliente-nome to nome-cliente(y).
           move registo-cliente-morada to morada-cliente(y).
           move registo-cliente-telefone to telefone-cliente(y).
           move registo-cliente-nif to nif-cliente(y).
           move registo-cliente-descricao to descricao-cliente(y).

       ler-produtos.
           open input arquivo-produto.
           move "n" to final-arquivo.
           move 0 to y.
           perform leia-proximo-produto.
           perform exiba-produto
               until final-arquivo = "s".
           close arquivo-produto.

       leia-proximo-produto.
           read arquivo-produto record at end move "s" to final-arquivo.

       exiba-produto.
           perform guardar-produto.
           perform leia-proximo-produto.

       guardar-produto.
           add 1 to y.
           move y to indice_produtos.
           move registo-produto-id to id-produto(y).
           move registo-produto-nome to nome-produto(y).
           move registo-produto-tipo to tipo-produto(y).
           move registo-produto-stock to stock-produto(y).
           move registo-produto-descricao to descricao-produto(y).

       ler-faturas.
           open input arquivo-fatura.
           move "n" to final-arquivo.
           move 0 to y.
           perform leia-proxima-fatura.
           perform exiba-fatura
               until final-arquivo = "s".
           close arquivo-fatura.

       leia-proxima-fatura.
           read arquivo-fatura record at end move "s" to final-arquivo.

       exiba-fatura.
           perform guardar-fatura.
           perform leia-proxima-fatura.

       guardar-fatura.
           add 1 to y.
           move y to indice_faturas.
           move registo-fatura-id to id-fatura(y).
           move registo-fatura-dia to dia-fatura(y).
           move registo-fatura-mes to mes-fatura(y).
           move registo-fatura-ano to ano-fatura(y).
           move registo-fatura-id-cliente to id-cliente-fatura(y).
           move registo-fatura-n-produtos to n-produtos-fatura(y).
           move registo-fatura-descricao to descricao-fatura(y).

       ler-produtos-faturas.
           open input arquivo-produto-fatura. 
           move "n" to final-arquivo.
           move 0 to y.
           perform leia-proximo-produto-fatura.
           perform exiba-produto-fatura
               until final-arquivo = "s".
           close arquivo-produto-fatura.

       leia-proximo-produto-fatura.
           read arquivo-produto-fatura record at end move "s" to 
           final-arquivo.

       exiba-produto-fatura.
           perform guardar-produto-fatura.
           perform leia-proximo-produto-fatura.

       guardar-produto-fatura.
           move registo-produto-fatura-id to y.
           move registo-produto-fatura-id-id to z.
           move registo-produto-fatura-id-prod to 
           id-produtos-fatura(y,z).
           move registo-produto-fatura-qt-prod to
           quantidade-produto-fatura(y,z).

       END PROGRAM trabalho-gestao.
