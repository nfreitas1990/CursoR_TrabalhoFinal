

# Script - Trabalho Final
# Natália - Maio 2022

# Carregar Pacotes
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)



# Carregar os Dados
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")


# DÚVIDA: Não consegui salvar no meu pc e versionar o arquivo quando em .rds dizia que o arquivo estava muito grande para versionar no github. Qual seria a melhor estratégia nesse caso? usar qual extensão para salvar o arquivo de dados? 


#--------------
# Problemática: 
#--------------
# Um investidor contratou um cinestra para produzir um filme e deseja ter o maior retorno possível do seu investimento.Enquanto o cineastra contratado deseja que o seu filme seja um sucesso. 

# ESTRATÉGIA QUE VOU ADOTAR:
# Identificar a melhor estratégia de investimento para que ambos tenham o seu desejo realizado.
# Para o investidor o que importa é, principalmente, o orçamento e receita. Para o investidor, se não ouver relação entre a nota imdb e o lucro do filme, nao interessa olhar para a nota.
# Para o cineastra o que interessa é que o filme seja bem avaliado, tanto pela cítica quanto pelos fãs, e que haja grande numero de avaliações.
# Será que existe um tipo de filme, que tenha uma chance maior de atender simultâneamente os anseios lucrativos e de visibilidade que cada um possui?

# INVESTIGAR:
# 1. Retorno Financeiro:
#         - Qual o lucro de cada filme?
#         - Qual tipo de filme produz o maior retorno financeiro?
#         - O retorno financeiro é proprocional ao investimento?
#         - Existe relação entre o retorno financeiro e a nota imdb?


# 2. Como o cinestra pode ficar famoso?
#         - qual tipo de filme com maior numero de avaliações?
#         - qual tipo de filme com maior nota?
#         - qual o filme foi melhor aceito pela critica e pelo publico?



# VISUALIZAR DADOS --------------------------------------------------------
# Objetivo: Identificar Problemas na base de dados 


# Checar o tipo das variáveis

glimpse(imdb)   # data_lancamento: está como character
                # data_lancamento: incompleta - faltando dia e mes em aguns filmes
                # ano: como double
                # duracao: como double
                # orcamento| receita | receit_eua: como character
                # orcamento tem muitos numeros - dificil visualizar
                # não tem coluna com o lucro
                # data_lancamento: não tem dias separados


# Acertar Dados -----------------------------------------------------------


#---    
## Datas
      
      # data_lancamento:criar coluna de mes e dia | Mudar tipo para date
          imdb <- imdb %>% 
                        mutate(data_lancamento = ymd(data_lancamento)) %>% 
                        separate(col = data_lancamento,
                                 into = c("ano_date","mes","dia"),  
                                 sep = "-", remove = FALSE) %>% 
                        select(-ano_date)
          
          class(imdb$data_lancamento)


      # ano: Mudar tipo
        
           imdb <- imdb %>% 
                    mutate(across(
                      .cols = c(ano, mes, dia),
                      .fns = as.factor))
          
          class(imdb$ano)
          class(imdb$mes)
          class(imdb$dia)
      

       
#---        
## Dinheiro  
       
      # orcamento:Tirando o Caractere da moeda para transformar para numero e calcular o lucro
        imdb <-imdb %>% 
                   separate(col = orcamento,
                            into = c("moeda_orcamento","orcamento"),  
                            sep = " ", remove = FALSE) %>% 
                   mutate(orcamento = as.double(orcamento)) 
       
      
      # receita
       imdb <-imdb %>% 
                   separate(col = receita,
                            into = c("moeda_receita","receita"),  
                            sep = " ", remove = FALSE) %>% 
                   mutate(receita = as.double(receita))        
       
      
      # receita_eua
       imdb <-imdb %>% 
                   separate(col = receita_eua,
                            into = c("moeda_receita_eua","receita_eua"),  
                            sep = " ", remove = FALSE) %>% 
                   mutate(receita_eua = as.double(receita_eua)) 
       
      
      # lucro: 
       
       
# QUESTÃO 3   # Liste todas as moedas que aparecem nas colunas `orcamento` e `receita 
              # Obj: Visualizar qnt de dados q vou deixar de fora ao considerar somente dolar
       
       # Moeda - Receita EUA : 5 registros != dolar
       imdb %>% 
         group_by(moeda_receita_eua) %>% 
         drop_na(moeda_receita_eua) %>% 
         summarise(moeda = table(moeda_receita_eua)) %>% 
         arrange(desc(moeda))
       
       # Moeda - Receita: 61 registros != dolar
       imdb %>% 
         group_by(moeda_receita) %>% 
         drop_na(moeda_receita) %>% 
         summarise(moeda = table(moeda_receita)) %>% 
         arrange(desc(moeda)) 
       
       
       # Moeda - Orcamento : 7108 registros != dolar  (ainda temos o dobro com moeda dolar, vou seguir, apesar de sair mtos filmes. Talvez eu volte e tente transformar as moedas para dólar depois)
       imdb %>% 
         group_by(moeda_orcamento) %>% 
         drop_na(moeda_orcamento) %>% 
         summarise(moeda = table(moeda_orcamento)) %>% 
         arrange(desc(moeda)) %>% 
         summarise(sum(moeda)-16602)    # soma da coluna moedas - (qnt do que é = dolar)
       
       
       
      # Resolvi criar nova base de dados para trabalhar o dinheiro pq quero apagar colunas das moedas e não quero perder o imdb original. Além disso, vou filtrar e excluir os filmes que não são em dolar, para não  transformar as moedas agora.
       
       
       imdb_lucrodolar <-   imdb %>% 
                               #select(titulo, 
                               #moeda_orcamento,orcamento, 
                               #moeda_receita, receita, 
                               #moeda_receita_eua,receita_eua) %>% 
                               drop_na(orcamento) %>% 
                               filter(moeda_orcamento == "$" & 
                                        moeda_receita == "$"&
                                        moeda_receita_eua == "$") 
       
       
       # Comparar as duas receitas para ver se são a mesma coisa
       # Não são a mesma coisa. E agora, qual usar? 
       table(imdb_lucrodolar$receita == imdb_lucrodolar$receita_eua) 
       
       
       # Parece que a "receita" é sempre maior do que a "receita_eua"
       # Vou usar a "receita" que parece ser a receita total do filme
       # E assumir que a "receita_eua" é a receita somente no eua
       imdb_lucrodolar %>% 
                       mutate(diferenca_receita = receita - receita_eua) %>% # p inferir qual é a total
                       filter(diferenca_receita < 0) %>% 
                       view()
       
       
       # Criando coluna de lucro na base
       # Colocando lucro para a escala de milhões pq tem mto numero
       # Apagando a coluna das moedas, pois já filtramos para dolar ao criar imdb_lucrodolar
       
       imdb_lucrodolar <- imdb_lucrodolar %>% 
                                           mutate(lucro_dolar = receita - orcamento) %>% 
                                           mutate(lucro_mi = lucro_dolar/1000000) %>%
                                           mutate(orcamento_mi = orcamento/1000000) %>% 
                                           mutate(receita_mi = receita/1000000) %>% 
                                           select(- c(moeda_orcamento,moeda_receita,
                                                      moeda_receita_eua, receita_eua)) 
       
 
  
  # Identificar valores faltantes    

    for (i in 1:length(imdb)){
        resultado <- table(is.na(imdb[i]))
        print(c(colnames(imdb[i]), resultado))
        print( "---------")
        "\n"
           }
    
# Atenção colunas completas somente: id_filme | titulo | titulo_original | nota_imdb | num_avaliacao 
#                                    data_lancamento |genero | duracao       
      
  
       #---
       # BASE DE DADOS
       View(imdb_lucrodolar)  # para trabalhar com lucros em dolar       
       View(imdb)             # base total para trabalhar com notas e outras coisas
       glimpse(imdb)       
       
       


# GERAL -------------------------------------------------------------------

# Configurando o tema dos gráficos    
  meu_tema <- theme(legend.position = "none",
                      axis.line.x = element_line(colour = "gray"),
                      axis.line.y = element_line(colour = "gray"),
                      
                      axis.title = element_text(face = "bold", size = 16),
                      axis.text.x = element_text(face = "plain", size=12),
                      axis.text.y = element_text(face = "plain", size=12),
                      
                      axis.ticks = element_line(colour = "gray", size = 0.2),
                      
                      panel.grid = element_blank(),
                      panel.background = element_blank())


# ANALISE DESCRITIVA ------------------------------------------------------

# 82.094 titulos no banco de dados todo
  n_distinct(imdb$titulo) 
       
# 7.220 títulos depois q filtrei os "NAs", dolar e os que tinham dolar na receita e orcamento para calcular o lucro com a mesma moeda.    
 n_distinct(imdb_lucrodolar$titulo) 

 # OBS: Achei q tivesse sobrado mais. Mas vi que o numero reduziu ainda mais devido a exclusão do NAs e quando eu selecionei os tinham receita e orcamento em dolar. Nao gostei. aff
 
    
#------------------------------
#1. Qual o lucro de cada filme?
#2. Quais filmes foram os mais lucrativos?
     
    # LISTAGEM DOS 20 FILMES MAIS LUCRATIVOS   
       imdb_lucrodolar %>% 
         select(titulo, lucro_mi) %>% 
         arrange(desc(lucro_mi)) %>% 
         slice_max(n=20, order_by = lucro_mi, with_ties = TRUE) %>% 
          ggplot(aes(y = fct_reorder(titulo,lucro_mi,.desc = F), x = lucro_mi)) +  # reordenar(forcats)
              geom_bar(stat = "identity", alpha = 1/2) +       #não contar os dados
              xlab("Lucro (Milhões)")+
              ylab ("Títulos")
 
 
   # CONCLUSÃO: avatar e avengers foram os filmes mais lucrativos, lucraram mais de 2 bilhoes de dolares.
   
 
 
 
  
#3. Filmes com orcamento alto foram sempre lucrativos?
#RECEITA vs ORCAMENTO: Ao fazer esse grafico, alguns filmes tiverem orcamento bem alto, mas a receita não foi tão alta, eu queria ver se esses filmes ainda assim foram lucrativos 
# Demorei mto para achar essa solução do "case_when" para diferenciar esses pontos no grafico. Ufa!
  
        imdb_lucrodolar %>% 
            mutate(avaliacao = case_when(lucro_mi<=0 ~ "nao-lucrativo",
                                         lucro_mi>0 ~"lucrativo")) %>%  
                 
            ggplot(aes(x = orcamento_mi, y = receita_mi, color = avaliacao)) +
                geom_point() +
                xlab("Orçamento (Milhões)")+
                ylab ("Receita (Milhões)")+
                scale_y_continuous(breaks=seq(0, 3000, 250))
          
         
            

        
# 4. Qual tipo de filme produz o maior retorno financeiro?       
       
# Problemas: tive q colocar espaço depois da vírgula pq senao alguns generos ficavam com espaço na frente quando separavam, meu deus!!!! sofri para descobrir isso.. haha  
       
        
#1      # Numero de filmes em cada categoria - Número de títulos por gênero
        imdb_lucrodolar %>% 
          mutate(split_generos = str_split(genero, "\\, ")) %>%  
          unnest(split_generos) %>% 
          group_by(split_generos) %>%
          summarise(n_generos = n_distinct(titulo)) %>% 
          ggplot(aes(y = fct_reorder(split_generos, n_generos, .desc= F) , x = n_generos)) +
          geom_bar(stat = "identity", alpha = 1/2) +
          scale_x_continuous(labels = scales::dollar)+
          xlab("Lucro Médio (Milhões)")+
          ylab ("Gêneros")+
          meu_tema
        
        
         
         
#2      # Generos mais lucrativos - Lucro médio por gênero
        
        #lucro medio 
        imdb_lucrodolar %>% 
            mutate(split_generos = str_split(genero, "\\, ")) %>%  
            unnest(split_generos) %>% 
            group_by(split_generos) %>% 
            summarise(media_lucro = mean(lucro_mi)) %>% 
            arrange(desc(media_lucro)) %>% 
            ggplot(aes(y = fct_reorder(split_generos, media_lucro, .desc= F) , x = media_lucro)) +
            geom_bar(stat = "identity", alpha = 1/2)+
            scale_x_continuous(labels = scales::dollar)+
            xlab("Lucro Médio (Milhões)")+
            ylab ("Gêneros")+
            meu_tema
          
          
        #lucro total
         imdb_lucrodolar %>% 
           mutate(split_generos = str_split(genero, "\\, ")) %>%  
           unnest(split_generos) %>% 
           group_by(split_generos) %>% 
           summarise(sum_lucro = sum(lucro_mi)) %>% 
           ggplot(aes(y = fct_reorder(split_generos, sum_lucro, .desc= F) , x = sum_lucro)) +
           geom_bar(stat= "Identity", alpha = 1/2)+
           scale_x_continuous(labels = scales::dollar)+
           xlab("Lucro Total (Milhões)")+
           ylab ("Gêneros")+
           meu_tema
         
         
  #    
  #       
  #       
  #          
  ### ---- ### PAUSA ### ---- ###
  # Treinando função para reduzir o codigo 

# DUVIDA: Consigo fazer a função, mas ela não está geral o suficiente para ser usada em outros casos, como faço isso?       
    
  graf_lucro <- function(tabela, operacao = mean){
                 if(operacao == "mean"){
                   tabela %>% 
                     mutate(split_generos = str_split(genero, "\\, ")) %>%  
                     unnest(split_generos) %>% 
                     group_by(split_generos) %>% 
                     summarise(media_lucro = mean(lucro_mi)) %>% 
                     arrange(desc(media_lucro)) %>% 
                     ggplot(aes(y = fct_reorder(split_generos, media_lucro, .desc= F) , x = media_lucro)) +
                     geom_bar(stat = "identity", alpha = 1/2)+
                     xlab("Lucro Médio (Milhões)")+
                     ylab ("Gêneros")
                 } else if (operacao == "sum"){
                   tabela %>% 
                     mutate(split_generos = str_split(genero, "\\, ")) %>%  
                     unnest(split_generos) %>% 
                     group_by(split_generos) %>% 
                     summarise(sum_lucro = sum(lucro_mi)) %>% 
                     ggplot(aes(y = fct_reorder(split_generos, sum_lucro, .desc= F) , x = sum_lucro)) +
                     geom_bar(stat= "Identity", alpha = 1/2)+
                     xlab("Lucro (Milhões)")+
                     ylab ("Gêneros")} else{
                       print("só aceita a operação 'sum' ou 'mean'")
                     }
               }     
  #    
  #    
  #    
  #    
  ### ---- ### FIM DA PAUSA ### ---- ###             
         
  #    
  #       
  #       
  #          
        ### ---- ### PAUSA ### ---- ###       
        # para salvar esse banco       
        #       
        # Para tentar trabalhar com o purrr sem fazer besteira na tabela q já funciona. rs
        # não consegui não me repetir no código. Voltar depois aqui!  
        imdb_lucrodolar_purr <- imdb_lucrodolar %>% 
                                  mutate(genero = str_split(genero, "\\, "),
                                         elenco = str_split(elenco, "\\, "),
                                         idioma = str_split(idioma, "\\, "),
                                         pais = str_split(pais,"\\, "),
                                         direcao = str_split(direcao,"\\, "),
                                         roteiro = str_split(roteiro,"\\, "))
        #    
        #    
        #    
        #    
        ### ---- ### FIM DA PAUSA ### ---- ###       
               
        
       
#3      # Mês mais lucrativo - lucro médio por mês                                
        imdb_lucrodolar_purr %>% 
          group_by(mes) %>% 
          drop_na(mes) %>% 
          summarise(media_lucro = mean(lucro_mi, na.rm = TRUE)) %>% 
          ggplot(aes(y = media_lucro, x= mes)) +
          geom_bar(stat = "identity", alpha = 1/2)+
          scale_y_continuous(labels = scales::dollar)+
          xlab("Mês")+
          ylab ("Lucro Médio (Milhões)")+
          meu_tema
          
        
        # Mês mais lucrativo dos gêneros mais lucrativo
        # Não achei informativo
        imdb_lucrodolar_purr %>% 
          group_by(mes, genero) %>%
          unnest(genero) %>% 
          drop_na(mes) %>% 
          summarise(media = mean(lucro_mi)) %>% 
           filter(genero == "Animation"|
                     genero == "Adventure"|
                     genero == "Sci-Fi"|
                     genero == "Fantasy") %>% 
           ggplot(aes(y = media, x= mes, fill = genero, label = round(media,digits = 1))) +
           geom_bar(stat = "identity")+
            scale_fill_manual(values=c("#E62634",
                                       "#009847",
                                       "#EF9A03",
                                       "#2A2961"))+
           geom_label(position = position_stack (vjust = 0.5),alpha = 1/2, 
                      colour = "lightgray", fontface = "bold", show_guide  = FALSE)+
           coord_flip()+
           xlab("Mês")+
           ylab ("Lucro Médio (Milhões)") +
           meu_tema+
           theme(legend.position = "bottom")+
           labs(fill = NULL)
         
        
       
          
        # Num. de filmes por mês por gênero
        # generos mais lucrativos
        imdb_lucrodolar_purr %>% 
           group_by(mes, genero) %>%
           unnest(genero) %>% 
           drop_na(mes) %>% 
           summarise(contagem = n()) %>% 
           filter(genero == "Animation"|
                  genero == "Adventure"|
                  genero == "Sci-Fi"|
                  genero == "Fantasy") %>% 
           ggplot(aes(y = contagem, x= mes, fill = genero, label = contagem)) +
           geom_bar(stat = "identity")+
           scale_fill_manual(values=c("#E62634",
                                      "#009847",
                                      "#EF9A03",
                                      "#2A2961"))+
           geom_label(position = position_stack (vjust = 0.9),alpha = 1/2, 
                      colour = "lightgray", fontface = "bold", show_guide  = FALSE)+
           #coord_flip()+
           scale_y_continuous(breaks=NULL)+
           xlab("Mês")+
           ylab ("Número de filmes") +
          
           meu_tema+
           theme(legend.position = "bottom")+
           labs(fill = NULL)
           
     
         
        
# 4     # Qual idioma mais lucrativo?
        # Idioma mais lucrativos - Lucro médio por idioma
       
        imdb_lucrodolar_purr %>%
          unnest(idioma) %>%
          group_by(idioma) %>%
          drop_na(idioma) %>% 
          summarise(sum_lucro = sum(lucro_mi, na.rm = TRUE)) %>%
          arrange(desc(sum_lucro)) %>%
          slice_max(n=5, order_by = sum_lucro, with_ties = TRUE) %>% 
          
          ggplot(aes(y = fct_reorder(idioma, sum_lucro, .desc= F) , x = sum_lucro)) +
          geom_bar(stat = "identity", alpha = 1/2)+
          xlab("Lucro (Milhões)")+
          ylab ("Idioma")+
          scale_x_continuous(labels = scales::dollar)+
          meu_tema
        

          
# 5     # Qual é a média de duração dos filmes que foram mais lucrativos?
        # Selecionar os filmes com mais de 10% de lucro
          
        
        imdb_lucrodolar_purr %>%
          select(titulo, duracao, lucro_mi, orcamento_mi) %>%   
          filter(lucro_mi >= orcamento_mi * 0.1) %>%
          arrange(desc(lucro_mi)) %>% 
          slice_max( n= 20, lucro_mi) %>% 
          summarise(media_duracao= mean(duracao)) 
          

        # duracao media dos 20 filmes mais lucrativos: 137min
        # duracao media dos filmes que lucraram mais de 10%: 109min
        
        
          
        # CONCLUSÃO: 1. Os 5 gêneros mais lucrativos: Animacao | Aventura | Ficcao | Acao | Fantasia 
        #            2. Os 5 gêneros mais produzidos: Drama | Comédia | Acao | Crime | Aventura
        #            3. Dezembro e Janeiro foram os meses mais lucrativos
        #            4. Os maiores lucros foram dos filmes em lingua Inglesa, Espanhola e Francesa
        #            5. Em média, os filmes mais lucrativos duram aproximadamente 2h e 20 min (137min)
  

        
#5.     #Existe relação entre o retorno financeiro e a nota imdb?             
     
        # DUVIDA: Nao consegui. Nem todas funções conseguem receber os dados do pipe?
        #         Entao para análises, uso o pipe para a manipulação, salvo a tabela para rodar análise?
        # imdb_lucrodolar %>%
        #     summarise(cor.test(nota_imdb, lucro_mi, method="spearman"))    
        # 
        # Tentativa 2: 
        # DUVIDA: Funciona, mas não consigo ver o p-valor
        # imdb_lucrodolar %>%
        #        summarise(r = cor (nota_imdb, lucro_mi, method="spearman"))
          
        
        cor.test(imdb_lucrodolar$lucro_mi, imdb_lucrodolar$nota_imdb, method="spearman") #sem pressuposto
        imdb_lucrodolar %>% 
          ggplot(aes(x = nota_imdb, y = lucro_mi))+
          geom_point(size=2, shape=16, stroke=2, alpha=1)+
          geom_smooth(method="lm")
          
        
# CONCLUSÃO: Não existe relação entre o lucro e a nota do imdb, como eu supunha inicial. Então, para o retorno financeiro nao interessa a nota recebida pelo público. Pois um filme bem avaliado pelo público não necessariamente teve otimo retorno financeiro.
        

#6        # Qual cineastra teve o maior retorno financeiro?
          
          imdb_lucrodolar_purr %>%
            select(titulo, lucro_mi, direcao, genero) %>% 
            unnest(direcao) %>% 
            group_by(direcao) %>% 
            summarise(med_lucro = mean(lucro_mi, na.rm = TRUE)) %>% 
            arrange(desc(med_lucro)) %>% 
            slice_max(n=10, med_lucro)
            
        
          # Funcao para olhar os generos produzidos pelos 10 diretores q mais lucraram  
            listar_gen <-  function (tabela, nome){
              tabela %>% 
                select(titulo, direcao, genero) %>% 
                unnest(direcao) %>%
                filter(direcao == "nome") %>% 
                unnest(genero) 
              }
      
            lista_direcao <- listar_gen(imdb_lucrodolar_purr,c("Jennifer Lee", "Anthony Russo","Joe Russo", "Josh Cooley","Pierre Coffin")) 
            
#Função nao funcionou, quando coloco o nome de todo mundo ele não retorna todos os filmes de cada diretor, some filme. não sei o q estou errando           
 
      
      
      # Então vamos sem função
      imdb_lucrodolar_purr %>% 
        select(titulo, direcao, genero) %>%
        unnest(direcao) %>%
        filter(direcao == as.character("Jennifer Lee")|
               direcao == as.character("Anthony Russo")|
               direcao == as.character("Joe Russo")|
               direcao == as.character("Josh Cooley")|
               direcao == as.character("Pierre Coffin")) %>% 
        unnest(genero) %>% 
        group_by(direcao, genero) %>% 
        summarise(contagem = length(genero)) %>%
        
        ggplot(aes(y= fct_reorder(direcao, genero),
                   x= contagem, fill= genero, label= contagem))+
          geom_bar(stat = "identity")+
          scale_fill_brewer(palette="Spectral")+
          xlab("Número de filmes por categoria")+
          ylab ("Direção") + 
          scale_x_continuous(breaks=NULL)+
          geom_label(position = position_stack (vjust = 0.2),alpha = 0.65, 
                 colour = "white", fontface = "bold", show_guide  = FALSE)+
          meu_tema +
          theme(legend.position = "bottom")+
          labs(fill = NULL)
      

## Conclusão: Se quiser contratar alguem para fazer um filme dos 5 gêneros mais lucrativos (Animacao | Aventura | Ficcao | Acao | Fantasia). Poderia contratar um diretor que tenha tido uma boa média de lucro nos seus filmes:

# Jennifer Lee - Frozen I e II
# Anthony Russo - Avengers: Infinity War | Avengers: Endgame | Captain America: Civil War
# Joe Russo -  Avengers: Infinity War | Avengers: Endgame | Captain America: Civil War
# Josh Cooley - Toy Story 4
# Pierre Coffin - Minions | Cattivissimo me (meu malvado favorito)
# Jing Wu: Wolf Warrior 2  
# Angus MacLane - Alla ricerca di Dory (procurando dori)
# Lee Unkrich - Toy Story 3 | Alla ricerca di Nemo (procurando nemo)|Coco
# James Cameron - Avatar | Titanic | True Lies| Terminator 2|The Abyss|Aliens|Terminator
            
 # Joe Russo e Anthony Russo são mais ecléticos, seus filmes se encaixam em diferentes categorias de filmes. Já Jennifer Lee, Josh Cooley e Pierre Coffin os filmes se encaiam em animação, comédia e aventura.          
          
     

   
 # ---------------------------------
      
      # 1. Existe melhor data para lancamento? 
      #    Qual o mês do ano com o maior númedo de filmes? E o dia do ano?

      imdb %>% 
        select(titulo, ano, data_lancamento) %>% 
        separate(col = data_lancamento,
                 into = c("ano","mes","dia"),
                 sep = "-") %>%
        group_by(mes) %>% 
        filter(!is.na(mes)) %>% 
        summarise(n_titulo = n_distinct(titulo, na.rm = TRUE)) %>% 
        
        ggplot() +
        geom_col(aes( y = n_titulo, x = mes),
                 colour = "orange", fill = "lightblue") +
        scale_x_discrete (name="Mês") +
        scale_y_continuous(name="Número de títulos") +
        
        labs(title = "Lançamento de Títulos")+
        meu_tema
      
      
      
      imdb %>% 
        select(titulo, ano, data_lancamento) %>% 
        separate(col = data_lancamento,
                 into = c("ano","mes","dia"),
                 sep = "-") %>%
        group_by(dia) %>% 
        filter(!is.na(dia)) %>% 
        summarise(n_titulo = n_distinct(titulo, na.rm = TRUE)) %>% 
        
        ggplot() +
        geom_col(aes( y = n_titulo, x = dia),
                 colour = "orange", fill = "lightblue") +
        scale_x_discrete (name="Dias") +
        scale_y_continuous(name="Número de títulos") +
        meu_tema
     
  # CONCLUSÃO: 
  # 1. No meio do ano (de maio a agosto) temos uma baixa na quantidade de filmes lançados, sendo esse número maior no final do ano e começo. 
  # 2. Os filmes são lancados, em geral, no primeiro dia do mês.
      
# LOGO, Deveriamos seguir a tendencia de lancar o filme no primeiro dia do mês, aproveitar o pagamento da galera, e evitar lançar nos meses de maio a agosto.
      
      
      
      
      # 2. Como o cinestra pode ficar famoso?
      #         - qual tipo de filme com maior numero de avaliações?
      #         - qual tipo de filme com maior nota?
      #         - qual o filme foi melhor aceito pela critica e pelo publico? 
  
  
  
  
  
  
  
  
  
  
  
  
  
    
       
    
       
       
# --- NOTA IMDB

    # Os 10 Filmes com MAIORES notas - sem considerar a diferenca no num_avalicoes
    imdb %>% 
        select(titulo, nota_imdb) %>% 
        arrange(desc(nota_imdb)) %>% 
        slice_max(n=10, order_by = nota_imdb, with_ties = TRUE)
    
    
    # Os 10 Filmes com MENORES notas - sem considerar a diferenca no num_avalicoes
    imdb %>% 
        select(titulo, nota_imdb) %>% 
        arrange(desc(nota_imdb)) %>% 
        slice_min(n=10, order_by = nota_imdb, with_ties = TRUE)
    

 








    # 1. Selecionar algumas colunas para simplificar a visualização
    # 2. Extrair os dias e meses para coluna individual. 
    # 3. Agrupar por mês
    # 4. Filtrar somente os filmes que posseum informação sobre o mese|dia de lancamento  
    # 5. Contar o número de filmes em cada mês|dia







