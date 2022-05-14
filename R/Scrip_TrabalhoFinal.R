

# Script - Trabalho Final
# Natália - Maio 2022

# Carregar Pacotes
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(purrr)


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
       # Não são a mesma coisa. 
       table(imdb_lucrodolar$receita == imdb_lucrodolar$receita_eua) 
       
       
       # Parece a "receita" é sempre maior do que a "receita_eua"
       # Vou usar a "receita" que parece ser a receita total do filme
       
       imdb_lucrodolar %>% 
                       mutate(diferenca_receita = receita - receita_eua) %>% 
                       filter(diferenca_receita < 0) %>% 
                       view()
       
       
       # Criando coluna de lucro na base
       # Colocando lucro para a escala de milhões
       # Apagando a coluna das moedas, pois já filtramos para dolar ao criar imdb_lucrodolar
       
       imdb_lucrodolar <- imdb_lucrodolar %>% 
                                           mutate(lucro_dolar = receita - orcamento) %>% 
                                           mutate(lucro_mi = lucro_dolar/1000000) %>%
                                           select(- c(moeda_orcamento,moeda_receita,
                                                      moeda_receita_eua, receita_eua)) 
       
#---
# BASE DE DADOS
  View(imdb_lucrodolar)  # para trabalhar com lucros em dolar       
  View(imdb)             # base total
  glimpse(imdb)       
       
       
       
       
       
       
       
       
       

# Identificar valores faltantes    

    for (i in 1:length(imdb)){
        resultado <- table(is.na(imdb[i]))
        print(c(colnames(imdb[i]), resultado))
        print( "---------")
        "\n"
           }
    
# Atenção colunas completas: id_filme | titulo | titulo_original | nota_imdb | num_avaliacao 
#                            data_lancamento |genero | duracao


# GERAL -------------------------------------------------------------------

# Configurando o tema dos gráficos    
meu_tema <- theme(legend.position = "none",
                  axis.line.x = element_line(colour = "black"),
                  axis.line.y = element_line(colour = "black"),
                  
                  axis.title = element_text(face = "bold", size = 16),
                  axis.text.x = element_text(face = "plain", size=16),
                  axis.text.y = element_text(face = "plain", size=16),
                  
                  axis.ticks = element_line(colour = "black", size = 0.2),
                  
                  panel.grid = element_blank(),
                  panel.background = element_blank())


# ANALISE DESCRITIVA ------------------------------------------------------

    colnames(imdb)
    View(imdb_avaliacoes)

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
    
# --- RECEITA

imdb_dinheiro <-   imdb %>% 
                        select(titulo, receita_eua, receita, orcamento) %>%
                        #drop_na(c(receita_eua, receita, orcamento)) %>% 
                        separate(col = receita_eua,
                                 into = c("moeda_rec_eua","receita_eua"),
                                 sep = " ") %>%
                        separate(col = receita,
                                 into = c("moeda_rec","receita"),
                                 sep = " ") %>%
                        separate(col = orcamento,
                                 into = c("moeda_orc","orcamento"),
                                 sep = " ")


 
   
# Acertando a tabela

# Transformar a coluna data_lancamento em formato data 
    # Problema: Alguns filmes (4563) não tem a data completa, possuem somente o ano. 
    # Estes filmes ficam como NA na data_lancamento quando transformamos para data

    #imdb <- imdb %>% 
     #   mutate(data_lancamento = ymd(data_lancamento))
    





# Descrição dados

# Quantidades
imdb %>% 
    summarise(
        across(.cols = c (id_filme, idioma),
               .fns = n_distinct)
    )






# 1. Qual o mês do ano com o maior númedo de filmes? E o dia do ano?

    # 1. Selecionar algumas colunas para simplificar a visualização
    # 2. Extrair os dias e meses para coluna individual. 
    # 3. Agrupar por mês
    # 4. Filtrar somente os filmes que posseum informação sobre o mese|dia de lancamento  
    # 5. Contar o número de filmes em cada mês|dia



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



