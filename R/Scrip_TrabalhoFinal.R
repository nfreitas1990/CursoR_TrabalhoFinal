

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



# DÚVIDA: Não consegui salvar e versionar o arquivo quando em .rds dizia que o arquivo estava muito grande. Então tive que salvar em .csv. Qual seria a melhor estratégia nesse caso? usar qual extensão para salvar o arquivo? 



# VISUALIZAR DADOS --------------------------------------------------------
# Objetivo: Identificar Problemas na base de dados 

   
# Checar o tipo das variáveis
    
    glimpse(imdb)   # data_lancamento: está como character
                    # data_lancamento: incompleta - faltando dia e mes em aguns filmes
                    # ano: como double
                    # duracao: como double
                    # orcamento| receita | receit_eua: como character


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

# QUESTÃO 3: Tipos de Moedas - ---------------------------------------------------------
# Liste todas as moedas que aparecem nas colunas `orcamento` e `receita`

# Moeda - Receita EUA
imdb_dinheiro %>% 
    group_by(moeda_rec_eua) %>% 
    drop_na(moeda_rec_eua) %>% 
    summarise(moeda = table(moeda_rec_eua)) %>% 
    arrange(desc(moeda))
  
 # Moeda - Receita
 imdb_dinheiro %>% 
     group_by(moeda_rec) %>% 
     drop_na(moeda_rec) %>% 
     summarise(moeda = table(moeda_rec)) %>% 
     arrange(desc(moeda)) 
 
 
 # Moeda - Orcamento 
 imdb_dinheiro %>% 
     group_by(moeda_orc) %>% 
     drop_na(moeda_orc) %>% 
     summarise(moeda = table(moeda_orc)) %>% 
     arrange(desc(moeda))
 

 
 
   
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



