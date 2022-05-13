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

imdb <- imdb_completa
# DÚVIDA: Não consegui salvar localmente e versionar o arquivo quando em .rds dizia que o arquivo estava muito grande para o versionamento. Então tive que salvar em .csv. Qual seria a melhor estratégia nesse caso? usar qual extensão para salvar o arquivo? 

#--------------
# Problemática: 
#--------------
# Um investidor contratou um cinestra para produzir um filme e deseja ter o maior retorno possível do seu investimento.Enquanto o cineastra deseja que o seu filme o coloque no rool da fama. 
# Identificar a melhor estratégia de investimento para que ambos tenham o seu desejo realizado   


# INVESTIGAR:
# 1. Retorno Financeiro:
#         - Qual o lucro de cada filme?
#         - Qual tipo de filme produz o maior retorno financeiro?
#         - O retorno financeiro é proprocional ao investimento?



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
            imdb$ano <- as_factor(imdb$ano)
            class(imdb$ano)
            
            imdb$dia <- as_factor(imdb$dia)
            class(imdb$dia)
            
            imdb$mes <- as_factor(imdb$mes)
            class(imdb$mes)
         
        imdb %>% 
          mutate(ano = as.factor(ano), mes = as.factor(mes), dia = as.factor(dia))
          
  #---        
  ## Dinheiro       
     
     # orcamento
     # Tirando o Caractere da moeda para transformar para numero
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
    
    # lucro
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
    
    # Parece a receita é sempre maior do que a receita_eua
    # Vou usar a receita que parece ser a total
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

# Atenção colunas completas: 
#   id_filme | titulo | titulo_original | nota_imdb | num_avaliacao 
#   data_lancamento |genero | duracao


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