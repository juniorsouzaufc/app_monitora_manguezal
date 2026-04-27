#Bibliotecas Utilizadas

library(readxl)
library(tidyverse)
library(writexl)
library(tibble)
library(forcats)
library(viridis)
library(openxlsx)

# Data set

#Definindo o diretório

setwd("D:/rstudio/comob/app-1")

# dataset csv
dados_csv <- read.csv("caranguejo_manguezal_30_nov_2026.csv",
                      header = TRUE, 
                      sep = ";", 
                      fileEncoding = "latin1")

glimpse(dados_csv)
summary(dados_csv)


#Excluindo algumas colunas usando o dplyr

library(dplyr)

dados <- dados_csv %>% select(-data_do_registro , -data_do_recebimento, -ultima_edicao,
                              -protocolo, -usuario, -validador, -data_validacao, 
                              -obs_validacao, -planilha_upload, -coleta_uuid, -controle_versao,
                              -uuid, -hora,-n_form_ref, -coletor.cpf, -coletor.nome, -data_coleta,
                              -cabecalho.hora_inicial, -cabecalho.obs_localizacao, -cabecalho.obs_localizacao,
                              -cabecalho.avaliadores, -cabecalho.obs_equipe, -tocas_registro.obs_toca,
                              -tocas_registro.foto_toca_1, -tocas_registro.foto_toca_2, -tocas_registro.foto_toca_3,
                              -tocas_registro.tocas_uuid, -nivel_inundacao.obs_inundacao, -arvores.obs_tot_arvores,
                              -arvores.arvores_registro.obs_arvore, -observacoes.hora_final, -observacoes.observacao,
                              -observacoes.foto_ficha_1, -observacoes.foto_ficha_2, -observacoes.foto_ficha_3,
                              -observacoes.foto_ficha_4, -observacoes.foto_ficha_5, -X_msk_name_, -X_rmc_name_, -X_rmd_name_)

#Filtrando um novo dataframe apenas com os dados VALIDADOS
glimpse(dados)

df_monitora <- dados %>% dplyr::filter(validado == "sim")

#Pronto o df_monitora é o nosso dataframe apenas com dados validados
glimpse(df_monitora)

#Extraindo a data para tres colunas no formato ano, mes e dia.

library(tidyr)

head(df_monitora$cabecalho.data)

df_monitora <- df_monitora %>%
  separate(cabecalho.data, into = c("ano", "mes", "dia"), sep = "-")

glimpse(df_monitora)


#Separando os data frame por Unidade de Conservação

# Divide em uma lista de data frames por UC
lista_df_uc <- split(df_monitora, df_monitora$uc)

names(lista_df_uc)   # mostra os grupos criados para cada UC

# Cria data frames com nomes diferentes
for(nome in names(lista_df_uc)) {
  assign(paste0("df_", nome), lista_df_uc[[nome]])
}

#Renomear cada nome do df criados para cada UC

#Criando o df_resex_carijos
df_esec_carijos <- `df_Estação Ecológica de Carijós`
rm(`df_Estação Ecológica de Carijós`)

#Criando o df_esec_jipioca
df_esec_jipioca <- `df_Estação Ecológica de Maracá Jipioca`
rm(`df_Estação Ecológica de Maracá Jipioca`)

#df_resex_choco
df_resex_choco <- `df_Reserva Extrativista Chocoaré-Mato Grosso`
rm(`df_Reserva Extrativista Chocoaré-Mato Grosso`)

#df_resex_cassuruba
df_resex_cassuruba <- `df_Reserva Extrativista de Cassurubá`
rm(`df_Reserva Extrativista de Cassurubá`)

#df_resex_cururupu
df_resex_cururupu <- `df_Reserva Extrativista de Cururupu`
rm(`df_Reserva Extrativista de Cururupu`)

#df_resex_grande_mae
df_resex_grande_mae <- `df_Reserva Extrativista Mãe Grande de Curuçá`
rm(`df_Reserva Extrativista Mãe Grande de Curuçá`)

#df_resex_arai_peroba
df_resex_arai_peroba <- `df_Reserva Extrativista Marinha Araí-Peroba`
rm(`df_Reserva Extrativista Marinha Araí-Peroba`)

#df_resex_caete_taperacu
df_resex_caete_taperacu <- `df_Reserva Extrativista Marinha Caeté-Taperaçu`
rm(`df_Reserva Extrativista Marinha Caeté-Taperaçu`)

#df_resex_gurupi
df_resex_gurupi <- `df_Reserva Extrativista Marinha de Gurupi-Piriá`
rm(`df_Reserva Extrativista Marinha de Gurupi-Piriá`)

#df_resex_soure
df_resex_soure <- `df_Reserva Extrativista Marinha de Soure`
rm(`df_Reserva Extrativista Marinha de Soure`)

#df_resex_prajubae
df_resex_pirajubae <- `df_Reserva Extrativista Marinha Pirajubaé`
rm(`df_Reserva Extrativista Marinha Pirajubaé`)

#df_resex_mestre_lucindo
df_resex_mestre_lucindo <- `df_Reserva Extrativista Marinha Mestre Lucindo`
rm(`df_Reserva Extrativista Marinha Mestre Lucindo`)

#Etapa concluída de elaboração de dataframes das UCs


#Criando um dataframe com a variável DG (mm) para todas as UCs

#novo df
lc_df_monitora <- df_monitora %>% select(coleta, uc, ciclo, ea, ua, ano, mes, dia, 
                                         cabecalho.zona, tocas_registro.diametro_galeria_mm)

glimpse(lc_df_monitora)

#Renomenado algumas variáveis
lc_df_monitora <- lc_df_monitora %>% rename(zona = cabecalho.zona,
                                            dg_mm = tocas_registro.diametro_galeria_mm)

glimpse(lc_df_monitora)

#alterando a virgula para ponto nos dados de DG (mm)
lc_df_monitora$dg_mm <- gsub(",", ".", lc_df_monitora$dg_mm)
class(lc_df_monitora$dg_mm)
lc_df_monitora$dg_mm <- as.numeric(lc_df_monitora$dg_mm)
class(lc_df_monitora$dg_mm)
unique(lc_df_monitora$dg_mm)

#Removendo os NAs do lc_df_monitora
summary(lc_df_monitora$dg_mm)
sum(is.na(lc_df_monitora$dg_mm))
which(is.na(lc_df_monitora$dg_mm))

#Espaço em brancos + Nas
sum(is.na(lc_df_monitora$dg_mm) | trimws(lc_df_monitora$dg_mm) == "")

#Criando dentro do mesmo df sem os valores Nas na coluna dg_mm
lc_df_monitora <- lc_df_monitora[!is.na(lc_df_monitora$dg_mm), ]

#Calculando o número de linhas de diferenças sem as Nas
nrow(df_monitora) - nrow(lc_df_monitora) #= 2976

glimpse(lc_df_monitora)

#Alterando a classe da variável ano e mes

#Ano
unique(lc_df_monitora$ano)
class(lc_df_monitora$ano)

lc_df_monitora$ano <- as.numeric(lc_df_monitora$ano)

unique(lc_df_monitora$ano)

lc_df_monitora$ano <- as.factor(lc_df_monitora$ano)

levels(lc_df_monitora$ano)

anos_ordem <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")

lc_df_monitora$ano <- factor(lc_df_monitora$ano,
                             levels = anos_ordem,
                             ordered = TRUE)

#Mes

unique(lc_df_monitora$mes)

meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", 
              "Jul", "Ago", "Set", "Out", "Nov", "Dez")


lc_df_monitora$mes_num <- as.numeric(lc_df_monitora$mes)
lc_df_monitora$mes_nome <- month.abb[as.numeric(lc_df_monitora$mes)]
lc_df_monitora$mes_nome <- meses_pt[as.numeric(lc_df_monitora$mes)]

head(lc_df_monitora[, c("mes", "mes_num", "mes_nome")])

lc_df_monitora <- lc_df_monitora %>%
  relocate(mes_nome, .before = 9)

lc_df_monitora$mes_num <- NULL

glimpse(lc_df_monitora)

lc_df_monitora$mes_nome <- factor(lc_df_monitora$mes_nome,
                                  levels = meses_pt,
                                  ordered = TRUE)

levels(lc_df_monitora$mes_nome)

class(lc_df_monitora$mes_nome)

str(lc_df_monitora$mes_nome)

unique(lc_df_monitora$mes_nome)

glimpse(lc_df_monitora)

#Zona

lc_df_monitora <- lc_df_monitora %>%
  mutate(zona = recode(zona,
                       "bacia" = "Bacia",
                       "franja" = "Franja"))


#criando alguns histogramas:

#Criando o histograma
summary(lc_df_monitora$dg_mm)

ggplot(lc_df_monitora, aes(x = dg_mm)) +
  geom_histogram(binwidth = 4, fill = "cadetblue1", color = "black") + 
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) + 
  facet_grid(ano*zona ~ uc, scales = "free") +
  labs(title = "Unidade de Conservação",
       x = "Diâmetro da Galeria (mm)",
       y = "Frequência") +
  theme_bw() + 
  theme(axis.title = element_text(face = "bold", colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 11),
        plot.title = element_text(face = "bold", colour = "black", size = 12),
        strip.text.x = element_text(face = "bold", colour = "black", size = 10), 
        strip.text.y = element_text(face = "bold", colour = "black", size = 10))

class(lc_df_monitora$ano)
levels(lc_df_monitora$ano)
#Shiny
library(shiny)
library(bslib)
library(ggplot2)
library(shinydashboard)
library(rsconnect)

ui <- dashboardPage(
  dashboardHeader(title = "Protocolo caranguejo"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filtros", tabName = "filtros", icon = icon("sliders-h")),
      selectInput("uc", "Selecione a Unidade de Conservação:",
                  choices = unique(lc_df_monitora$uc),
                  selected = unique(lc_df_monitora$uc)[1]),
      selectInput("ano", "Selecione o Ano:",
                  choices = NULL),
      selectInput("zona", "Selecione a Zona:",
                  choices = unique(lc_df_monitora$zona),
                  selected = unique(lc_df_monitora$zona)[1]),
      sliderInput("bins", "Número de bins:",
                  min = 5, max = 50, value = 10, step = 1)
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Histograma", plotOutput("histograma")),
      tabPanel("Linhas", plotOutput("linha_zonas")),
      tabPanel("Boxplot", plotOutput("boxplot_zonas"))
    )
  )
)

server <- function(input, output, session) {
  
  # Atualiza os anos disponíveis conforme a UC escolhida
  observeEvent(input$uc, {
    anos_disponiveis <- lc_df_monitora %>%
      filter(uc == input$uc) %>%
      pull(ano) %>%
      unique()
    anos_disponiveis <- intersect(levels(lc_df_monitora$ano), anos_disponiveis)
    updateSelectInput(session, "ano",
                      choices = anos_disponiveis,
                      selected = anos_disponiveis[1])
  })
  
  # Histograma com média e desvio padrão
  output$histograma <- renderPlot({
    dados_filtrados <- lc_df_monitora %>%
      filter(uc == input$uc, ano == input$ano, zona == input$zona)
    
    media <- mean(dados_filtrados$dg_mm, na.rm = TRUE)
    desvio <- sd(dados_filtrados$dg_mm, na.rm = TRUE)
    
    x_max <- max(dados_filtrados$dg_mm, na.rm = TRUE)
    y_max <- max(hist(dados_filtrados$dg_mm, breaks = input$bins, plot = FALSE)$counts)
    x_pos <- x_max * 0.8
    y_pos <- y_max * 0.9
    
    #GGplot do histograma
    ggplot(dados_filtrados, aes(x = dg_mm)) +
      geom_histogram(bins = input$bins, fill = "cadetblue1", color = "black") + 
      
      # vetor que destaca a linha da média
      geom_vline(aes(xintercept = media, color = "Média"), linetype = "solid", size = 1.5) + 
      
      # linhas do desvio padrão
      geom_vline(aes(xintercept = media - desvio, color = "Desvio Padrão"), linetype = "dashed", size = 0.8) +
      geom_vline(aes(xintercept = media + desvio, color = "Desvio Padrão"), linetype = "dashed", size = 0.8) +
      
      # texto com valores
      annotate("label", x = x_pos, y = y_pos,
               label = paste0("Média = ", round(media,1),
                              "\nDesvio padrão = ", round(desvio,1)),
               hjust = 0, vjust = 1, color = "black", size = 5,
               fontface = "bold", fill = "white") + 
      
      #Legenda para as linhas
      scale_color_manual(name = "Legenda:",
                         values = c("Média" = "red", "Desvio Padrão" = "darkred")) + 
      #Escalas do eixo x e y:
      scale_y_continuous(expand = expansion(mult = c(0,0.05))) + 
      scale_x_continuous(breaks = seq(0, 180, 20)) + 
      labs(title = "Histograma de Diâmetro da galeria (mm)",
           x = "Diâmetro de Galeria (mm)",
           y = "Frequência") +
      theme_bw() + 
      theme(axis.title = element_text(face = "bold", colour = "black", size = 15),
            axis.text = element_text(colour = "black", size = 14, face = "bold"),
            plot.title = element_text(face = "bold", colour = "black", size = 16),
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 12, colour = "black", face = "bold"),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", colour = "black", size = 0.8),
            legend.box.background = element_rect(colour = "black", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  # novo gráfico de linhas comparando zonas ao longo dos anos
  output$linha_zonas <- renderPlot({
    dados_linha <- lc_df_monitora %>%
      filter(uc == input$uc) %>%
      group_by(ano, zona) %>%
      summarise(
        media_dg = mean(dg_mm, na.rm = TRUE),
        sd_dg = sd(dg_mm, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(dados_linha, aes(x = ano, y = media_dg, group = zona, color = zona)) +
      geom_line(size = 1, position = position_dodge(width = 0.3)) +
      geom_point(size = 3, position = position_dodge(width = 0.3)) +
      geom_errorbar(aes(ymin = media_dg - sd_dg, ymax = media_dg + sd_dg),
                    width = 0.2, size = 0.8, position = position_dodge(width = 0.3)) +
      labs(title = paste("Média de Diâmetro de Galeria (mm) ao longo do anos -", input$uc),
           x = "Ano",
           y = "Média de Diâmetro de Galeria (mm)",
           color = "Zona:") +
      theme_bw() +
      theme(axis.title = element_text(colour = "black", size = 15, face = "bold"),
            axis.text = element_text(colour = "black", size = 14, face = "bold"),
            plot.title = element_text(face = "bold", size = 16, colour = "black"),
            legend.text = element_text(size = 13, colour = "black", face = "bold"),
            legend.title = element_text(size = 13, colour = "black", face = "bold"),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", colour = "black", size = 0.8),
            legend.box.background = element_rect(colour = "black", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  # novo gráfico de boxplot com barras de erro
  output$boxplot_zonas <- renderPlot({
    ggplot(lc_df_monitora %>% filter(uc == input$uc),
           aes(x = ano, y = dg_mm, fill = zona)) +
      geom_errorbar(stat = "boxplot", width = 0.2, position = position_dodge(width = 0.8)) + 
      geom_boxplot(position = position_dodge(width = 0.8)) + 
      labs(title = paste("Distribuição de Diâmetro de Galeria (mm) por Ano e Zona -", input$uc),
           x = "Ano",
           y = "Diâmetro da Galeria (mm)",
           fill = "Zona:") +
      theme_bw() +
      theme(axis.title = element_text(colour = "black", size = 15, face = "bold"),
            axis.text = element_text(colour = "black", size = 14, face = "bold"),
            plot.title = element_text(face = "bold", size = 16, colour = "black"),
            legend.text = element_text(size = 13, colour = "black", face = "bold"),
            legend.title = element_text(size = 13, colour = "black", face = "bold"),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", colour = "black", size = 0.8),
            legend.box.background = element_rect(colour = "black", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
}

shinyApp(ui = ui, server = server)

rsconnect::deployApp()
rsconnect::showLogs(appName = "app_monitora_manguezal",
                    account = "qaskyr-juniorferreira")


#D:/rstudio/comob/app-1
