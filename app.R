#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
