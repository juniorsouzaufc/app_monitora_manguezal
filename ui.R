#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Protocolo caranguejo"),
    
    sidebarLayout(
        sidebarPanel(
            # Primeiro Input: UC
            selectInput("uc", "Selecione a Unidade de Conservação:",
                        choices = unique(lc_df_monitora$uc),
                        selected = unique(lc_df_monitora$uc)[1]),
            
            # Input de Ano (será atualizado dinamicamente)
            selectInput("ano", "Selecione o Ano:",
                        choices = NULL),  # começa vazio
            
            # Input de Zona
            selectInput("zona", "Selecione a Zona:",
                        choices = unique(lc_df_monitora$zona),
                        selected = unique(lc_df_monitora$zona)[1]),
            
            # Input para nº de bins
            sliderInput("bins", "Número de bins:",
                        min = 5, max = 50, value = 10, step = 1)
        ),
        
        mainPanel(
            plotOutput("histograma")
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
        
        # Ordena os anos de acordo com os níveis do fator
        anos_disponiveis <- intersect(levels(lc_df_monitora$ano), anos_disponiveis)
        
        
        updateSelectInput(session, "ano",
                          choices = anos_disponiveis,
                          selected = anos_disponiveis[1])
    })
    
    output$histograma <- renderPlot({
        dados_filtrados <- lc_df_monitora %>%
            filter(uc == input$uc, ano == input$ano, zona == input$zona)
        
        # calcular média e desvio padrão
        media <- mean(dados_filtrados$dg_mm, na.rm = TRUE)
        desvio <- sd(dados_filtrados$dg_mm, na.rm = TRUE)
        
        # calcular posição dinâmica para o texto
        # pega limites do eixo x e y com base nos dados
        x_max <- max(dados_filtrados$dg_mm, na.rm = TRUE)
        y_max <- max(hist(dados_filtrados$dg_mm, breaks = input$bins, plot = FALSE)$counts)
        
        # define posição relativa (ex: 80% do eixo x e 90% do eixo y)
        x_pos <- x_max * 0.8
        y_pos <- y_max * 0.9
        
        #GGplot
        ggplot(dados_filtrados, aes(x = dg_mm)) +
            geom_histogram(bins = input$bins, fill = "cadetblue1", color = "black") + 
            
            # vetor que destaca a linha da média
            geom_vline(xintercept = media,color = "red", linetype = "solid", size = 1.5) + 
            
            # linhas do desvio padrão
            geom_vline(xintercept = media - desvio, color = "darkred", linetype = "dashed", size = 0.8) +
            geom_vline(xintercept = media + desvio, color = "darkred", linetype = "dashed", size = 0.8) +
            
            # texto com valores
            annotate("text", x = x_pos, y = y_pos,
                     label = paste0("Média = ", round(media,1),
                                    "\nDesvio padrão = ", round(desvio,1)),
                     hjust = 0, vjust = 1, color = "black", size = 5, fontface = "bold") +
            
            scale_y_continuous(expand = expansion(mult = c(0,0.05))) + 
            scale_x_continuous(breaks = seq(0, 180, 20)) + 
            labs(title = "Histograma de Diâmetro da galeria (mm)",
                 x = "Diâmetro de Galeria (mm)",
                 y = "Frequência") +
            theme_bw() + 
            theme(axis.title = element_text(face = "bold", colour = "black", size = 15),
                  axis.text = element_text(colour = "black", size = 14),
                  plot.title = element_text(face = "bold", colour = "black", size = 16))
    })
}

shinyApp(ui = ui, server = server)
