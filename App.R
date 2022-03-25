

# Leitura dos Pacotes ----------------------------------------------------------

rm(list=ls())
options(OutDec = ',')

library(tidyverse)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(gt)
library(gtsummary) # as_gt
library(highcharter)
library(DT)
library(plotly)



# Carregando Modulos -----------------------------------------------------------

source('modulos/Analises.R', encoding = 'UTF-8')



# Carregando os dados ----------------------------------------------------------


header <- uiOutput('')

sidebar <- uiOutput('main_sidebar')

body <- dashboardBody(
  uiOutput('main_body')
)


ui <- dashboardPage(
  dashboardHeader(title = 'Dashboard Analytics'),
  sidebar,
  body
)


# Servidor ####################################################################

server <- function(input, output, session) {
  
  output$main_sidebar <- renderUI({
    
    dashboardSidebar(
      
      sidebarUserPanel(name = a(href = 'https://a3data.com.br/', 'A3Data'),
                       # O arquivo de imagem deve estar em www/
                       image = 'a3.jpg'
      ),
      
      sidebarMenu(
        id = 'tabs',
        menuItem("Sobre os Dados", tabName = "sobre_dados", icon = icon("align-justify")),
        menuItem('Análises', tabName = 'analise', icon = icon('user-friends'))
      )
    )
  })
    
  
  
  
# Body -------------------------------------------------------------------------
  
  output$main_body <- renderUI({
    tabItems(
      tabItem(
        tabName = 'sobre_dados',  
        class = 'active',
        h2(class = 'title-header', 'Sobre o banco de dados'),
        h5('Os dados foram extraídos da revista Motor Trend US de 1974 e abrangem 
           o consumo de combustível e 10 aspectos do design e 
           desempenho do automóvel para 32 automóveis (modelos 1973-74)'),
        br(),
        h4('O banco de dados está composto pelas variáveis:'),
        p(strong('mpg'), ' = Milhas/(EUA) galão', 
          br(), strong('cyl'), ' = Número de cilindros', 
          br(), strong('disp'), ' = Deslocamento (cu.in.)', 
          br(), strong('hp'), ' = Potência bruta', 
          br(), strong('drat'), ' = Relação do eixo traseiro', 
          br(), strong('wt'), ' = Peso (1000 libras)', 
          br(), strong('qsec'), ' = Tempo de 1/4 de milha', 
          br(), strong('vs'), ' = Motor (0 = V-shaped, 1 = straight)', 
          br(), strong('am'), ' = Tipo de marcha (0 = automática, 1 = manual)', 
          br(), strong('gear'), ' = Número de marchas para frente')
      ),
      analise_ui(id = 'analise')
    )
  })
  

  
  
  ## Modulos -------------------------------------------------------------------
  
  callModule(
    module = analise_server,
    id = 'analise'
  )

}


shinyApp(ui = ui, server = server)
