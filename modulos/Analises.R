


##### Funções ------------------------------------------------------------------

basic <- function(x, more=F) {
  stats <- list()
  
  stats$N <- length(x)
  stats$Media <- round(mean(x),2)
  stats$D.P <- round(sd(x),2)
  
  t1 <- unlist(stats)
  names(t1) <- c("N", "Média", "D.P.")
  t1
}



##### Leitura e manipulação dos Dados ------------------------------------------


Dados <- reactive({
  
  df <- mtcars %>% 
    mutate(
      Cambio = case_when(
        am == 1 ~ 'Automático',
        am == 0 ~ 'Manual'
      ))
  
  return(df)
  
})

# isolate(Dados())


##### shiny --------------------------------------------------------------------

analise_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = 'analise',
    h1(
      style='color: #1E2733; font-size: 25px;',
      align = 'center',
      'Exemplos de análises'
    ),
    br(),
    fluidRow(
      box(
        width = 6,
        status = "primary",
        title = 'Descrição de variáveis categóricas', 
        gt::gt_output(ns('tabela_cat'))
      ),
      box(
        width = 6,
        status = "primary",
        title = 'Descrição de variáveis numéricas', 
        DT::dataTableOutput(ns('tabela_num'))
      )
    ),
    br(),
    br(),
    fluidRow(
      box(width = 4,
          status = 'primary',
          selectInput(inputId = ns('cambio'),
                      label = 'Selecione a câmbio', 
                      choices = c('Automático',   'Manual', 'Todos'))
      )
    ),
    br(),
    fluidRow(
      box(width = 6,
          status = 'primary',
          radioButtons(
            inputId = ns('cilindros'),
            label = 'Selecione o número de cilindros',
            choices = c(Dados() %>%
                          pull(cyl) %>%
                          unique(), 'Todos')),
          title = 'Gráfico da média de milhas/galão por número de marchas',
          plotlyOutput(ns('cilindros_plotly'))
      ),
      box(width = 6,
          status = 'primary',
          title = 'Gráfico da média de milhas/galão por número de marchas', 
          plotlyOutput(ns('cilindros_ggplot'))
      )
    ),
    br(),
    fluidRow(
      box(width = 6,
          status = 'primary',
          title = 'Gráfico da média de potência por número de marchas', 
          highchartOutput(ns('cambio_plot'))
      )
    )
  )
}


analise_server <- function(input, output, session) {
  
  
  
  output$tabela_cat <- gt::render_gt({
    
    tabela <- Dados() %>% 
      dplyr::select(cyl,
                    vs,
                    am,
                    gear) 
    
    colnames(tabela) <- c('Número de cilindros',
                          'Motor (0 = V-shaped, 1 = straight)',
                          'Tipo de marcha (0 = automática, 1 = manual)',
                          'Número de marchas para frente')
    
    tabela %>% 
      tbl_summary(
        type = all_categorical() ~ "categorical") %>% 
      modify_header(update =list(label  ~  "**Variáveis**"))%>% 
      bold_labels()  %>% as_gt()
    
  })
  
  output$tabela_num <- DT::renderDataTable({
    
    tabela <- t(apply(Dados() %>% dplyr::select(mpg,
                                                disp,
                                                hp,
                                                drat,
                                                wt,
                                                qsec), 2, basic)) %>% 
      data.frame() %>% 
      transmute(Variáveis = c('Milhas/(EUA) galão',
                              'Deslocamento (cu.in.)',
                              'Potência bruta',
                              'Relação do eixo traseiro',
                              'Peso (1000 libras)',
                              'Tempo de 1/4 de milha'),
                N,
                Média,
                D.P.)
    
    tabela %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    extensions = c('Buttons'), 
                    options = 
                      list(pageLength = 20,
                           dom = 'Bdt',
                           buttons = c('excel', 'pdf'),
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#1E2733', 'color': '#fff'});",
                             "}")
                      )
      )
  })
  
  tabela_plot <- reactive({
    
    req(input$cambio)
    req(input$cilindros)
    
    if (input$cambio == 'Todos') {
      tabela_plot1 <- df %>%
        group_by(gear) %>% 
        summarise(Media = mean(mpg)) %>% 
        ungroup()
      
    } else {
      tabela_plot1 <- Dados() %>% 
        filter(Cambio == input$cambio) %>% 
        group_by(gear) %>% 
        summarise(Media = mean(mpg)) %>% 
        ungroup()
    }
    
    
    if (input$cilindros == 'Todos') {
      tabela_plot2 <- Dados() %>%
        group_by(gear) %>% 
        summarise(Media = mean(hp)) %>% 
        ungroup()
      
    } else {
      tabela_plot2 <- Dados() %>% 
        filter(cyl == input$cilindros) %>% 
        group_by(gear) %>% 
        summarise(Media = mean(hp)) %>% 
        ungroup()
    }
    
    list(tabela_plot1 = tabela_plot1,
         tabela_plot2 = tabela_plot2)
    
  })
 
  
  output$cambio_plot <- renderHighchart({
    
    dados <- tabela_plot()[['tabela_plot1']] %>% 
      mutate(Media = round(Media, 2))
    
      highchart() %>% 
      hc_add_series(data = dados, 
                    type = 'column', 
                    hcaes(x = gear, 
                          y = Media)) %>% 
      hc_xAxis(title = list(text = 'Número de marchas')) %>% 
      hc_yAxis(title = list(text = 'Média da Potência')) %>% 
      hc_tooltip(pointFormat = "Número de marchas: <b> {point.x}</b><br>
                 Média da Potência: <b> {point.y}</b>") %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>% 
      hc_legend(NULL)
    
  })
  
  
  
  output$cilindros_plotly <- renderPlotly({
    
    Dados <- tabela_plot()[['tabela_plot2']] %>% 
      mutate(Media = round(Media, 2),
             gear = as.factor(gear))
    
    fig <- plot_ly(Dados, 
                   x = ~gear, y = ~Media, 
                   type = 'bar', 
                   name = '')
    
    fig <- fig %>% layout(title = '',
                          xaxis = list(title = 'Número de marchas'),
                          yaxis = list(title = 'Milhas/galão'),
                          margin = list(b = 100),
                          barmode = 'group'
                          )
    
    fig
    
    
  })
  
  
  output$cilindros_ggplot <- renderPlotly({
    
    dados3 <- tabela_plot()[['tabela_plot2']] %>% 
      mutate(Media = round(Media, 2))
    
    ggplotly(ggplot(data = dados3,
                    aes(x = gear,
                        y = Media)) +
               geom_bar(fill = 'skyblue3', 
                        stat = 'identity') +
               xlab('Número de marchas') + ylab('Milhas/galão'))
    
  })

}