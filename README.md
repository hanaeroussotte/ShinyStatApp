# ShinyStatApp
library(shiny)
library(ggplot2)
library(shinythemes)
library(desctable)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(viridis)



ui<- fluidPage(
  theme = shinytheme("sandstone"),
  
  sidebarLayout(
    sidebarPanel( 
      titlePanel("Charger votre fichier"),
      fileInput('file1', 'Choisir fichier CSV',
                accept=c('.csv')),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   choices = c(Comma=',', Semicolon=';', Tab='\t'),
                   selected = ','),
      
      selectInput('xcol', 'X Variable', "", selected = NULL),
      selectInput('ycol', 'Y Variable', "", selected = NULL)
    ),
    
    mainPanel(
      h3("Graphique représentant Y en fonction de X"),
      plotOutput('MyPlot'),
      tabsetPanel(
        tabPanel(title="Tableau descriptif",  
                 tableOutput('summary')),
        tabPanel(title="Tableau comparatif", 
                 h3('Vous soumettez votre variable X à une comparaison statistique'),
                 tableOutput('comptable')),
        tabPanel(title="Figures comparatives", 
                 h1("Figures comparatives"),
                 h2("Vous comparez vos deux variables X et Y"),
                 plotOutput('fig1'),
                 plotOutput('fig2')),
        tabPanel(title="Figures descriptives",
                 h1("Figures descriptives pour description univariée"),
                 h2('Vous soumettez votre variable X à une description statistique'),
                 h4("Histogramme \n", plotOutput('histo'),
                    h4("Graphique de densité \n"), plotOutput('density'),
                    h4("Dot Plot"), plotOutput('dotplot')
                 )
        ) 
      )
    )
  )
)

server <- function(input, output, session) {
  # output$files <- renderTable(input$file1$datapath)
  # output$summary<-renderPrint(input$file1$datapath)
  myfiles <- reactive({
    req(input$file1$datapath, file.exists(input$file1$datapath))
    read.csv(input$file1$datapath)
  })
  
  observeEvent(myfiles(), {
    req(myfiles())
    nms <- colnames(myfiles())
    updateSelectInput(
      session, 
      inputId = 'xcol', 
      label = 'X Variable',
      choices = nms, selected = nms[1]
    )
    
    updateSelectInput(
      session, 
      inputId = 'ycol', 
      label = 'Y Variable',
      choices = nms, selected = nms[1]
    )
  })
  
  #Graphique en fonction de variable choisie par l'utilisateur
  
  output$MyPlot <- renderPlot({
    req(myfiles(), input$xcol, input$ycol)
    ggplot(data = myfiles(), mapping = aes_string(input$xcol, input$ycol)) +
      geom_point() +
      theme_gray()
  })
  
  #Summary  
  output$summary <- renderTable(
    myfiles() %>% 
      desctable(stats=stats_auto) 
  )
  
  # Tableau comparatif
  output$comptable <- renderTable(
    myfiles()%>%
      group_by(str_c(input$xcol))%>%
      desctable(tests=tests_auto)
  )
  
  # Histogramme
  output$histo <- renderPlot({
    req(myfiles(), input$xcol)
    barplot(myfiles()[[input$xcol]], data=myfiles())
  })
  
  
  
  output$freqpol <- renderPlot({
    req(myfiles(), input$xcol, input$ycol)
    ggplot(data = myfiles(), mapping = aes_string(x=input$xcol)) +
      geom_freqpoly()
  })
  
  #graph densite   
  output$density <- renderPlot({
    req(myfiles(), input$xcol, input$ycol)
    ggplot(data = myfiles(), mapping = aes_string(x=input$xcol)) +
      geom_density()
  })
  
  #Graph point 
  output$dotplot <- renderPlot({
    req(myfiles(), input$xcol, input$ycol)
    ggplot(data = myfiles(), mapping = aes_string(x=input$xcol)) +
      geom_dotplot()
    
  })
  
  
  #Figures comparatives
  #Analyse de variance
  #Parametrique
  output$fig1<-renderPlot(
    ggviolin(myfiles(),x=input$xcol, y=input$ycol,color="blue",add="boxplot")
    + stat_compare_means(method="anova", label.y=60)
  )
  #Non-parametrique
  output$fig2<-renderPlot(
    ggboxplot(myfiles(),x=input$xcol, y=input$ycol, color="red", add="jitter")
    + stat_compare_means(method="kruskal.test")
    + stat_compare_means(method="wilcox.test", label="p.signif")
  ) 
  
}

shinyApp(ui , server)
