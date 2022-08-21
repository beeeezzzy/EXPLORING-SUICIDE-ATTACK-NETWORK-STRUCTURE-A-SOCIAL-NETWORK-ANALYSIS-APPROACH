library(shiny)
library(igraph)
library(statnet)
library(intergraph)
library(shinyjs)

#front end
ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_node", "Upload the CSV node file"),
      fileInput("file_ties", "Upload the CSV ties file"),
      selectInput("layout", "Select layout", c("Fruchtermanreingold",
                                               "Kamadakawai",
                                               "Circle",
                                               "Tree")),
      selectInput("layout2", "Select Ego Network", c("Al-Qaeda",
                                                     "Hezbollah",
                                                     "AQI")),
      selectInput("layout1", "Select Community Detection Algorithm", c("Cluster Walktrap",
                                                                       "Cluster Label Prop",
                                                                       "Cluster Infomap",
                                                                       "Cluster Optimal")),
      sliderInput("slide1", "Vertex Label Size", min = 0.1, max = 1.0, value = 0.7),
      sliderInput("slide2", "Edge Arrow Size", min = 0.1, max = 1.0, value = 0.3),
      textOutput("text1"),
      actionButton("hide", "Hide Plot"),
      actionButton("show", "Show Plot"),
     
    #sideBarPanel  
    ),
    
    mainPanel(
      h1("R Plot"),
      h2("Visual Representation of the Network"),
      plotOutput("display_output", width = "100%"),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      h2("Ego Network of Prominent Actors"),
      plotOutput("display_output2", width = "100%"),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      h2("Cluster Network"),
      plotOutput("display_output1", width = "100%")
      #mainPanel
    ),
    
  #sidebarLayout
  )
  
#fluidPage
)


#back end
server <- function(input, output, session){
  
  observeEvent(input$hide, {
    hide("display_output")
    hide("display_output1")
  })
  
  observeEvent(input$show, {
    show("display_output")
    show("display_output1")
  })
  
   dataset1 <- reactive({
    
    d1 <- read.csv(input$file_node$datapath, sep = ",", header = T)
    d2 <- read.csv(input$file_ties$datapath, sep = ",", header = T)
    
    d3 <- graph_from_data_frame(d=d2, directed= TRUE, vertices=d1)
    
    return(d3)
  })
  
    get_degree <- reactive({
    d3_indeg <- igraph::degree(dataset1(), mode= c("in"))
    
    return(d3_indeg)
  })
  
  output$display_output <- renderPlot({
  
  colors <- c(paste0(rep("grey",80),seq(80,1)))
  
  name <- input$layout
  if(name == "Fruchtermanreingold"){
    
    plot(dataset1(),
         vertex.label.cex = as.numeric(input$slide1),
         vertex.label.color="black",
         vertex.size = log(get_degree())*2,
         vertex.color = colors[get_degree()],
         edge.arrow.size= as.numeric(input$slide2), 
         edge.width = E(dataset1())$weight,
         layout=layout_with_fr)
    
  }else if (name == "Kamadakawai"){
    
    plot(dataset1(),
         vertex.label.cex = as.numeric(input$slide1),
         vertex.label.color="black",
         vertex.size = log(get_degree())*2,
         vertex.color = colors[get_degree()],
         edge.arrow.size= as.numeric(input$slide2), 
         edge.width = E(dataset1())$weight,
         layout=layout_with_kk)
    
  }else if (name == "Circle"){
    
    plot(dataset1(),
         vertex.label.cex = as.numeric(input$slide1),
         vertex.label.color="black",
         vertex.size = log(get_degree())*2,
         vertex.color = colors[get_degree()],
         edge.arrow.size= as.numeric(input$slide2), 
         edge.width = E(dataset1())$weight,
         layout=layout_in_circle)
    
  }else if (name == "Tree"){
    
    plot(dataset1(),
         vertex.label.cex = as.numeric(input$slide1),
         vertex.label.color="black",
         vertex.size = log(get_degree())*2,
         vertex.color = colors[get_degree()],
         edge.arrow.size= as.numeric(input$slide2), 
         edge.width = E(dataset1())$weight,
         layout=layout_as_tree)
    
  }
  
  #Output$display_output
  }, 
  height = 800, 
  width = 1200)
  
  output$display_output1 <- renderPlot({
    
    name1 <- input$layout1
    if(name1 == "Cluster Walktrap"){
      
      wt <- cluster_walktrap(dataset1())
      modularity(wt)
      
      plot(wt, 
           dataset1(),
           vertex.label.cex = as.numeric(input$slide1),
           vertex.label.color = "black",
           vertex.size = 5,
           edge.arrow.size = as.numeric(input$slide2),
           edge.color = "gray",
           layout=layout.fruchterman.reingold)
      
    }else if (name1 == "Cluster Label Prop"){
      lp <- cluster_label_prop(dataset1())
      modularity(lp)
      
      plot(lp, 
           dataset1(),
           vertex.label.cex = as.numeric(input$slide1),
           vertex.label.color = "black",
           vertex.size = 5,
           edge.arrow.size = as.numeric(input$slide2),
           edge.color = "gray",
           layout=layout.fruchterman.reingold)
      
    }else if (name1 == "Cluster Infomap"){
      im <- cluster_infomap(dataset1())
      modularity(im)
      
      plot(im, 
           dataset1(),
           vertex.label.cex = as.numeric(input$slide1),
           vertex.label.color = "black",
           vertex.size = 5,
           edge.arrow.size = as.numeric(input$slide2),
           edge.color = "gray",
           layout=layout.fruchterman.reingold)
      
    }else if (name1 == "Cluster Optimal"){
      op <- cluster_infomap(dataset1())
      modularity(op)
      
      plot(op, 
           dataset1(),
           vertex.label.cex = as.numeric(input$slide1),
           vertex.label.color = "black",
           vertex.size = 5,
           edge.arrow.size = as.numeric(input$slide2),
           edge.color = "gray",
           layout=layout.fruchterman.reingold)
      
    }
    #Output$display_output1
  }, 
  height = 800, 
  width = 1200)
  
  output$display_output2 <- renderPlot({
    
    net1 <- asNetwork(dataset1())
    
    name2 <- input$layout2
    if(name2 == "Al-Qaeda"){
      
      A_ego <- ego.extract(net1, ego = 26 , neighborhood = c("in"))
      
      gplot(A_ego,
            gmode = "graph",
            mode="fruchtermanreingold",
            displayisolates = F,
            displaylabels=T,
            label.cex=as.numeric(input$slide1),
            main = "Al-Qaeda's Ego Network")
      
    }else if (name2 == "Hezbollah"){
      
      H_ego <- ego.extract(net1, ego = 3 , neighborhood = c("in"))
      
      gplot(H_ego,
            gmode = "graph",
            mode="fruchtermanreingold",
            displayisolates = F,
            displaylabels=T,
            label.cex=as.numeric(input$slide1),
            main = "Hezbollah's Ego Network")
      
    }else if (name2 == "AQI"){
      AQ_ego <- ego.extract(net1, ego = 51 , neighborhood = c("in"))
      
      gplot(AQ_ego,
            gmode = "graph",
            mode="fruchtermanreingold",
            displayisolates = F,
            displaylabels=T,
            label.cex=as.numeric(input$slide1),
            main = "AQI's Ego Network")
    }
    #Output$display_output1
  }, 
  height = 800, 
  width = 1200)
#server
}


shinyApp(ui, server)