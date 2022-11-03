# # Develop an Analytics dashboard using R - Shiny App
# # Identify two or three important graphs from your analysis of component 1
# # Build an interactive graph using the R - shiny app (10 points)
# # Deploy the app in R - studio (8 points). [Please keep in your mind that most probably you will use the free account in R -
# #                                           
# studio, which does not give you more than 1 GB space. So  I will strongly recommend that develop your dashboard using a 
# small subset of data. It is okay if you do not use all the data. The purpose of this exercise is to demonstrate your capability] 
# Submit the link of the in moodle
# # Present your dashboard and record your session and upload your session on YouTube. Submit the YouTube link in moodle (7 points). 
# You can use zoom to record your session.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
library(gridExtra)
library(grid)
library(lattice)

merged_ghg = read.csv('GHG merged.csv')
merged_nd = read.csv('ND merged.csv')
merged_ghg_dis = read.csv("GHG ND merged.csv")
top10_ghg = read.csv("top10_ghg.csv")
top10_dis = read.csv("top10_dis.csv")


ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Component 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Univariate Analysis", tabName = "Univariate"),
      menuItem("Multivariate Analysis", tabName = "Multivariate"),
      menuItem("More Analysis", tabName = "MoreAnalysis")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Univariate",
              h2("Univariate Analysis of Greenhouse Gases and Natural Disasters"),
              fluidRow(
                tabBox(
                  title = "Datasets",
                  id = "tabset1", height = "100%", width = "100%",
                  tabPanel("Greenhouse Gases Emissions",
                           fluidRow(
                             column(3,
                                    selectInput(
                                      inputId = "uni_ghg_select",
                                      "Select Variable",
                                      c("Total GHG Emissions" = "Total GHG Emissions",
                                        "% change since 1990" = "% change since 1990",
                                        "Per capita GHG Emissions" = "Per capita GHG Emissions",
                                        "% of GHG Emissions from Energy" = "% of GHG Emissions from Energy",
                                        "% of GHG Emissions from Industry" = "% of GHG Emissions from Industry",
                                        "% of GHG Emissions from Agriculture" = "% of GHG Emissions from Agriculture",
                                        "% of GHG Emissions from Waste" = "% of GHG Emissions from Waste"
                                        )
                                    )
                                    ),
                             br(),
                             column(3,
                                    actionButton("uni_ghg_go", "Show me")
                                    )
                           ),
                           fluidRow(
                             column(6,
                                    plotOutput(
                                      outputId = "uni_ghg_bar"
                                    )),
                             column(6,
                                    br(),
                                    plotOutput(
                                      outputId = "uni_ghg_box"
                                    ))
                           )
                           ),
                  tabPanel("Natural Disasters",
                           fluidRow(
                             column(3,
                                    selectInput(
                                      inputId = "uni_nd_select",
                                      "Select Variable",
                                      c("Climatological Disasters" = "Climatological Disasters",
                                        "Hydrological Disasters" = "Hydrological Disasters",
                                        "Geophysical Disasters" = "Geophysical Disasters",
                                        "Meteorological Disasters" = "Meteorological Disasters"
                                      )
                                    )
                             ),
                             br(),
                             column(3,
                                    actionButton("uni_nd_go", "Show me")
                             )
                           ),
                           fluidRow(
                             column(6,
                                    plotOutput(
                                      outputId = "uni_nd_bar"
                                    )),
                             column(6,
                                    br(),
                                    plotOutput(
                                      outputId = "uni_nd_box"
                                    ))
                           )
                  )
                  
                )
              )
              ),
      tabItem(
        tabName = "Multivariate",
        h2("Multivariate Analysis"),
        fluidRow(
          tabBox(
            title = "Plots",
            id = "tabset1", height = "100%", width = "100%",
            tabPanel("Occurences of Disasters of top 10 Per Capita GHG Emitters",
              fluidRow(
                column(3,
                       selectInput(
                         inputId = "mul_nd_select",
                         "Select Disaster",
                         c("Total" = "Total",
                           "Climatological Disasters" = "Climatological Disasters",
                           "Hydrological Disasters" = "Hydrological Disasters",
                           "Geophysical Disasters" = "Geophysical Disasters",
                           "Meteorological Disasters" = "Meteorological Disasters"
                         )
                       )),
                column(3,
                       br(),
                       actionButton("mul_nd_go", "Show me"))
              ),
              fluidRow(
                column(
                  11,  align = "center",
                  plotOutput(
                    outputId = "mul_nd_bar"
                )
                
                )
              )
            ),
            tabPanel("Greenhouse Gases Emissions",
                     fluidRow(
                       column(6,
                              br(),
                              plotOutput("mul_ghg_1")),
                       column(6,
                              br(),
                              plotOutput("mul_ghg_2"))
                     ),
                     
            )
          )
        )),
      tabItem(
        tabName = "MoreAnalysis",
        h2("Total GHG Emissions and Total Disasters
            of top 10 GHG Emittors vs Rest of the World"),
        fluidRow(
          column(12, align = "center",
                 br(), br(),
                 plotOutput("pie_plot"))
        )
      )
    )
  )
)

server <- function(input, output) {

  uni_ghg_choice = reactive({
    if(input$uni_ghg_select == "Total GHG Emissions")
      "GHG.total.without.LULUCF..latest.year"
    else if (input$uni_ghg_select == "% change since 1990")
      "X..change.since.1990"
    else if (input$uni_ghg_select == "Per capita GHG Emissions")
      "GHG.emissions.per.capita...latest.year"
    else if (input$uni_ghg_select == "% of GHG Emissions from Energy")
      "GHG.from.energy..as.percentage.to.total"
    else if (input$uni_ghg_select == "% of GHG Emissions from Industry")
      "GHG.from.industrial.processes.and.product.use..as.percentage.to.total"
    else if (input$uni_ghg_select == "% of GHG Emissions from Agriculture")
      "GHG.from.agriculture..as.percentage.to.total"
    else if (input$uni_ghg_select == "% of GHG Emissions from Waste")
      "GHG.from.waste..as.percentage.to.total"
  })
  
  
  observeEvent(input$uni_ghg_go,{
    

    
    output$uni_ghg_bar = renderPlot({
      isolate(
        ggplot(merged_ghg[1:10,],
               aes_string(x = paste0("reorder(Country,",uni_ghg_choice(),")"),
                   y =  uni_ghg_choice())
               ) +
          geom_bar(stat = "identity", fill = "mediumpurple") +
          coord_flip() +
          xlab('Country') + ylab(input$uni_ghg_select)
      )
    }, height = "auto")
    
    output$uni_ghg_box = renderPlot({
      isolate(
        ggplot(merged_ghg)+
          geom_boxplot(
               aes_string(paste0(uni_ghg_choice())), fill = "mediumpurple"
        ) + xlab(input$uni_ghg_select)
      )
    }, height = 200)
  })
  
  uni_nd_choice = reactive({
    if(input$uni_nd_select == "Climatological Disasters"){
      "CTotal_Occurences"
    }
    else if(input$uni_nd_select == "Hydrological Disasters"){
      "HTotal_Occurences"
    }
    else if(input$uni_nd_select == "Geophysical Disasters"){
      "GTotal_Occurences"
    }
    else if(input$uni_nd_select == "Meteorological Disasters"){
      "MTotal_Occurences"
    }
  })
  
  observeEvent(input$uni_nd_go,{
    

    
    output$uni_nd_bar = renderPlot({
      isolate(
        ggplot(merged_nd[1:10,],
               aes_string(x = paste0("reorder(Countries.or.areas,",uni_nd_choice(),")"),
                          y = uni_nd_choice())
        ) +
          geom_bar(stat = "identity", fill = "mediumpurple") +
          coord_flip() +
          xlab('Country') + ylab(input$uni_nd_select)
      )
    }, height = "auto")
      
    output$uni_nd_box = renderPlot({
      isolate(
        ggplot(merged_nd)+
          geom_boxplot(
            aes_string(paste0(uni_nd_choice())), fill = "mediumpurple"
          ) + xlab(input$uni_nd_select)
      )
    }, height = 200)
  })
  
  mul_nd_choice = reactive({
    if(input$mul_nd_select == "Total"){
      "Total_Occurences"
    }
    else if(input$mul_nd_select == "Climatological Disasters"){
      "CTotal_Occurences"
    }
    else if(input$mul_nd_select == "Hydrological Disasters"){
      "HTotal_Occurences"
    }
    else if(input$mul_nd_select == "Geophysical Disasters"){
      "GTotal_Occurences"
    }
    else if(input$mul_nd_select == "Meteorological Disasters"){
      "MTotal_Occurences"
    }
  })
  
  observeEvent(input$mul_nd_go,{
    
    output$mul_nd_bar = renderPlot({
      isolate(
        ggplot(merged_ghg_dis[1:10,],
               aes(x = reorder(Country, GHG.emissions.per.capita...latest.year),
                   y = GHG.emissions.per.capita...latest.year)) +
          geom_bar(stat = "identity", aes_string(fill = mul_nd_choice()))+
          coord_flip() +
          xlab("Country") + ylab("Per Capita GHG Emissions") +
          theme(legend.position = "top") 
        # +
        #   labs(title = "Total Occurrences of Climatological
        #                 Disasters of top 10 Per Capita GHG Emitters",
        #        fill = "Total Occurrences")
      )
    }, width = 800, height = "auto")
  
  })
  
  output$mul_ghg_1 = renderPlot({
    ggplot(merged_ghg[1:10,],
           aes(x = reorder(Country, GHG.total.without.LULUCF..latest.year),
               y = GHG.total.without.LULUCF..latest.year)) +
      geom_bar(stat = "identity",
               aes(fill = GHG.emissions.per.capita...latest.year)) +
      coord_flip() + scale_fill_gradient(low = 'yellow', high = 'red') +
      ylab('Total GHG Emissions') + xlab('Country') +
      labs(fill = "Per Capita GHG Emissions") +
      theme(legend.position="top")
  })
  
  output$mul_ghg_2 = renderPlot({
    ggplot(merged_ghg[c(1:8,10,11),],
           aes(x = reorder(Country, GHG.emissions.per.capita...latest.year),
               y = GHG.emissions.per.capita...latest.year)) +
      geom_bar(stat = "identity", aes(fill = X..change.since.1990)) +
      scale_y_reverse() + 
      coord_flip() + scale_fill_gradient(low = 'yellow', high = 'red')+
      ylab('Per Capita GHG Emissions') + xlab('Country') +
      labs(fill = "Percentage change from 1990-2019") +
      theme(legend.position="top") + scale_x_discrete(position = "top")
  })
    
  output$pie_plot = renderPlot({
    p1 = ggplot(top10_ghg, aes(x = "", y = values, fill = group)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Total GHG Emissions ") +
      xlab("") + ylab("") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    p2 = ggplot(top10_dis, aes(x = "", y = values, fill = group)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Total Occurrences of Disasters") +
      xlab("") + ylab("") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    grid.arrange(p1, p2, ncol=2)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
