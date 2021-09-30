#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(data.table)
dataset1<-read.csv("https://raw.githubusercontent.com/ActionAnalyticsCFB/Stats/main/StatsWRanks.csv",stringsAsFactors = FALSE)
hexcolors<-read.csv("https://raw.githubusercontent.com/ActionAnalyticsCFB/RecruitingPostseasonRankings/main/HexColors.csv")
hexcolorsDiff <-read.csv("https://raw.githubusercontent.com/ActionAnalyticsCFB/Stats/main/ColorsDiff.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(title=div("Stats Comparison", img(src="ActionLogo.png", style = "float:right;", height = 60, width = 68),
        fluidRow(
            column(
                width = 6,
                    div(style = "font-size:14px; text-align:left;",
                        "Dashboard built by: ",
                            tags$a(href = "https://github.com/wjbashamStats/CFB",
                                "Action Analytics", target = "_blank")
                    ),
            ),
        )
    )
    ),
    navbarPage("Games Stastic Comparisons:",
        tabPanel("Spider Plot - Offense vs Defense",fluidPage(theme = shinytheme("flatly")),
        verticalLayout(
            mainPanel(
                column(12, plotlyOutput("plot1", width = 1100, height=1100),  align = "center"),
            ),
        
            sidebarPanel(
                selectInput(inputId = "Game",        
                    label = "Choose Game To Analyze",
                    choices = unique(dataset1$matchup),
                    selected = "Nebraska @ Illinois"
                ),
                selectInput(inputId = "Set",        
                            label = "Home O vs Away D or Home D vs Away O",
                            choices = list("Home O vs Away D", "Home D vs Away O"),
                            selected = "Home O vs Away D"),
            ),
            fluid = TRUE
        )),
        tabPanel("Table", target="_blank", ".",style = "font-size:25px",
                 splitLayout(
                     mainPanel(
                         column(12, dataTableOutput("table1"))
                     )
                 )
        ),
        tabPanel("Stat Differential", target="_blank", ".",style = "font-size:25px",
                 verticalLayout(
                     mainPanel(
                         column(12, dataTableOutput("table2"))
                     ),
                     sidebarPanel(
                         column(width = 12,
                            selectInput(inputId = "StatDifferential",        
                            label = "Choose First Stat To Analyze",
                            selected = 'home_off_plays',
                            choices = colnames(dataset1)[12:175]
                            )
                        )
                     ),
                     sidebarPanel(
                         column(width = 12,
                                selectInput(inputId = "StatDifferential2",        
                                            label = "Choose Second Stat To Analyze",
                                            selected = 'away_def_plays',
                                            choices = colnames(dataset1)[12:175]
                                )
                         )
                     )
                 )
        ),
        tabPanel("Glossary", target="_blank", ".",style = "font-size:25px",
            p("The abbreviations used in the radar chart are:
              ",style = "font-size:25px"),
            hr(), 
            p("PP = Pass Play",style = "font-size:20px;color: blue"),
            p("RP = Run Play",style = "font-size:20px;color: blue"),
            p("PD = Passing Down",style = "font-size:20px;color: blue"),
            p("SD = Standard Down",style = "font-size:20px;color: blue"),
            p("FD = Finishing Drives",style = "font-size:20px;color: blue"),
            p("SP+ = SP+ Ranking",style = "font-size:20px;color: blue"),
            
            p("SR = Success Rate. Definition: an efficiency metric that determines the success of a play. 
              Successful plays meet one of the following criteria:
                The offense scored.
                1st downs which gain at least 50% of the yards to go.
                2nd downs which gain at least 70% of the yards to go.
                3rd and 4th downs which gain at least 100% of the yards to go.",style = "font-size:20px;color: blue"),
            p("Success Rate Notes: On offense you want a high success rate, on Defense you want a low Success Rate.",style = "font-size:20px;color: blue"),
            p("EPA/Play: measure the outcome of a play. 
              It takes the EP value from the beginning of a play (e.g. 2nd and 5 at the 50) and subtracts it from the EP value 
              resulting from the play (e.g. rush for 10 yards results in 1st and 10 from the 40). Important Note Here,
              If you're on Offense a high EPA/Play is good. On Defense you want a low EPA/Play (it is actually EPA/Play Allowed on Defense)",style = "font-size:20px;color: blue"),
            p("Havoc: Refers to the percentage of plays in with the defense recorded a TFL, forced a fumble, intercepted a pass or broke up a pass.
              On defense you want a high Havoc rate, on offense you want a low Havoc Rate (it is actually Havoc Allowed).",style = "font-size:20px;color: blue"),
            p("Passing Downs: Passing Downs are defined as 2nd downs with 7 or more yards to go or 3rd and 4th downs with 5 or more yards to go.",style = "font-size:20px;color: blue"),
            p("Standard Down: anything that is not considered to be a passing down.",style = "font-size:20px;color: blue"),
            p("So, For Example. If a Defense ranks 128th in standard down success rate, that means that out of all their standard downs, they let their opponents get a successful play at a higer rate than anyone else in the country on standard downs ",style = "font-size:20px;color: blue"),
            p("Another Example: If an offense ranks 1st on passing play EPA/Play, it means it had the highest EPA per play on passing plays in the country ",style = "font-size:20px;color: blue"),
            
            hr()
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    selectedData1 <-reactive({
        filter(dataset1,matchup %in% input$Game)
    })
    homeOffenseStats <- reactive({
        selectedData1() %>%
            select(8, 90, 91, 16, 26, 17, 89, 14) 
    })
    AwayOffenseStats <- reactive({
        selectedData1() %>%
            select(10, 170, 171, 96, 106, 97, 169,94) 
    })
    homeDefenseStats <- reactive({
        selectedData1() %>%
            select(8, 90, 91, 54, 64, 55, 88, 52) 
    })
    AwayDefenseStats <- reactive({
        selectedData1() %>%
            select(10, 170, 171, 134, 144, 135, 168, 132) 
    })
    
    homeStatsForTable<-reactive({
        selectedData1() %>%
            select(90, 91, 16, 26, 17, 89, 14, 54, 64, 55, 88, 52,33, 34, 35,37,38,39, 41,43,44, 46, 48, 49) 
    })
    
    awayStatsForTable<-reactive({
        selectedData1() %>%
        select(170, 171, 134, 144, 135, 168, 132,96, 106, 97, 169,94,151,152, 153,155,157,158, 160,162,163, 165, 166, 167)
    })
    rowNames <- c(
      "Home SP+ vs Away SP+",
      "Home TAN PR vs TAN PR",
      "Home Off SR vs Away Def SR",
      "Home Off Finishing Drives vs Away Def Finishing Drives",
      "Home Off Exp vs Away Def Exp",
      "Home Off Havoc vs Away Def Havoc ",
      "Home Off EPA/Play vs Away Def EPA/Play",
      ###########
      "Home DEF SR vs Away Off SR",
      "Home Def Finishing Drives vs Away Off Finishing Drives",
      "Home Def Exp vs Away Off Exp",
      "Home Def Havoc vs Away OFf Havoc ",
      "Home Def EPA/Play vs Away Off EPA/Play",
      "Home Standard Down EPA/Play vs Away Standard Down EPA/Play Allowed",
      "Home Standard Down Success Rate vs Away Standard Down Success Rate Allowed",
      "Home Standard Down Explosiveness vs Away Standard Down Explosiveness",
      "Home Passing Down EPA/Play vs Away Passing Down EPA/Play Allowed",
      "Home Passing Down Success Rate vs Away Passing Down Success Rate Allowed",
      "Home Passing Down Explosiveness vs Away Passing Down Explosiveness",
      "Home Rushing Play EPA/Play vs Away Rushing Play EPA/Play Allowed",
      "Home Rushing Play Success Rate vs Away Rushing Play Success Rate Allowed",
      "Home Rushing Play Explosiveness vs Away Rushing Play Explosiveness",
      "Home Passing Play EPA/Play vs Away Passing Play EPA/Play Allowed",
      "Home Passing Play Success Rate vs Away Passing Play Success Rate Allowed",
      "Home Passing Play Explosiveness vs Away Passing Play Explosiveness")
    
    
    tableFinal<-reactive({
        table<-cbind(rowNames,as.data.frame(t(homeStatsForTable())),as.data.frame(t(awayStatsForTable()))) 
        colnames(table) <- c("Stats (Ranks)", homeOffenseStats()[1,1], AwayOffenseStats()[1,1])
        table$absDifference <- abs(as.numeric(table[,2])-as.numeric(table[,3]))        
        return(table)
    })
  
    
    output$table1 <- renderDataTable(
                        datatable(
                            tableFinal(), 
                            extensions = 'Buttons',
                            filter = 'bottom',
                            options = list(
                                dom = 'Bfrtip',
                                buttons = 
                                    list('copy', 'print', list(
                                        extend = 'collection',
                                        buttons = c('csv', 'excel', 'pdf'),
                                        text = 'Download')),
                                iDisplayLength = 75,
                                columnDefs = list(list(targets = 4, visible = TRUE), list(targets = 0, visible = FALSE))

                            ) 
                        ) 
                        %>%
                            formatStyle(
                                homeOffenseStats()[1,1],
                                backgroundColor = styleInterval(hexcolors[1:129,1], hexcolors[,2]),
                                fontWeight = 'bold'
                            )
                     
                        %>%
                            formatStyle(
                                AwayOffenseStats()[1,1],
                                backgroundColor = styleInterval(hexcolors[1:129,1], hexcolors[,2]),
                                fontWeight = 'bold'
                      
                        
                     )
                     %>%
                       formatStyle(
                         "absDifference",
                         backgroundColor = styleInterval(hexcolorsDiff[1:263,1], hexcolorsDiff[,2]),
                         fontWeight = 'bold'
                       )
    )
    

    selectedData2 <-reactive({
        listOFNum<-dataset1[,c('matchup',input$StatDifferential, input$StatDifferential2 )]
        listOFNum$absDifference <- abs(as.numeric(listOFNum[,2])-as.numeric(listOFNum[,3]))
        return(listOFNum)
    })    
    

    
    output$table2 <- renderDataTable(
        datatable(
            selectedData2(), 
             extensions = 'Buttons',
             filter = 'bottom',
             options = list(
                 dom = 'Bfrtip',
                 buttons = 
                     list('copy', 'print', list(
                         extend = 'collection',
                         buttons = c('csv', 'excel', 'pdf'),
                         text = 'Download')),
                 iDisplayLength = 75            
             ) 
         ) 
         %>%
             formatStyle(
                 input$StatDifferential,
                 backgroundColor = styleInterval(hexcolors[1:129,1], hexcolors[,2]),
                 fontWeight = 'bold'
             )
         %>%
             formatStyle(
                 input$StatDifferential2,
                 backgroundColor = styleInterval(hexcolors[1:129,1], hexcolors[,2]),
                 fontWeight = 'bold'
             )
        %>%
            formatStyle(
                "absDifference",
                backgroundColor = styleInterval(hexcolorsDiff[1:263,1], hexcolorsDiff[,2]),
                fontWeight = 'bold'
            )
    )
    

    
    
    output$plot1 <- renderPlotly({
        if (input$Set == 'Home O vs Away D') {
            plot_ly(
                type = 'scatterpolar',
                mode = "lines+markers",
                fill = 'toself'
            ) %>%
                add_trace(
                    r = as.matrix(homeOffenseStats()[1,2:8]),
                    theta = c("SP+ ",
                              "TAN PR ",
                              "Success Rate ",
                              "Finishing Drives ",
                              "Explosiveness ",
                              "Havoc ",
                              "EPA/Play "),
                    showlegend = TRUE,
                    mode = "markers",
                    name = homeOffenseStats()[1,1]
                )%>%
                add_trace(
                    r = as.matrix(AwayDefenseStats()[1,2:8]),
                    theta = c("SP+ ",
                              "TAN PR ",
                              "Success Rate ",
                              "Finishing Drives ",
                              "Explosiveness ",
                              "Havoc ",
                              "EPA/Play "),
                    showlegend = TRUE,
                    mode = "markers",
                    visible="legendonly",
                    name = AwayDefenseStats()[1,1]
                )%>%
                layout(
                    title = 
                        list(text = paste0(homeOffenseStats()[1,1], "'s Offensive Ranks vs ", AwayDefenseStats()[1,1], "'s Defensive Ranks"),
                            font = list(
                                family = 'Arial',
                                size = 30,
                                color = '#000'
                            ),
                    xanchor = 'center', 
                    yanchor =  'top'),
                    polar = list(
                        radialaxis = list(
                            visible = T,
                            range = c(131,0)
                        ),
                       legend = list(orientation = 'h')
                    ),
                    showlegend=TRUE
                )
        }
        else{
            plot_ly(
                type = 'scatterpolar',
                mode = "lines",
                fill = 'toself'
            ) %>%
                add_trace(
                    r = as.matrix(homeDefenseStats()[1,2:8]),
                    theta = c("SP+ ",
                                "TAN PR ",
                                "Success Rate ",
                                "Finishing Drives ",
                                "Explosiveness ",
                                "Havoc ",
                                "EPA/Play "),
                    showlegend = TRUE,
                    mode = "markers",
                    name = homeDefenseStats()[1,1]
                )%>%
                add_trace(
                    r = as.matrix(AwayOffenseStats()[1,2:8]),
                    theta = c("SP+ ",
                                "TAN PR ",
                                "Success Rate ",
                                "Finishing Drives ",
                                "Explosiveness ",
                                "Havoc ",
                                "EPA/Play "),
                    showlegend = TRUE,
                    mode = "markers",
                    visible="legendonly",
                    name = AwayOffenseStats()[1,1]
                )%>%
                layout(
                    title = 
                        list(text = paste0(homeOffenseStats()[1,1], "'s Defensive Ranks vs ", AwayDefenseStats()[1,1], "'s Offensive Ranks"),
                        xanchor = 'center', yanchor =  'top',
                        font = list(
                          family = 'Arial Black',
                          size = 30,
                          color = '#000'
                        )
                        ),
                    polar = list(
                        radialaxis = list(
                            visible = T,
                            range = c(131,0)
                        ),
                        legend = list(orientation = 'h')
                    ),
                    showlegend=TRUE
                )
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
