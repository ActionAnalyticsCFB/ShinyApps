library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(ggimage)
library(dplyr)  # For filtering and manipulating data
library(png)
library(grid)
library(stringr)
library(plotly)
library(ggrepel)
library(magick)
library(cowplot)
library(cfbfastR)
library(shinythemes)
library(tidyr)

dataset1<-read.csv("https://raw.githubusercontent.com/ActionAnalyticsCFB/Stats/main/EditedStatsNoRanks.csv",stringsAsFactors = FALSE)
dataset2<-read.csv("https://raw.githubusercontent.com/ActionAnalyticsCFB/Stats/main/EditedStatsNoRanks.csv",stringsAsFactors = FALSE)

actionLogo <- image_read("www/ActionLogo.png") 

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
    navbarPage("Stastic Comparisons:",
               tabPanel("XY Chart",fluidPage(theme = shinytheme("flatly")),
    
    plotOutput("plot", hover = NULL, height = "auto"), #need height to be auto for the function below to work
    hr(),
    #Creates the sidebar for the app
    column(6,
           #allows the user to filter by school or conference
           selectInput(inputId = "Filterchoice",        
                       label = "Filter by Team or Conferences?",
                       choices = list("Team", "Conference"),
                       selected = "Conference"),
           textInput(inputId="Title", label="Enter a Title For your Graph", value = "Title Goes Here", width = NULL, placeholder = NULL),
           textInput(inputId="YAxis", label="Enter a Title For your YAxis", value = "Title Goes Here", width = NULL, placeholder = NULL),
           textInput(inputId="XAxis", label="Enter a Title For your XAxis", value = "Title Goes Here", width = NULL, placeholder = NULL),
           
           textInput(inputId="subTitle", label="Enter a subtitle For your Graph", value = "subTitle Goes Here", width = NULL, placeholder = NULL)
           
    ),
    column(6,
           conditionalPanel(condition = "input.Filterchoice == 'Team'",
                            selectInput(inputId = "School", 
                                        label = "Teams", 
                                        choices = unique(dataset1$Team), 
                                        selected = c("Clemson","Alabama"),
                                        multiple = TRUE)
                            
           ),  
           conditionalPanel(condition = "input.Filterchoice == 'Conference'",
                            selectInput(inputId = "Conference", 
                                        label = "Conferences", 
                                        choices = unique(dataset1$conference), 
                                        selected = unique(dataset1$conference),
                                        multiple = TRUE)
                            
           ),
           #if the first selectinput is school, display this one
    ),
    column(6,
           #create Statistic Input for user
           #create Statistic Input for user
           selectInput(inputId = "Stat",
                       label = "Select Stat for X Axis",
                       choices = names(dataset1)[5:124],
                       selected = "off_ppa"),
           selectInput(inputId = "Stat2",
                       label = "Select Stat for Y Axis",
                       choices = names(dataset1)[5:124],
                       selected = "off_success_rate"),
           #Create Down Input for user
           #Create Down Input for user
    ),
    
    column(6,
           selectInput(inputId = "AverageOnGraph",
                       label = "Average On Plot?",
                       choices = c("Yes", "No"),
                       selected = "Yes"),
           downloadButton("downloadData","Download")
           
    )
    ),

    tabPanel("XY Chart Grey out",fluidPage(theme = shinytheme("flatly")),
             plotOutput("plot2", hover = NULL, height = "auto"), #need height to be auto for the function below to work
             hr(),
             #Creates the sidebar for the app
             column(6,
                    #allows the user to filter by school or conference
                    textInput(inputId="TitleGreyOut", label="Enter a Title For your Graph", value = "Title Goes Here", width = NULL, placeholder = NULL),
                    textInput(inputId="YAxisGreyOut", label="Enter a Title For your YAxis", value = "Title Goes Here", width = NULL, placeholder = NULL),
                    textInput(inputId="XAxisGreyOut", label="Enter a Title For your XAxis", value = "Title Goes Here", width = NULL, placeholder = NULL),
                    textInput(inputId="subTitleGreyOut", label="Enter a subtitle For your Graph", value = "subTitle Goes Here", width = NULL, placeholder = NULL)
                    
             ),
             column(6,
                    selectInput(inputId = "SchoolGreyOut", 
                        label = "Choose Teams to Highlight", 
                        choices = unique(dataset1$Team), 
                        selected = c("Wyoming","Northern Illinos"),
                        multiple = TRUE)
                                     
                    )
                    #if the first selectinput is school, display this one
             ),
             column(6,
                    #create Statistic Input for user
                    #create Statistic Input for user
                    selectInput(inputId = "StatGreyOut",
                                label = "Select Stat for X Axis",
                                choices = names(dataset1)[5:124],
                                selected = "TARP.O"),
                    selectInput(inputId = "Stat2GreyOut",
                                label = "Select Stat for Y Axis",
                                choices = names(dataset1)[5:124],
                                selected = "TARP.D"),
                    #Create Down Input for user
                    #Create Down Input for user
             ),
             
             column(6,
                    selectInput(inputId = "AverageOnGraphGreyout",
                                label = "Average On Plot?",
                                choices = c("Yes", "No"),
                                selected = "Yes"),
                    downloadButton("downloadDataGreyout","Download")
                    
             )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #Filter the dataset based on the input from the school or conference filter!
    
    choice <-reactive({
        if (input$Filterchoice == 'Conference') {
            filter(dataset1,conference %in% input$Conference)
        }
        else{
            filter(dataset1,Team %in% input$School)
        }
    })
    
    #now filter from downs, based on the choice of input (choice is the function that creates teh dataset that downs is filtering on)
    
    
    statPaste <- reactive(paste0(input$Stat))
    ystatPaste <- reactive(paste0(input$Stat2))
    titlePaste <- reactive(paste0(input$Title))
    #get the string version of the down chosen for plot aesthetics
    LogosYesNo<-reactive(paste0(input$Images))
    AverageYesNo<-reactive(paste0(input$AverageOnGraph))
    xAxisPaste <- reactive(paste0(input$XAxis))
    yAxisPaste <- reactive(paste0(input$YAxis))
    subtitlePaste <- reactive(paste0(input$subTitle))
    
    
    #get the plot height inputted by user
    ht<-reactive(input$plotHt)
    
    #create plot
    output$plot <- renderPlot({
        ggplot(choice()) + 
            annotation_custom(rasterGrob(actionLogo, 
                                         width = unit(1,"npc"), 
                                         height = unit(1,"npc")), 
                              -Inf, Inf, -Inf, Inf) +
            geom_image(aes(x=get(input$Stat), y=get(input$Stat2)), image = choice()$logo, asp = 16/9) +
            geom_vline(aes(xintercept=mean(get(input$Stat))), color="green", linetype="dashed", size=1) + 
            geom_hline(aes(yintercept=mean(get(input$Stat2))), color="green", linetype="dashed", size=1) +
            
            labs(y = yAxisPaste(),
                 x = xAxisPaste(),
                 colour = "",
                 caption = "Figure: @Analytics_CFB with Shiny | Data: @CFB_data with #cfbfastr",
                 title = titlePaste(),
                 subtitle = subtitlePaste())+
            #coord_flip() + #flip coordinates, much easier to read
            #Labs adds text
            theme_gray() +
            theme( #set element text sizes and other formatting stuff like legends
                axis.text.x = element_text(size = 10, face = "bold", angle = 45),
                axis.ticks.y =  element_blank(),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold", color = "black"),
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 14, face = "bold"),
                plot.caption = element_text(size = 10, face = "bold"))
        
        #adds a line for visibility
        
    },
    #function that allows the plot height to change dynamically, this is part of the renderplot function()
    height = 800,
    width = 1000
    )
    
    
    choiceGreyOut <-reactive({
        filter(dataset2,Team %in% input$SchoolGreyOut)
    })
    statPasteGreyOut <- reactive(paste0(input$StatGreyOut))
    ystatPasteGreyOut <- reactive(paste0(input$Stat2GreyOut))
    titlePasteGreyOut <- reactive(paste0(input$TitleGreyOut))
    #get the string version of the down chosen for plot aesthetics
    AverageYesNoGreyOut<-reactive(paste0(input$AverageOnGraphGreyOut))
    xAxisPasteGreyOut <- reactive(paste0(input$XAxisGreyOut))
    yAxisPasteGreyOut <- reactive(paste0(input$YAxisGreyOut))
    subtitlePasteGreyOut <- reactive(paste0(input$subTitleGreyOut))
    
    output$plot2 <- renderPlot({
        ggplot(dataset2) + 
            annotation_custom(rasterGrob(actionLogo, 
                                         width = unit(1,"npc"), 
                                         height = unit(1,"npc")), 
                              -Inf, Inf, -Inf, Inf) +
            geom_image(aes(x=get(input$StatGreyOut), y=get(input$Stat2GreyOut)), image = dataset2$logo, color = 'gray25', asp = 16/9) +
            geom_image(data=choiceGreyOut(), aes(x=get(input$StatGreyOut), y=get(input$Stat2GreyOut)), image = choiceGreyOut()$logo, asp = 16/9)+
            geom_vline(aes(xintercept=mean(get(input$StatGreyOut))), color="green", linetype="dashed", size=1) + 
            geom_hline(aes(yintercept=mean(get(input$Stat2GreyOut))), color="green", linetype="dashed", size=1) +
            
            labs(y = yAxisPasteGreyOut(),
                 x = xAxisPasteGreyOut(),
                 colour = "",
                 caption = "Figure: @Analytics_CFB with Shiny | Data: @CFB_data with #cfbfastr",
                 title = titlePasteGreyOut(),
                 subtitle = subtitlePasteGreyOut())+
            #coord_flip() + #flip coordinates, much easier to read
            #Labs adds text
            theme_bw() +
            theme( #set element text sizes and other formatting stuff like legends
                axis.text.x = element_text(size = 10, face = "bold", angle = 45),
                axis.ticks.y =  element_blank(),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold", color = "black"),
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 14, face = "bold"),
                plot.caption = element_text(size = 10, face = "bold"))
        
        #adds a line for visibility
        
    },
    #function that allows the plot height to change dynamically, this is part of the renderplot function()
    height = 800,
    width = 1000
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
