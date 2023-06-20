#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Version 0.1 Apply upload file function for Demo in SPACE Server 
#Version 0.1b Render Sunburst plot 
#Version 0.2 Include file path input to get pillar information


# Define UI for application
library(pacman)
p_load(haven,dplyr,data.table,stringr,openxlsx,plotly
       ,docxtractr,tidyfst,tokenizers,tidytext,openxlsx)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SPACE Programming Plan Viewer"),
  
  # Sidebar layout with input and output definitions ---- 
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width=2
      
      # Include clarifying text ----
      ,helpText("Note: Please make sure that you have upload the latest Pg Plan CSV file before clicking button below. "
      )
      
      # Input: Select a file ----
      ,fileInput("file1", "Choose Pg Plan .CSV File",
                 multiple = FALSE,
                 accept = c(".csv"
                            ,".CSV")
      )
      
      # Input: actionButton() to Read Pg Plan ----
      ,actionButton("ReadPg", "Read Pg Plan")
      
      # Horizontal line ----
      # ,tags$hr()
      # 
      # ,fileInput("fileFolder", "Choose Preprod folder",
      #            multiple = FALSE
      # )      

      # Horizontal line ----
      ,tags$hr()
      
      # Include clarifying text ----
      ,helpText("Read DPS to Get Mock.Reference is under Developping "
      )
      
      # Input: Select a file ----
      # ,fileInput("file1", "Choose DPS Part1 .Docx File",
      #           multiple = FALSE,
      #           accept = c(".Docx"
      #                      ,".docx")
      # )
      
      # Input: Specify the columns of TOC to filter ----
      # ,numericInput("cols", "Number of TOC Columns:", NA)
      
      # Input: actionButton() to Read Pg Plan ----
      #,actionButton("Read", "Read DPS TOC")
      
      # Horizontal line ----
      ,tags$hr()

      # Select Which kinda Plot to display ----
      ,radioButtons("PlotType", "Display which kinda Plot?",
                      choices = c(Treemap = "Treemap",
                                  Sunburst = "Sunburst"),
                      selected = "Treemap")
      # Horizontal line ----
      ,tags$hr()
      
      # selectInput to select variables for viewing----
      ,selectInput(inputId    = 'PgPlanVar'
                   ,label      = 'Choose Hierarchical Order:'
                   ,choices    = c("")
                   ,multiple   = TRUE
                   ,selectize  = TRUE
                   ,selected   = NULL
      )
      
      # Horizontal line ----
      ,tags$hr()
      
      # Input: actionButton() to Read Pg Plan ----
      ,actionButton("OutputPlot", "View Pg Plan")
      
    )
    
    # Main panel for displaying outputs ----
    ,mainPanel(
      width=10
      # Output: Data file ----
      ,verbatimTextOutput('Order')
      ,verbatimTextOutput('TestOut')
      ,plotlyOutput("Treemap"
                    ,width = "100%"
                    ,height = "800px")
    )
  )
)

# Define server logic required to draw a treemap or sunbrust plot
server <- function(input, output, session) {
  
  ReadLatestPgPlan <- function(){
    dt_Prgm_Plan_Raw <- setDT(read.csv(input$file1$datapath,header=F,sep="\\",encoding ="UTF-8"))
    dt_Prgm_Plan <<- dt_Prgm_Plan_Raw %>% mutate(V1=sub("Yes;No","Yes:No",V1)
                                            ,SemicolonCount=stringr::str_count(V1,";")-8)
    dt_Prgm_Plan <<- dt_Prgm_Plan %>% 
      mutate(`Programmer`= str_split_i(dt_Prgm_Plan$V1, ";", -4)
             ,`QCer`     = str_split_i(dt_Prgm_Plan$V1, ";", -3)
             ,`Table_ID` = stringr::str_replace_all(toupper(stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,1])
                                                  ,".RTF|.TXT"
                                                  ,""
                                                  )
             ,Type       = str_split_i(dt_Prgm_Plan$V1, ";", -7)
             ,Risk       = str_split_i(dt_Prgm_Plan$V1, ";", -6)
             ,Code       = str_split_i(dt_Prgm_Plan$V1, ";", -5)
             ,Title      = case_when(SemicolonCount == 0 ~ stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,2]
                                    ,SemicolonCount == 1 ~ paste(stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,2]
                                                                ,stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,3]
                                                                ,sep=";"
                                                                )
                                    ,SemicolonCount == 2 ~ paste(stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,2]
                                                                ,stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,3]
                                                                ,stringr::str_split(dt_Prgm_Plan$V1,";",simplify = T)[,4]
                                                                ,sep=";"
                                                                )
                                    ,T ~ "Too Much Semicolon in Title!")
             ,OutputTime = str_split_i(dt_Prgm_Plan$V1, ";", -2)
             ,Status = str_split_i(dt_Prgm_Plan$V1, ";", -1)
      ) %>% mutate(Table_Type=case_when(stringr::str_trunc(Table_ID,1,"right",ellipsis = "") == "T" ~ "Table"
                                        ,stringr::str_trunc(Table_ID,1,"right",ellipsis = "") == "G" ~ "Graph"
                                        ,stringr::str_trunc(Table_ID,1,"right",ellipsis = "") == "L" ~ "Listing"
                                        ,T ~ "Other"
                                        )
      ) %>% filter(Table_ID != "PLANNED ITEMS"
      ) %>% select(-c(V1,SemicolonCount))
  }
  
  # DPS Input ---------------------------------------------------------------
  # Reading .docx DPS files,
  observe({
    tryCatch(
      {
        ReadLatestPgPlan()
        updateSelectInput(inputId = "PgPlanVar"
                          ,label = 'Ready to Choose Hierarchical Order:'
                          ,choices = c(names(dt_Prgm_Plan))
                          ,selected = NULL
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  }) %>% bindEvent(input$ReadPg)
  
  output$Order <- reactive(VecStr <<- c(input$PgPlanVar))
  # output$TestOut <- reactive(VecStr <<- c(input$fileFolder$datapath))
  
  GetHierarchicalData4Plot <- function(){ 
    print(length(VecStr))
    if (length(VecStr)>=2){
      Key_Lv1 <<- dt_Prgm_Plan %>% group_by(.data[[VecStr[1]]]) %>% 
        summarise(COUNT = n()) %>%  
        mutate(ID=.data[[VecStr[1]]]
               ,Label=paste(casefold(.data[[VecStr[1]]]),COUNT,sep="<br> Count:")
               ,Parent="") %>% ungroup %>%
        select(-c(.data[[VecStr[1]]]
        ))
      
      Key_Lv2 <<- dt_Prgm_Plan %>% group_by(.data[[VecStr[1]]]
                                            ,.data[[VecStr[2]]]
      ) %>% 
        summarise(COUNT = n()) %>% 
        mutate(ID=paste(.data[[VecStr[1]]]
                        ,.data[[VecStr[2]]]
                        )
        ,Label=paste(casefold(.data[[VecStr[2]]]),COUNT,sep="<br> Count:")
        ,Parent=.data[[VecStr[1]]]
        ) %>% ungroup %>% 
        select(-c(.data[[VecStr[1]]]
                  ,.data[[VecStr[2]]]
        )
        ) 
    }
    
    if (length(VecStr)>=3){
      Key_Lv3 <<- dt_Prgm_Plan %>% group_by(.data[[VecStr[1]]]
                                           ,.data[[VecStr[2]]]
                                           ,.data[[VecStr[3]]]
      ) %>% 
        summarise(COUNT = n()) %>% 
        mutate(ID=paste(.data[[VecStr[1]]]
                        ,.data[[VecStr[2]]]
                        ,.data[[VecStr[3]]]
                        )
        ,Label =paste(.data[[VecStr[3]]],COUNT,sep="<br> Count:")
        ,Parent=paste(.data[[VecStr[1]]]
                      ,.data[[VecStr[2]]]
        )
        ) %>% ungroup %>% 
        select(-c(.data[[VecStr[1]]]
                  ,.data[[VecStr[2]]]
                  ,.data[[VecStr[3]]]
                )
        )
    }
    
    if (length(VecStr)>=4){
      Key_Lv4 <<- dt_Prgm_Plan %>% group_by(.data[[VecStr[1]]]
                                           ,.data[[VecStr[2]]]
                                           ,.data[[VecStr[3]]]
                                           ,.data[[VecStr[4]]]
 
      ) %>% 
        summarise(COUNT = n()) %>% 
        mutate(ID=paste(.data[[VecStr[1]]]
                        ,.data[[VecStr[2]]]
                        ,.data[[VecStr[3]]]
                        ,.data[[VecStr[4]]]
        )
        ,Label =paste(.data[[VecStr[4]]],COUNT,sep="<br> Count:")
        ,Parent=paste(.data[[VecStr[1]]]
                      ,.data[[VecStr[2]]]
                      ,.data[[VecStr[3]]]
        )
        ) %>% ungroup %>% 
        select(-c(.data[[VecStr[1]]]
                  ,.data[[VecStr[2]]]
                  ,.data[[VecStr[3]]]
                  ,.data[[VecStr[4]]]
                 )
              )
    }
    
    if (length(VecStr)>=5){
      Key_Lv5 <<- dt_Prgm_Plan %>% group_by(.data[[VecStr[1]]]
                                           ,.data[[VecStr[2]]]
                                           ,.data[[VecStr[3]]]
                                           ,.data[[VecStr[4]]]
                                           ,.data[[VecStr[5]]]
      ) %>% 
        summarise(COUNT = n()) %>% 
        mutate(ID=paste(.data[[VecStr[1]]]
                        ,.data[[VecStr[2]]]
                        ,.data[[VecStr[3]]]
                        ,.data[[VecStr[4]]]
                        ,.data[[VecStr[5]]]
        )
        ,Label =paste(.data[[VecStr[5]]],COUNT,sep="<br> Count:")
        ,Parent=paste(.data[[VecStr[1]]]
                      ,.data[[VecStr[2]]]
                      ,.data[[VecStr[3]]]
                      ,.data[[VecStr[4]]]
        )
        ) %>% ungroup %>% 
        select(-c(.data[[VecStr[1]]]
                  ,.data[[VecStr[2]]]
                  ,.data[[VecStr[3]]]
                  ,.data[[VecStr[4]]]
                  ,.data[[VecStr[5]]]
        )
        )
    }
    
    if (length(VecStr)>=6){
      Key_Lv6 <<- dt_Prgm_Plan %>% group_by(.data[[VecStr[1]]]
                                                   ,.data[[VecStr[2]]]
                                                   ,.data[[VecStr[3]]]
                                                   ,.data[[VecStr[4]]]
                                                   ,.data[[VecStr[5]]]
                                                   ,.data[[VecStr[6]]]
      ) %>%
        summarise(COUNT = n()) %>%
        mutate(ID=paste(.data[[VecStr[1]]]
                        ,.data[[VecStr[2]]]
                        ,.data[[VecStr[3]]]
                        ,.data[[VecStr[4]]]
                        ,.data[[VecStr[5]]]
                        ,.data[[VecStr[6]]]
        )
        ,Label =.data[[VecStr[6]]]
        ,Parent=paste(.data[[VecStr[1]]]
                      ,.data[[VecStr[2]]]
                      ,.data[[VecStr[3]]]
                      ,.data[[VecStr[4]]]
                      ,.data[[VecStr[5]]]
        )
        ) %>% ungroup %>% 
        select(-c(.data[[VecStr[1]]]
                  ,.data[[VecStr[2]]]
                  ,.data[[VecStr[3]]]
                  ,.data[[VecStr[4]]]
                  ,.data[[VecStr[5]]]
                  ,.data[[VecStr[6]]]
                 )
             )
    }
  }
  
  RenderTreemap <- function(){
    Key_All <<- data.frame(matrix(NA, ncol=0, nrow = 0))
    for (i in seq(1:length(VecStr))){
      print(i)
      Key_All <<- rbind(Key_All,eval(parse(text =paste0("Key_Lv",i))))
    }
    fig <<- plot_ly(Key_All
                    ,type="treemap"
                    ,ids = ~ID
                    ,labels=~Label
                    ,parents=~Parent
                    ,values=~COUNT
                    ,maxdepth = 2
                    # ,marker=list(colorscale='Reds')
    ) %>%
      layout(uniformtext=list(minsize=30))
  }

  RenderSunburst <- function(){
    Key_All <<- data.frame(matrix(NA, ncol=0, nrow = 0))
    for (i in seq(1:length(VecStr))){
      print(i)
      Key_All <<- rbind(Key_All,eval(parse(text =paste0("Key_Lv",i))))
    }
    fig <<- plot_ly(Key_All
                    ,type="sunburst"
                    ,ids = ~ID
                    ,labels=~Label
                    ,parents=~Parent
                    # ,values=~COUNT
                    ,maxdepth = 2
                    ,insidetextorientation='horizontal'
    ) %>%
      layout(uniformtext=list(minsize=45))
  }
    
  RenderPlot <- reactive({
    tryCatch(
      { 
        GetHierarchicalData4Plot()
        if(input$PlotType == "Treemap") {
          return(RenderTreemap())
        }
        else {
          return(RenderSunburst())
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  }) %>% bindEvent(input$OutputPlot)
  
  output$Treemap <- renderPlotly(RenderPlot())
}

# Run the application 
shinyApp(ui = ui, server = server)
