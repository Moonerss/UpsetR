#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##### Loading required packages #####
library(shiny)
library(shinydashboard)
library(UpSetR)
library(DT)
library(ggplot2)

##### Set globle options #####
options(shiny.maxRequestSize=100*1024^3) #100G
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
rm(list=ls())

## example data
input_data <- read.table("data/moive.txt", header = T,sep = "\t")
# 不能用内置数据集
# input_data <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"))


############# dashboard Header ##################

## Messages menu #####################
messages <- dropdownMenu(type = "messages",
                         messageItem(
                             from = "Sales Dept",
                             message = "Sales are steady this month."),
                         messageItem(
                             from = "New User",
                             message = "How do I register?",
                             icon = icon("question"),
                             time = "13:45"),
                         messageItem(
                             from = "Support",
                             message = "The new server is ready.",
                             icon = icon("life-ring"),
                             time = "2014-12-01")
                         )

## Notifications
notifications <- dropdownMenu(type = "notifications",
                              notificationItem(
                                  text = "5 new users today",
                                  icon("users")),
                              notificationItem(
                                  text = "12 items delivered",
                                  icon("truck"),
                                  status = "success"),
                              notificationItem(
                                  text = "Server load at 86%",
                                  icon = icon("exclamation-triangle"),
                                  status = "warning")
                              )

## Tasks
tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
                      taskItem(value = 90, color = "green", "Documentation"),
                      taskItem(value = 17, color = "aqua", "Project X"),
                      taskItem(value = 75, color = "yellow", "Server deployment"),
                      taskItem(value = 80, color = "red", "Overall project")
                      )

## dashboard Header
header <- dashboardHeader(
    title = "Shiny UpSetR",
    titleWidth = 300,
    disable = F,
    messages,
    notifications,
    tasks)

############# sidebar #######################

## file input
files <- fileInput(
    inputId = 'infile',
    label = h5('Choose Multiple Set File'),
    multiple = F,
    accept=c('.txt', 'txt'),
    buttonLabel = "Browse",
    placeholder = "No file selected"
)

## select set
nsets <- sliderInput(
    inputId = "nSets",
    label = h5("Number of sets to look at"),
    min = 2,
    max = 10,
    value = 5,
    step = 1,
    round = T,
    ticks = T,
    animate = T,
    width = NULL
)

## sets order
orders <- radioButtons(
    inputId = "keepOrder",
    label = h5("Keep order"),
    choices = c("TRUE","FALSE"),
    selected = "FALSE"
)

## ordered by
order_by <-   selectInput(
    inputId = "orderBy",
    label = h5("Order by:"),
    choices = c("freq","degree"),
    multiple = F,
    selected = "freq"
)

## group by
group_by <- selectInput(
    inputId = "groupBy",
    label = h5("Group by:"),
    choices = c("degree","sets"),
    multiple = F,
    selected = "degree"
)

## y label name
y_label <-   textInput(
    inputId = "yLabel",
    label = h5("Y-axis Label:"),
    value = "Genre Intersections",
    width = NULL,
    placeholder = "Please input y-axis title"
)

## x label name
x_label <-   textInput(
    inputId = "xLabel",
    label = h5("X-axis Label:"),
    value = "Movies Per Genre",
    width = NULL,
    placeholder = "Please input x-axis title"
)

## select data set
# dat_select <- selectInput(
#     inputId = "sets",
#     label = h5('Selected Sets:'),
#     choices = c("a", "b"),#names(input_data),
#     multiple = TRUE
# )

## Sidebar content ##################
sidebar <- dashboardSidebar(
    ## width
    width = 300,
    collapsed = F,
    files,
    nsets,
    orders,
    order_by,
    group_by,
    y_label,
    x_label,
#    dat_select,
    tags$div(
        actionButton(
            inputId = "submitButton",
            label = "Submit",
            icon= icon("arrow-up"),
            style="color:#fff;background-color:#337ab7;border-color:#2e6da4"),
        align="center"),
    tags$p(
        helpText("Click the button to update the value displayed in the plot."),
        align="center")
)

############# body content #######################
fig <- box(
    title = "Figure",
#    footer = "test",
    status = "success",
    solidHeader = TRUE,
    width = 10,
    height = 850,
    collapsible = TRUE,
    collapsed = FALSE,
    tabsetPanel(
        tabPanel(
            title = "Plot",
            fluidRow(
                column(width = 12,
                       plotOutput(
                           outputId = "Plot",
                           height = 720),
                       tags$p(
                           downloadButton(
                               outputId = "download1",
                               label = "Download"),
                           align="right")
                       )
            )

        ),
        tabPanel(
            title = "DataSet",
            fluidRow(
                column(width = 12,
                       DT::dataTableOutput('dataSet'),
                       hr(),
                       tags$p(
                           downloadButton(
                               outputId = "download2",
                               label = "Download"),
                           align = "right"
                       ))
                )
            )
        )
)

############# right select bar #######################
right_bar <- box(
    title = "Parameter panel",
    solidHeader = TRUE,
#    footer = "Thank you",
    status = "primary",
    width = 2,
    height = 850,
    collapsible = TRUE,
    collapsed = FALSE,
    sliderInput(inputId = "point",
                label = h4("Point Size:"),
                min = 0,
                max = 5,
                value = 3,
                step = 0.5),
    sliderInput(inputId = "line",
                label = h4("Line Size:"),
                min = 0,
                max = 5,
                value = 1,
                step = 0.5),
    sliderInput(inputId = "font",
                label = h4("Font Size:"),
                min = 0,
                max = 5,
                value = 1.5,
                step = 0.5),
    selectInput(
        inputId = "color1",
        label = h4("Main bar color:"),
        choices = c(
            "Red" = "red","Blue" = "blue","Green" = "green",
            "Pink" = "pink","Purple" = "purple","Yellow" = "yellow",
            "Cyan" = "cyan","Maroon" = "maroon","Orange" = "orange",
            "Black" = "black","Magenta" = "magenta","Orchid" = "orchid",
            "Brown" = "brown","Grey" = "grey","Salmon"="salmon"),
        selected = "black"),
    selectInput(
        inputId = "color2",
        label = h4("Sets bar color:"),
        choices = c(
            "Red" = "red","Blue" = "blue","Green" = "green",
            "Pink" = "pink","Purple" = "purple","Yellow" = "yellow",
            "Cyan" = "cyan","Maroon" = "maroon","Orange" = "orange",
            "Black" = "black","Magenta" = "magenta","Orchid" = "orchid",
            "Brown" = "brown","Grey" = "grey","Salmon"="salmon"),
        selected = "red"),
    selectInput(
        inputId = "color3",
        label = h4("Matrix color:"),
        choices = c(
            "Red" = "red","Blue" = "blue","Green" = "green",
            "Pink" = "pink","Purple" = "purple","Yellow" = "yellow",
            "Cyan" = "cyan","Maroon" = "maroon","Orange" = "orange",
            "Black" = "black","Magenta" = "magenta","Orchid" = "orchid",
            "Brown" = "brown","Grey" = "grey","Salmon"="salmon"),
        selected = "blue"),
    selectInput(
        inputId = "color4",
        label = h4("Matrix shading color:"),
        choices = c(
            "Red" = "red","Blue" = "blue","Green" = "green",
            "Pink" = "pink","Purple" = "purple","Yellow" = "yellow",
            "Cyan" = "cyan","Maroon" = "maroon","Orange" = "orange",
            "Black" = "black","Magenta" = "magenta","Orchid" = "orchid",
            "Brown" = "brown","Grey" = "grey","Salmon"="salmon"),
        selected = "purple"))

################ body content ###############################
body <- dashboardBody(
    fluidRow(fig, right_bar)
)

################ The UI and input ###########################

ui <- dashboardPage(
    skin = "green",
    header,
    sidebar,
    body
)

server <- function(input, output) {
#    setsSelected <- eventReactive(
#        input$submitButton, {
#            input$sets
#        })
    ## choose the input data
    dataContent <- reactive({
        infile <- input$infile
        if (is.null(infile)) {
            input_data <- input_data
        } else {
            input_data <- read.table(inFile$datapath,header = T,sep = "\t",check.names = F)
        }
        input_data
    })

    ## plot the picture
    output$Plot <- renderPlot({
        if (is.null(input$sets)) {
            p <- upset(data = input_data, nsets = input$nSets, number.angles = 0,
                       keep.order = input$keepOrder, point.size = input$point,
                       line.size = input$line, mainbar.y.label = input$yLabel,
                       sets.x.label = input$xLabel, mb.ratio = c(0.6, 0.4),
                       main.bar.color = input$color1, sets.bar.color = input$color2,
                       matrix.color = input$color3, shade.color = input$color4,
                       text.scale = input$font, order.by = input$orderBy,
                       group.by = input$groupBy)
        } else {
            p <- upset(data = input_data, nsets = input$nSets,
                       # sets = setsSelected,
                       number.angles = 0, keep.order = input$keepOrder,
                       point.size = input$point, line.size = input$line,
                       mainbar.y.label = input$yLabel, sets.x.label = input$xLabel,
                       mb.ratio = c(0.6, 0.4), main.bar.color = input$color1,
                       sets.bar.color = input$color2, matrix.color = input$color3,
                       shade.color = input$color4, text.scale = input$font,
                       order.by = input$orderBy, group.by = input$groupBy)
        }
        return(p)
    })

    # download the picture
    # Download the Basic Plot
     output$download1 <- downloadHandler(
         filename = function(){
             paste0("UpSetPlot-", Sys.Date(), ".pdf")
         },
         contentType = "image/pdf",
         content = function(file){
             pdf(file, onefile = F,width = 16,height = 8)
             if (is.null(input$sets)){
                 p <- upset(data = input_data, nsets = input$nSets, number.angles = 0,
                            keep.order = input$keepOrder, point.size = input$point,
                            line.size = input$line, mainbar.y.label = input$yLabel,
                            sets.x.label = input$xLabel, mb.ratio = c(0.6, 0.4),
                            main.bar.color = input$color1, sets.bar.color = input$color2,
                            matrix.color = input$color3, shade.color = input$color4,
                            text.scale = input$font, order.by = input$orderBy,
                            group.by = input$groupBy)
             }else{
                 p <- upset(data = input_data, nsets = input$nSets,
                            # sets = setsSelected,
                            number.angles = 0, keep.order = input$keepOrder,
                            point.size = input$point, line.size = input$line,
                            mainbar.y.label = input$yLabel, sets.x.label = input$xLabel,
                            mb.ratio = c(0.6, 0.4), main.bar.color = input$color1,
                            sets.bar.color = input$color2, matrix.color = input$color3,
                            shade.color = input$color4, text.scale = input$font,
                            order.by = input$orderBy, group.by = input$groupBy)
             }
             print(p)
             dev.off()
     })

    # View ad download the DateSets
    output$dataSet <- DT::renderDataTable({
        dataContent()
    },
    options = list(
        pageLength = 15,
        lengthMenu = c("10","15","25","50","100"),
        initComplete = I("function(settings, json) {alert('Done.');}")
    )
    )
    # download the view data
    output$download2 <- downloadHandler(
        filename = function(){
            paste0("UpSetDataSet-", Sys.Date(), ".csv")
        },
        content = function(file){
            write.csv(dataContent(), file, row.names = FALSE)
        }
    )
}



shinyApp(ui = ui, server = server)

