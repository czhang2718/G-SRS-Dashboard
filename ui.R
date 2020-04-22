library(shiny)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(xml2)

# PAGE 1
dashboardPage(
    dashboardHeader(title = "G-SRS Data Explorer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Compare Adverse Events", tabName="compare_ae"),
            menuItem("Compare Substances", tabName = "compare_subs")
        )
    ),
    dashboardBody(
        tags$script(src = "myscript.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
        tabItems(
            tabItem("compare_ae", fluidRow(
                column(width=4, 
                       box(title = "Selectors", status = "warning", solidHeader = TRUE, width=NULL,
                           collapsible = TRUE,
                           selectizeInput("xcol","Adverse Event (x-axis)",vars,selected=vars[1],multiple=FALSE, 
                                          options=list(maxOptions=12000)),
                           selectizeInput("ycol","Adverse Event (y-axis)",vars,selected=vars[2],multiple=FALSE), 
                                          options=list(maxOptions=12000)),
                       box(title="Restrictions", status = "info", side="left", width=NULL,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           sliderInput(inputId="casecount",label="Substance Count",min=100,max=50000,value=1000),
                           sliderInput(inputId="ptcount",label="Adverse Event Count",min=5,max=100,value=10)),
                       box(title="Filter by ATC Classification", status = "danger", side="left", width=NULL,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           soldHeader = FALSE,
                           uiOutput("list1"),
                           uiOutput("list2"),
                           uiOutput("list3"),
                           uiOutput("list4")
                       )),
                
                column(width=8,
                       tabBox(
                           id = "tabset",
                           # height = "450px",
                           width = NULL,
                           tabPanel("Plot", div(
                               style = "position:relative",
                               plotOutput("scatterPlot", 
                                          hover = hoverOpts("plot_hover"),
                                          click = clickOpts(id = "plot_click")),
                               uiOutput("hover_info"),
                               uiOutput("click_info"),
                               textOutput('cor1a'),
                               textOutput('cor1b'),
                               tags$head(tags$style("#cor1a{font-size: 16px; color: grey}")),
                               tags$head(tags$style("#cor1b{font-size: 16px; color: grey}"))
                           )),
                           
                           tabPanel("Data Table", 
                                    div(style="display: inline-block; float: left", checkboxInput("alldata", "Unfiltered", value = FALSE)),
                                    div(style="display: inline-block; float: right", downloadButton("download_xlsx", "XLSX")),
                                    div(style="display: inline-block; float: right", downloadButton("download_txt", "TXT")),
                                    div(style="display: inline-block; float: right", downloadButton("download_csv", "CSV")),
                                    DTOutput("table1")
                           )
                           
                       )
                )
            )
            
            ),
            tabItem("compare_subs", fluidRow(
                column(width=4,
                       box(title = "Selectors", status = "success", solidHeader = TRUE, width=NULL,
                           collapsible = TRUE,
                           selectizeInput("xcol2","Substance x-axis",vars2,selected=vars2[2],multiple=FALSE,
                                          options=list(maxOptions=2500)),
                           selectizeInput("ycol2","Substance y-axis",vars2,selected=vars2[3],multiple=FALSE,
                                          options=list(maxOptions=2500))),
                       box(title="Restrictions", status="info", side="left", width=NULL,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           sliderInput(inputId="ptcount2",label="Adverse Event Count",min=5,max=100,value=10)
                       )
                ),
                column(width=8,
                       tabBox(
                           width=NULL,
                           id = "tabset2",
                           # height = "450px",
                           tabPanel("Plot", div(
                               style = "position:relative",
                               plotOutput("scatterPlot2", hover = hoverOpts("plot_hover2")),
                               uiOutput("hover_coords"),
                               textOutput('cor2'),
                               tags$head(tags$style("#cor2{font-size: 16px; color: grey}"))
                           )),
                           
                           tabPanel("Data Table", 
                                    div(style="display: inline-block; float: right", downloadButton("download_xlsx2", "XLSX")),
                                    div(style="display: inline-block; float: right", downloadButton("download_txt2", "TXT")),
                                    div(style="display: inline-block; float: right", downloadButton("download_csv2", "CSV")),
                                    tags$br(), tags$br(),
                                    DTOutput("table2")
                           )
                       )
                    )
                
                )
            )
        )
    )
)