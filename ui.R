library(shiny)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(xml2)
library(plotly)
library(shinyBS)
library(shinycssloaders)
library(formattable)
library(tinytex) # <- formattable dependency
library(dplyr)

# PAGE 1
dashboardPage(
    dashboardHeader(title = "G-SRS Data Explorer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName="intro"),
            menuItem("Compare Adverse Events", tabName="compare_ae"),
            menuItem("Compare Substances", tabName = "compare_subs"),
            menuItem("Class Comparison", tabName="class_comp")
        )
    ),
    dashboardBody(
        tags$script(src = "myscript.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tabItems(
            tabItem("intro",
                    h1("G-SRS Data Explorer", align="center"),
                    fluidRow(
                        column(width = 12,
                               selectInput("intro_drug", "Select Drug", vars2, multiple=FALSE, width="30%", selected=vars2[5])
                        )  
                    ),
                    fluidRow(
                        
                        column(width = 6,
                               fluidRow(
                                   column(width=12,
                                          tags$div(id="pie-div", box(id= "intro-pie", width = NULL, status="warning",
                                                                     solidHeader = TRUE, plotlyOutput("pie_chart"))
                                          )),
                                   column(width=12, tags$div(id="summary", box(id="table-box", title="Summary Statistics", width=NULL, status="primary",
                                                                               solidHeader=T, dataTableOutput("sum_table"))))
                                   
                               )
                        ),
                        
                        column(width = 6,
                               box(id= "intro-box", title = "Adverse Events", width = NULL, 
                                   
                                   div(style="display: inline-block;", selectInput("sort_by", c("Number of Adverse Events", "PRR"), 
                                                                                   label = "Sort by", selected="Number of Adverse Events", multiple=FALSE, width = "210px")),
                                   actionButton("popdt", "", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"),
                                   div(style="display: inline-block; float: right", downloadButton("bar1_xlsx", "XLSX")),
                                   div(style="display: inline-block; float: right", downloadButton("bar1_txt", "TXT")),
                                   div(style="display: inline-block; float: right", downloadButton("bar1_csv", "CSV")),
                                   div(id="aebar", style="overflow-y: scroll; position: relative", plotlyOutput("top_ae"))
                               )
                        )
                        
                    )
                    
            ),
            
            tabItem("compare_ae", 
                    fluidRow(
                        column(width=4, 
                               box(id = "box1a", status = "warning", width=NULL,
                                   collapsible = TRUE,
                                   selectizeInput("xcol","Adverse Event (x-axis)",vars,selected=vars[4],multiple=FALSE, 
                                                  options=list(maxOptions=12000)),
                                   selectizeInput("ycol","Adverse Event (y-axis)",vars,selected=vars[5930],multiple=FALSE), 
                                   options=list(maxOptions=12000)),
                               box(id = "box1b", title="Change Parameters", status = "info", side="left", width=NULL,
                                   collapsible = TRUE,
                                   collapsed = TRUE,
                                   sliderInput(inputId="casecount",label="Substance Count",min=100,max=50000,value=1000),
                                   sliderInput(inputId="ptcount",label="Adverse Event Count",min=5,max=100,value=10)),
                               box(id = "box1c", title="ATC Classes", status = "danger", side="left", width=NULL,
                                   collapsible = TRUE,
                                   collapsed = TRUE,
                                   soldHeader = FALSE,
                                   uiOutput("list1"),
                                   uiOutput("list2"),
                                   uiOutput("list3"),
                                   uiOutput("list4")
                               ),
                               collapseInput(boxId = "box1a"),
                               collapseInput(boxId = "box1b"),
                               collapseInput(boxId = "box1c")),
                        
                        column(width=8,
                               tabBox(
                                   title = "Correlate two adverse events",
                                   id = "tabset",
                                   # height = "450px",
                                   width = NULL,
                                   tabPanel("Single Selection", div(
                                       style = "position:relative",
                                       plotOutput("scatterPlot", 
                                                  hover = hoverOpts("plot_hover"),
                                                  click = clickOpts(id = "plot_click")),
                                       uiOutput("hover_info"),
                                       uiOutput("click_info"),
                                       textOutput('cor1a'),
                                       textOutput('cor1b'),
                                       tags$head(tags$style("#cor1a{font-size: 16px; color: grey}")),
                                       tags$head(tags$style("#cor1b{font-size: 16px; color: grey}")))
                                   ),
                                   
                                   tabPanel("Data Table", 
                                            div(style="display: inline-block; float: left", checkboxInput("alldata", "Unfiltered", value = FALSE)),
                                            div(style="display: inline-block; float: right", downloadButton("download_xlsx", "XLSX")),
                                            div(style="display: inline-block; float: right", downloadButton("download_txt", "TXT")),
                                            div(style="display: inline-block; float: right", downloadButton("download_csv", "CSV")),
                                            DTOutput("table1")
                                   ),
                                   tabPanel("Multiple Selection",
                                            width = "100%",
                                            # height = "100%",
                                            selectizeInput("ae1", "Adverse Event", vars, width = "30%",  selected = vars[4], options = list(maxOptions=12000)),
                                            selectizeInput("other_ae", "Compare With", vars, width = "100%", options = list(maxOptions=12000), multiple = TRUE),
                                            span("Number of Observations \u2265"),
                                            div(style="display: inline-block;", numericInput("num_obs", label = NULL, value = 5, width = "55px", min=1, max=1000)),
                                            actionLink("classes", "Filter"),    
                                            bsModal("filter", "Filter Observations", trigger = "classes", size = "large", splitLayout(cellArgs = list(style = "padding-left: 60px"),
                                                                                                                                      tags$div(
                                                                                                                                          h5("Restrictions on Substance Observations"),
                                                                                                                                          sliderInput(inputId="casecount_box1",label="Minimum Case Count",min=100,max=50000,value=1000),
                                                                                                                                          tags$br(),
                                                                                                                                          sliderInput(inputId="ptcount_box1",label="Minimum Adverse Event Count",min=5,max=100,value=10)
                                                                                                                                      ),
                                                                                                                                      tags$div(
                                                                                                                                          h5("Filter Substances by ATC Classification"),
                                                                                                                                          uiOutput("class1"),
                                                                                                                                          uiOutput("class2"),
                                                                                                                                          uiOutput("class3"),
                                                                                                                                          uiOutput("class4"),
                                                                                                                                      )
                                            ),
                                            tags$div(align="center", actionButton("filt", "Filter"), actionButton("reset", "Reset")),
                                            tags$head(tags$style("#reset .modal-footer{ display:none; margin: auto}")),
                                            tags$head(tags$style("#filter .modal-footer{ display:none; margin: auto}")),
                                            tags$head(tags$style("#filt {display:inline-block; padding:0.3em 1.2em; margin:0 0.1em 0.1em 0; border:0.16em solid rgba(255,255,255,0);  
                                                box-sizing: border-box; text-decoration:none; font-family:'Roboto',sans-serif; font-weight:300; color:#FFFFFF; 
                                                text-shadow: 0 0.04em 0.04em rgba(0,0,0,0.35); text-align:center; background-color:#55c24f}")),
                                            tags$head(tags$style("#reset {display:inline-block; padding:0.3em 1.2em; margin:0 0.1em 0.1em 0; border:0.16em solid rgba(255,255,255,0); 
                                                box-sizing: border-box; text-decoration:none; font-family:'Roboto',sans-serif; font-weight:300; color:#FFFFFF; 
                                                text-shadow: 0 0.04em 0.04em rgba(0,0,0,0.35); text-align:center; background-color:#bdbdbd}")),
                                            tags$head(tags$style("#reset:hover { background-color: #8f8f8f}")),
                                            tags$head(tags$style("#filt:hover { background-color:#45a340 }"))
                                            ),
                                            withSpinner(plotlyOutput("single_ae1"))
                                   )
                                   
                               )
                        )
                    )
                    
            ),
            tabItem("compare_subs",
                    fluidRow(
                        column(width=4,
                               box(status = "success", width=NULL,
                                   collapsible = TRUE,
                                   selectizeInput("xcol2","Substance x-axis",vars2,selected=vars2[11],multiple=FALSE,
                                                  options=list(maxOptions=2500)),
                                   selectizeInput("ycol2","Substance y-axis",vars2,selected=vars2[14],multiple=FALSE,
                                                  options=list(maxOptions=2500))),
                               box(title="Change Parameters", status="info", side="left", width=NULL,
                                   collapsible = TRUE,
                                   collapsed = TRUE,
                                   sliderInput(inputId="ptcount2",label="Adverse Event Count",min=5,max=100,value=10)
                               )
                        ),
                        column(width=8,
                               tabBox(
                                   title = "Correlate two substances",
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
                                   ),
                                   tabPanel(title = "Multiple Selection", width = "100%",
                                            selectizeInput("sub1", "Substance", vars2, width = "30%",  selected = vars2[4], options = list(maxOptions=2500)),
                                            # other_ae --> sub2
                                            selectizeInput("sub2", "Compare With:", vars2, width = "100%", options = list(maxOptions=2500), multiple = TRUE),
                                            span("Showing correlations for adverse events with >="),
                                            div(style="display: inline-block;", numericInput("num_subs", label = NULL, value = 5, width = "55px", min=2, max=1000)),
                                            span("correlated substances. "),
                                            "Minimum adverse event count: ",
                                            div(style="display: inline-block;", numericInput("min_ae", label = NULL, value = 10, width = "60px", min=5, max=100)),
                                            withSpinner(plotlyOutput("subs_bar"))
                                   )
                               )
                        )
                        
                    )
            ),
            tabItem("class_comp",
                    fluidRow(
                        column(width=12, align="center", 
                               div(style="line-height: 50%", div(style="display: inline-block; vertical-align:top; width:90px", 
                                                                 selectInput("cc_type", "Type", c("Drug", "Class"), multiple=FALSE, selected="Drug")),
                               div(style="display: inline-block; vertical-align:top; width:30%", uiOutput("cc_2")), 
                               p(" vs.", style="font-size: 18px; font-weight: bold; display: inline-block; vertical-align: -500%"),
                               div(style="display: inline-block; vertical-align:top; width:65px", 
                                   selectInput("cc_level", "Level", c("1", "2", "3", "4"), multiple=FALSE, selected="1")),
                               div(style="display: inline-block; vertical-align:top; width:30%", uiOutput("cc_1")))
                        )
                    ),
                    fluidRow(
                        column(width=12,
                               box(width=NULL, div(style="width:30%", uiOutput("sortby4")), div(style="overflow-x: scroll; position: relative", withSpinner(plotlyOutput("bar4"))), status="warning"),
                               box(width=NULL, withSpinner(plotlyOutput("cor4")), textOutput('c4'), status="danger"))
                    )
            )
            
        )
    )
)
