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
library(shinyjs)

dropdownBtn <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
    
    status <- match.arg(status)
    # dropdown button content
    html_ul <- list(
        class = "dropdown-menu",
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"),
        lapply(X = list(...), FUN = tags$li, style = "white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;margin-left: 10px; margin-right: 10px; height: 400px; overflow-y: scroll")
    )
    # dropdown button apparence
    html_button <- list(
        class = paste0("btn btn-", status," dropdown-toggle"),
        type = "button", 
        `data-toggle` = "dropdown"
    )
    html_button <- c(html_button, list(label))
    html_button <- c(html_button, list(tags$span(class = "caret")))
    # final result
    tags$div(
        class = "dropdown",
        do.call(tags$button, html_button),
        do.call(tags$ul, html_ul),
        tags$script(
            "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
    )
}

tagList(
    useShinyjs(),
    tags$script(src = "myscript.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
    div(id="start-page", img(id="bg", src="img3.jpg"),
        h1(id="loading_text", "G-SRS Data Explorer"),
        HTML("<button id='startbutton'>Start</button>")),
    hidden(div(id="main-content", dashboardPage(
        dashboardHeader(title = "G-SRS Dashboard"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName="intro"),
                menuItem("Compare Adverse Events", tabName="compare_ae"),
                menuItem("Compare Substances", tabName = "compare_subs"),
                menuItem("Class Comparison", tabName="class_comp")
            )
        ),
        
        dashboardBody(
            tabItems(
                tabItem("intro",
                        fluidRow(
                            column(width = 12,
                                   box(width=9, solidHeader=TRUE,
                                       selectInput("intro_drug", "Select Substance", vars2, multiple=FALSE, selected=vars2[11])),
                                   
                                   box(width=3, solidHeader=TRUE,
                                       
                                       div(align="center", 
                                           selectInput("downloadType", "Download As", c(".csv", ".txt", ".xlsx", ".json"), selected=".csv", width="60%"),
                                           downloadButton("dload", "Download")),
                                   )
                            )
                        ),
                        
                        fluidRow(
                            column(width = 6,
                                   box(id= "intro-pie", width = NULL, status="warning",
                                       solidHeader = TRUE, plotlyOutput("pie_chart")),
                                   box(id="table-box", title="Summary Statistics", width=NULL, status="primary",
                                       solidHeader=T, dataTableOutput("sum_table"))
                                   
                            ),
                            
                            column(width = 6,
                                   box(id= "intro-box", title = "Adverse Events", width = NULL, 
                                       
                                       div(style="display: inline-block;", selectInput("sort_by", c("Number of Adverse Events", "PRR"), 
                                                                                       label = "Sort by", selected="Number of Adverse Events", multiple=FALSE, width = "210px")),
                                       actionButton("popdt", "", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                       span(style="float:right;  padding-right: 20px; visibility:hidden", "."),
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
                                   box(id = "box1b", title="Parameters", status = "info", side="left", width=NULL,
                                       collapsible = TRUE,
                                       collapsed = TRUE,
                                       tags$style(HTML("
                                            input:invalid {
                                            background-color: #FFCCCC;
                                            }")),
                                       uiOutput("casecount"),
                                       uiOutput("ptcount"),
                                       actionButton("reset_ae", "Reset")),
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
                                       id = "tabset",
                                       # height = "450px",
                                       width = NULL,
                                       tabPanel("Two AE", div(
                                           actionButton("dt_open1", "Data Table"),
                                           style = "position:relative",
                                           plotOutput("scatterPlot", 
                                                      hover = hoverOpts("plot_hover"),
                                                      click = clickOpts(id = "plot_click")),
                                           uiOutput("hover_info"),
                                           uiOutput("click_info"),
                                           textOutput('cor1a'),
                                           textOutput('cor1b'),
                                           tags$head(tags$style("#cor1a{font-size: 16px; color: grey}")),
                                           bsTooltip("cor1a", "Pearson&#39s Correlation Coefficient", placement="left"),
                                           tags$head(tags$style("#cor1b{font-size: 16px; color: grey}")))
                                       ),
                                       
                                       bsModal("ae_dt", "Data Table", trigger="dt_open1", size="large",
                                                div(style="float: left", checkboxInput("alldata", "Unfiltered", value = FALSE)),
                                                div(style="float:right", downloadButton("download_2", "Download")),
                                                div(style="float:right", selectInput("downloadType2", label=NULL, choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
                                                
                                                DTOutput("table1")
                                       ),
                                       tabPanel("Multiple AE",
                                                width = "100%",
                                                selectizeInput("ae1", "Adverse Event", vars, width = "30%",  selected = vars[4], options = list(maxOptions=12000)),
                                                dropdownBtn(
                                                    label = "Compare With", status = "default", width = "100%",
                                                    checkboxGroupInput(inputId = "other_ae", label = "Choose", choices = vars)
                                                ),
                                                
                                                span("Number of Substances \u2265"),
                                                div(style="display: inline-block;", numericInput("num_obs", label = NULL, value = 5, width = "55px", min=1, max=1000)),
                                                actionLink("classes", "Filter"),    
                                                bsModal("filter", "Filter Observations", trigger = "classes", size = "large", splitLayout(cellArgs = list(style = "padding-left: 60px"),
                                                                                                                                          tags$div(
                                                                                                                                              h5("Restrictions on Substance Observations"),
                                                                                                                                              numericInput(inputId="casecount_box1",label="Minimum Case Count",min=100,max=50000,value=1000, step=1),
                                                                                                                                              numericInput(inputId="ptcount_box1",label="Minimum Adverse Event Count",min=5,max=100,value=10, step=1)
                                                                                                                                          ),
                                                                                                                                          tags$div(
                                                                                                                                              h5("Filter Substances by ATC Classification"),
                                                                                                                                              uiOutput("class1"),
                                                                                                                                              uiOutput("class2"),
                                                                                                                                              uiOutput("class3"),
                                                                                                                                              uiOutput("class4") 
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
                                                withSpinner(plotlyOutput("mult_ae1"))
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
                                   box(title="Parameters", status="info", side="left", width=NULL,
                                       collapsible = TRUE,
                                       collapsed = TRUE,
                                       uiOutput("ptcount2"),
                                       # numericInput(inputId="ptcount2",label="Adverse Event Count",min=5,max=100,value=10),
                                       actionButton("reset_subs", "Reset")
                                   )
                            ),
                            column(width=8,
                                   tabBox(
                                       width=NULL,
                                       id = "tabset2",
                                       # height = "450px",
                                       tabPanel("Two Substances", div(
                                           style = "position:relative",
                                           actionButton("dt_open2", "Data Table"),
                                           plotOutput("scatterPlot2", hover = hoverOpts("plot_hover2")),
                                           uiOutput("hover_coords"),
                                           textOutput('cor2'),
                                           tags$head(tags$style("#cor2{font-size: 16px; color: grey}")),
                                           bsTooltip("cor2", "Pearson&#39s Correlation Coefficient", placement="left")
                                       )),
                                       
                                       bsModal("subs_dt", "Data Table", trigger="dt_open2", size="large",
                                                div(style="float:right", downloadButton("download_3", "Download")),
                                                div(style="float:right", selectInput("downloadType3", label=NULL, choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
                                                
                                                DTOutput("table2")
                                       ),
                                       tabPanel(title = "Multiple Susbtances", width = "100%",
                                                selectizeInput("sub1", "Substance", vars2, width = "30%",  selected = vars2[11], options = list(maxOptions=2500)),
                                                # other_ae --> sub2
                                                dropdownBtn(
                                                    label = "Compare With", status = "default", width = "100%",
                                                    checkboxGroupInput(inputId = "sub2", label = "Choose", choices = vars2)
                                                ),
                                                span("Showing correlations for substances with \u2265"),
                                                div(style="display: inline-block;", numericInput("num_ae", label = NULL, value = 5, width = "55px", min=2, max=1000)),
                                                span("correlated adverse events. "),
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
                                   div(style="line-height: 50%", 
                                       div(style="display: block; float: left", "* Denotes that the selected drug is in the selected class"), br(),
                                       div(style="display: inline-block; vertical-align:top; width:20%", uiOutput("cc_2")), 
                                       p(" vs.", style="font-size: 18px; font-weight: bold; display: inline-block; vertical-align: -520%"),
                                       div(style="display: inline-block; vertical-align:top; width:10%", selectInput("class_op", "Class Type", c("ATC Class", "User-selected drugs", "Upload list"))),
                                       hidden(div(id="atc_div", div(style="display: inline-block; vertical-align: top; width:7%", selectInput("cc_level", "Level", c("1", "2", "3", "4"), multiple=FALSE, selected="1")),
                                           div(style="display: inline-block; vertical-align: top; width:30%", uiOutput("cc_1")))),
                                       hidden(div(id="cstm", style="display: inline-block; vertical-align: -520%; width: 10%", dropdownBtn(
                                         label = "Custom list", status = "default", width = "50%",
                                         checkboxGroupInput(inputId = "custom_list", label = "Choose", choices = vars2)))),
                                       hidden(div(id="upload_div", style="display: inline-block; vertical-align: top; width: 15%", fileInput("drugs_file", "Upload", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".xlsx")))))
                            )
                        ),
                        
                        fluidRow(
                            column(id="column1", width=12,
                                   tabBox(width=NULL,
                                     tabPanel("Correlations", div(style="overflow-x: scroll; position: relative", withSpinner(plotlyOutput("drug_cor")))),
                                     tabPanel("Box Plots", status="success", div(style="overflow-x: scroll; position: relative", withSpinner(plotlyOutput("boxplots")))),
                                     tabPanel("Histogram", width=12,
                                              splitLayout(  
                                                       div(style="margin-top: 20px", plotlyOutput("histogram")),
                                                       div(div(style="float: left", numericInput("pcentile_input", "Percentile", value=95, min=1, max=99, step=.5)),
                                                       div(style="float:right", downloadButton("dload4_1", "Download")),
                                                       div(style="float:right", selectInput("downloadType4_1", label=NULL, choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
                                                       DTOutput("drugperc")))
                                       ),
                                     tabPanel("PRRs Sorted", div(style="width:30%", uiOutput("sortby4")), div(style="overflow-x: scroll; position: relative", withSpinner(plotlyOutput("bar4"))), status="warning")
                                   )
                            )
                        )
                )
                
            )
        )
    )
)
)
)
