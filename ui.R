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
        dashboardHeader(title = "G-SRS Dashboard",
                        dropdownMenu(type = "messages", icon=icon("question-circle"), headerText = "",
                                     messageItem(
                                       from = "PRR",
                                       icon=icon("chart-bar"),
                                       message = div(style="white-space:normal;", "[definition of PRR]Drugs in this database are chemical substance used in the treatment, cure, prevention, or diagnosis of disease")
                                     ),
                                     messageItem(
                                       from = "Drug",
                                       message = "Drugs in this database are chemical substance used in the treatment, cure, prevention, or diagnosis of disease",
                                       icon = icon("prescription-bottle")
                                     ),
                                     messageItem(
                                       from = "Adverse Event",
                                       message = "An adverse event is an unanticipated experience or side effect associated with the use of a drug or therapeutic biologic in humans, whether or not it is considered related to the product.",
                                       icon = icon("lungs-virus")
                                     ))
                        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Browse Substances", tabName="intro"),
                menuItem("Browse Adverse Events", tabName="intro2"),
                menuItem("Compare Adverse Events", tabName="compare_ae"),
                menuItem("Compare Substances", tabName = "compare_subs"),
                menuItem("Class Comparison", tabName="class_comp"),
                menuItem("Clustering", tabName="heatmap")
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
                                   box(id= "intro-pie", width = NULL, height="50%", status="warning",
                                       solidHeader = TRUE, 
                                       actionButton("pop_pie", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                       bsModal("pop_pie_modal", "", trigger="pop_pie", size="large", plotlyOutput("pie_chart2")),
                                       tags$br(), tags$br(),
                                       plotlyOutput("pie_chart")
                                       ),
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
                # NEW
                tabItem("intro2",
                        fluidRow(
                          column(width = 12,
                                 box(width=9, solidHeader=TRUE,
                                     selectInput("intro_ae", "Select Adverse Event", vars, multiple=FALSE, selected="ABDOMINAL ADHESIONS")),
                                 
                                 box(width=3, solidHeader=TRUE,
                                     
                                     div(align="center", 
                                         selectInput("downloadType6", "Download As", c(".csv", ".txt", ".xlsx", ".json"), selected=".csv", width="60%"),
                                         downloadButton("dload2", "Download")),
                                 )
                          )
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 box(id= "intro-pie2", width = NULL, status="warning",
                                     solidHeader = TRUE, 
                                     actionButton("pop_pie2", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                     bsModal("pop_pie_modal2", "", trigger="pop_pie2", size="large", plotlyOutput("pie_chart22")),
                                     tags$br(), tags$br(),
                                     plotlyOutput("pie_chart21")
                                 ),
                                 box(id="table-box2", title="Summary Statistics", width=NULL, status="primary",
                                     solidHeader=T, dataTableOutput("sum_table2"))
                                 
                          ),
                          
                          column(width = 6,
                                 box(id= "intro-box2", title = "Adverse Events", width = NULL, 
                                     
                                     div(style="display: inline-block;", selectInput("sort_by2", c("PT Count", "PRR"), 
                                                                                     label = "Sort by", selected="PT Count", multiple=FALSE, width = "210px")),
                                     actionButton("popdt2", "", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                     span(style="float:right;  padding-right: 20px; visibility:hidden", "."),
                                     div(id="subsbar", style="overflow-y: scroll; position: relative", plotlyOutput("top_subs"))
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
                                           actionButton("pop_scatterPlot", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                           bsModal("scatterPlot_modal", "", trigger="pop_scatterPlot", size="large", plotOutput("scatterPlot12")),
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
                                                actionButton("pop_mult_ae1", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                                bsModal("mult_ae1_modal", "", trigger="pop_mult_ae1", size="large", plotlyOutput("mult_ae12")),
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
                                           actionButton("pop_scatterPlot2", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                           bsModal("scatterPlot2_modal", "", trigger="pop_scatterPlot2", size="large", plotOutput("scatterPlot22")),
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
                                                "Minimum adverse event count: ",
                                                div(style="display: inline-block;", numericInput("min_ae", label = NULL, value = 10, width = "60px", min=5, max=100)),
                                                actionButton("pop_subs_bar", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                                bsModal("subs_bar_modal", "", trigger="pop_subs_bar", size="large", plotlyOutput("subs_bar2")),
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
                                     tabPanel("Correlations", div(style="overflow-x: scroll; position: relative", 
                                                                  actionButton("pop_drug_cor", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                                                  bsModal("drug_cor_modal", "", trigger="pop_drug_cor", size="large",div(style="overflow-x: scroll; position: relative",  plotlyOutput("drug_cor2"))),
                                                                  tags$br(), tags$br(),
                                                                  withSpinner(plotlyOutput("drug_cor")))),
                                     tabPanel("Box Plots", status="success", div(style="overflow-x: scroll; position: relative", 
                                                                                 actionButton("pop_boxplots", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                                                                 bsModal("boxplots_modal", "", trigger="pop_boxplots", size="large", div(style="overflow-x: scroll; position: relative",plotlyOutput("boxplots2"))),
                                                                                 tags$br(), tags$br(),
                                                                                 withSpinner(plotlyOutput("boxplots")))),
                                     tabPanel("Histogram", width=12,
                                              splitLayout(  
                                                       div(style="margin-top: 40px; overflow-x: hidden", 
                                                           actionButton("pop_histogram", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: left"), 
                                                           bsModal("histogram_modal", "", trigger="pop_histogram", size="large", div(style="margin-top: 60px; overflow-x: hidden", plotlyOutput("histogram2"))),
                                                           tags$br(), tags$br(),
                                                           withSpinner(plotlyOutput("histogram"))),
                                                       div(uiOutput("drug_name"), tags$style(HTML("#drug_name{font-size: large; text-align: center}")), div(style="float: left; display:inline-block", numericInput("pcentile_input", "Percentile", value=95, step=.5, width="75px")),
                                                                                      div(style="display: inline-block", uiOutput("perc_val")), div(style="display: inline-block", uiOutput("perc_count")),
                                                       div(style="float:right", downloadButton("dload4_1", "Download")),
                                                       div(style="float:right", selectInput("downloadType4_1", label=NULL, choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
                                                       DTOutput("drugperc")))
                                       ),
                                     tabPanel("PRRs Sorted", div(style="width:30%", uiOutput("sortby4")), div(style="overflow-x: scroll; position: relative", 
                                                                                                              actionButton("pop_bar4", label="", icon = icon("fas fa-expand-arrows-alt"), style="display: inline-block; float: right"), 
                                                                                                              tags$br(), tags$br(),
                                                                                                              bsModal("bar4_modal", "", trigger="pop_bar4", size="large", div(style="overflow-x: scroll; position: relative", plotlyOutput("bar42"))),
                                                                                                              withSpinner(plotlyOutput("bar4"))), status="warning")
                                   )
                            )
                        )
                ),
                tabItem("heatmap", 
                        fluidPage(
                          shiny::sidebarLayout(
                            shiny::sidebarPanel(
                              numericInput("num_pt_input", "Number of PT TERMs", min=1, max=100, value=20),
                              htmltools::h4('Data'),
                              fileInput("heat_file", "File Upload",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              selectizeInput('class_choices', 'ATC Class', choices = list(
                                "Level 1" = l1,
                                "Level 2" = l2,
                                "Level 3" = l3,
                                "Level 4 "= l4), selected="ANTIINFLAMMATORY AGENTS", multiple = FALSE, options= list(maxOptions = 11200)),
                              tags$b("Create List"),
                              dropdownBtn(
                                label = "Select", status = "default", width = "100%",
                                checkboxGroupInput(inputId = "check_drugs", label = "Select", choices = vars2)
                              ),
                              div(style="display: inline-block; float: right", hidden(actionButton('done_heat', "Run"))),
                              shiny::uiOutput('data'),
                              shiny::conditionalPanel('input.showSample',shiny::uiOutput('sample')),
                              # br(),
                              
                              htmltools::br(),htmltools::hr(),htmltools::h4('Row dendrogram'),
                              shiny::column(width=6,shiny::selectizeInput("distFun_row", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
                              shiny::column(width=6,shiny::selectizeInput("hclustFun_row", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'average')),
                              shiny::column(width=12,shiny::numericInput("r", "Number of Clusters", min = 1, max = 9, step=1, value = 2)),    
                              #column(width=4,numericInput("r", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),   
                              
                              htmltools::br(),htmltools::hr(),htmltools::h4('Column dendrogram'),
                              shiny::column(width=6,shiny::selectizeInput("distFun_col", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
                              shiny::column(width=6,shiny::selectizeInput("hclustFun_col", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'average')),
                              shiny::column(width=12,shiny::sliderInput("c", "Number of Clusters", min = 1, max = 9, value = 2)),
                              #column(width=4,numericInput("c", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),    
                              
                              htmltools::br(),htmltools::hr(),  htmltools::h4('Additional Parameters'),
                              
                              shiny::column(3,shiny::checkboxInput('showColor','Color')),
                              shiny::column(3,shiny::checkboxInput('showMargin','Layout')),
                              shiny::column(3,shiny::checkboxInput('showDendo','Dendrogram')),
                              htmltools::hr(),
                              shiny::conditionalPanel('input.showColor==1',
                                                      htmltools::hr(),
                                                      htmltools::h4('Color Manipulation'),
                                                      shiny::uiOutput('colUI'),
                                                      shiny::sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
                                                      shiny::checkboxInput('colRngAuto','Auto Color Range',value = TRUE),
                                                      shiny::conditionalPanel('!input.colRngAuto',shiny::uiOutput('colRng'))
                              ),
                              
                              shiny::conditionalPanel('input.showDendo==1',
                                                      htmltools::hr(),
                                                      htmltools::h4('Dendrogram Manipulation'),
                                                      shiny::selectInput('dendrogram','Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both'),
                                                      shiny::selectizeInput("seriation", "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
                                                      shiny::sliderInput('branches_lwd','Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1)
                              ),             
                              
                              shiny::conditionalPanel('input.showMargin==1',
                                                      htmltools::hr(),
                                                      htmltools::h4('Widget Layout'),
                                                      shiny::column(4,shiny::textInput('main','Title','')),
                                                      shiny::column(4,shiny::textInput('xlab','X Title','')),
                                                      shiny::column(4,shiny::textInput('ylab','Y Title','')),
                                                      shiny::sliderInput('row_text_angle','Row Text Angle',value = 0,min=0,max=180),
                                                      shiny::sliderInput('column_text_angle','Column Text Angle',value = 45,min=0,max=180),
                                                      shiny::sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
                                                      shiny::sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40)
                              )
                            ),
                            
                            shiny::mainPanel(
                              shiny::tabsetPanel(
                                shiny::tabPanel("Heatmaply",
                                                htmltools::tags$a(id = 'downloadData', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, shiny::icon("clone"), 'Download Heatmap as HTML'),
                                                htmltools::tags$head(htmltools::tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
                                                withSpinner(plotly::plotlyOutput("heatout",height=paste0(plotHeight,'px')))
                                ),
                                shiny::tabPanel("Matrix",
                                                div(selectInput("downloadTypeHeat", "Download As", c(".csv", ".txt", ".xlsx", ".json"), selected=".csv", width="60%"),
                                                    downloadButton("dloadheat", "Download")),
                                                DT::dataTableOutput('tables')
                                ),
                                shiny::tabPanel("Dendrogram",
                                                withSpinner(plotOutput("dendro"))
                                ),
                                shiny::tabPanel("Substance Clusters",
                                                withSpinner(dataTableOutput('drug.cluster'))
                                ),
                                shiny::tabPanel("ATC Comparison",
                                                withSpinner(plotOutput("grouped"))
                                )
                              ) 
                            )
                        ))
                )
            )
        )
    )
)
)
)
