library(shiny)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(shinydashboard)
library(tools)
library(httr)
library(gghighlight)
library(dplyr)
library(writexl)
library(plotly)
library(shinyBS)
library(shinycssloaders)
library(formattable)
library(tinytex) # formattable dependency
library(dplyr)
library(shinyjs)
library(jsonlite)
library(scales)
library(doBy)
library(readxl)
library(stringr)

function(input, output, session) {
    
    # ------------------------------------------------INTRO PG--------------------------------------------------
    load_data()
    
    onclick("startbutton",
        {hide("start-page");
        show("main-content")}
    )
    
    #right-side vertical bar chart
    output$top_ae <- renderPlotly({
        #adverse events
        aes = dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)] %>% distinct();
        shiny::validate(
            need(nrow(aes)>0, 'No Data Available')
        )
        if(input$sort_by == "Number of Adverse Events"){
            plot_ly(aes, x=~PT_COUNT, y=~reorder(PT_TERM, PT_COUNT),type = "bar", orientation = "h", height = 12*nrow(aes), width="100%",
                    color = I("#f5c767"), source = "C") %>%
                layout(bargap=.1, showlegend = FALSE, autosize=TRUE, yaxis = list(title="", automargin = TRUE, tickfont = list(size = 8)), 
                       xaxis = list(title="Number of Adverse Events", side="top")) %>%
                plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
        }
        else {
            plot_ly(aes, x=~PRR, y=~reorder(PT_TERM, PRR), type = "bar", orientation = "h", height = 12*nrow(aes), width="100%",
                    color=I("#821e4e"), source = "D") %>%
                layout(bargap=.1, showlegend = FALSE, autosize=TRUE, yaxis = list(title="", automargin = TRUE, tickfont = list(size = 8)), 
                       xaxis = list(automargin=TRUE, side="top")) %>%
                plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
        }
    })
    
    #download buttons
    output$dload <- downloadHandler(
        
        filename = function(){
            paste(input$intro_drug, "-related adverse events", input$downloadType)
        },
        content = function(file){
            if(input$downloadType == ".csv") {
                write.csv(dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)], file, row.names = FALSE)
            } else if(input$downloadType == ".json") {
                exportJSON <- toJSON(dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)])
                write(exportJSON, file)
            } else if(input$downloadType == ".xlsx") {
                write_xlsx(dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)], path=file)
            } else if(input$downloadType == ".txt") {
                write.table(dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)], file, row.names=FALSE)
            }
        }
        
    )
    
    # custon color bar
    my_color_bar <- function (color = "orange", fixedWidth=180,...) 
    {
        formatter("span", style = function(x) style(display = "inline-block", 
                                                    direction = "rtl", `border-radius` = "4px", `padding-right` = "2px", 
                                                    `background-color` = csscolor(color), width = paste(fixedWidth*proportion(x),"px",sep=""), 
                                                    ...))
    }
    
    
    #pop up formattable dt on maximize button
    observeEvent(input$popdt, {
        aes = dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)]
        if(nrow(aes)==0){
            showNotification("Not enough data available", type="warning")
        }
        shiny::validate(
            need(nrow(aes)>0, "No Data Available")
        )
        aes$PT_COUNT = round(aes$PT_COUNT, 2)
        aes$PRR = round(aes$PRR, 2)
        
        if(input$sort_by=="Number of Adverse Events"){
            aes <- aes[order(-aes$PT_COUNT),]
            df <- data.frame(ae=aes$PT_TERM, count=aes$PT_COUNT, prr=aes$PRR)
            colnames(df) <- c("Adverse Event", "Count", "PRR")
        }
        
        else{
            aes <- aes[order(-aes$PRR),]
            df <- data.frame(ae=aes$PT_TERM, count=aes$PT_COUNT, prr=aes$PRR)
            colnames(df) <- c("Adverse Event", "Count", "PRR")
        }
        
        showModal(modalDialog(
            size = "l",
            easyClose = TRUE,
            title=paste0(input$intro_drug, "-related Adverse Events"),
            div(renderFormattable({formattable(df, align = c("l",rep("r", ncol(df) - 1)),
                                               list(Count=my_color_bar(color="#fad584"), PRR=my_color_bar(color="#cf4085"))
            )}), style="max-height: 510px; overflow-y: scroll"),
            footer = tagList(
                modalButton("Exit")
            )
        ))
        
    })
    
    #pie
    output$pie_chart <- renderPlotly({
        total_rows = nrow(dset[which(dset$INAME==input$intro_drug),])
        shiny::validate(
            need(total_rows>0, "No Data Available")
        )
        g1 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR<1),])
        g2 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=1 & dset$PRR<5),])
        g3 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=5 & dset$PRR<10),])
        g4 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=10 & dset$PRR<100),])
        g5 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=100),])
        df <- data.frame("label"=c("<1", "1-5", "5-10", "10-100", ">100"), "vals" = c(g1, g2, g3, g4, g5))
        plot_ly(df, labels=~label, values=~vals, key=c("<1", "1-5", "5-10", "10-100", ">100"), type="pie", source="E", hovertemplate="PRR: %{label} <br> %{value}<extra></extra>", name="") %>%
            plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE) %>% 
            layout(autosize = T, height = 250, legend=list(title=list(text='<b> PRR </b>')))
    })
    
    observeEvent(event_data("plotly_click", source = "E"), {
        pieData = event_data("plotly_click", source = "E")
        if(pieData$key=="<1") tableDat = dset[which(dset$INAME==input$intro_drug & dset$PRR<1), c(3, 7, 11)]
        else if(pieData$key=="1-5") tableDat = dset[which(dset$INAME==input$intro_drug & dset$PRR>=1 & dset$PRR<5), c(3, 7, 11)]
        else if(pieData$key=="5-10") tableDat = dset[which(dset$INAME==input$intro_drug & dset$PRR>=5 & dset$PRR<10), c(3, 7, 11)]
        else if(pieData$key=="10-100") tableDat = dset[which(dset$INAME==input$intro_drug & dset$PRR>=10 & dset$PRR<100), c(3, 7, 11)]
        else if(pieData$key==">100") tableDat = dset[which(dset$INAME==input$intro_drug & dset$PRR>=100), c(3, 7, 11)]
        
        output$pie_data <- renderDataTable({datatable(tableDat, selection = "none", rownames=FALSE, options = list(
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
            autoWidth = FALSE,
            width="100%",
            scrollX = TRUE,
            scrollY = '350px')
        )})
        
        output$download_5 <- downloadHandler(
            
            filename = function(){
                paste(input$intro_drug, "-related adverse events, PRR ", pieData$key, input$downloadType5)
            },
            content = function(file){
                if(input$downloadType5 == ".csv") {
                    write.csv(tableDat, file, row.names = FALSE)
                } else if(input$downloadType5 == ".json") {
                    exportJSON <- toJSON(tableDat)
                    write(exportJSON, file)
                } else if(input$downloadType5 == ".xlsx") {
                    write_xlsx(tableDat, path=file)
                } else if(input$downloadType5 == ".txt") {
                    write.table(tableDat, file, row.names=FALSE)
                }
            }
            
        )
        
        showModal(modalDialog(
            size = "l",
            title = paste0(input$intro_drug, "- related adverse events with PRR ", pieData$key),
            div(style="float:right", downloadButton("download_5", "Download")),
            div(style="float:right", selectInput("downloadType5", label=NULL, choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
            
            # div(style="display: inline-block; float: right", downloadButton("pie_csv", "CSV")),
            # div(style="display: inline-block; float: right", downloadButton("pie_txt", "TXT")),
            # div(style="display: inline-block; float: right", downloadButton("pie_xlsx", "XLSX")),
            tags$br(),
            tags$br(),
            DT::dataTableOutput("pie_data"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Exit")
            )
        ))
        
    })
    
    #summary table
    output$sum_table <- renderDT({
        
        #mean, median, standard deviation, count, iqr
        dat1 <- dset$PRR[which(dset$INAME==input$intro_drug)]
        q1_prr <- round(quantile(dat1)[2], 2)
        median_prr = round(median(dat1), 2)
        q3_prr <- round(quantile(dat1)[4], 2)
        mean_prr <- round(mean(dat1), 2)
        std_prr <- round(sd(dat1), 2)
        iqr_prr <- round(IQR(dat1), 2)
        min_prr <- round(min(dat1), 2)
        max_prr <- round(max(dat1), 2)
        
        dat2 <- dset$PT_COUNT[which(dset$INAME==input$intro_drug)]
        q1_cnt <- round(quantile(dat2)[2], 2)
        median_cnt <- round(median(dat2), 2)
        q3_cnt <- round(quantile(dat2)[4], 2)
        mean_cnt <- round(mean(dat2), 2)
        std_cnt <- round(sd(dat2), 2)
        iqr_cnt <- round(IQR(dat2), 2)
        min_cnt <- round(min(dat2), 2)
        max_cnt <- round(max(dat2), 2)
        
        # Min, q1, median, q3, max, mean, standard deviation
        df <- cbind(data.frame("1" =c("PRR", "Count"), "2" = c(min_prr, min_cnt), "3"=c(q1_prr, q1_cnt), "4" = c(median_prr, median_cnt), 
                               "5"=c(q3_prr, q3_cnt), "6" = c(max_prr, max_cnt), "7"=c(mean_prr, mean_cnt), "8"=c(std_prr, std_cnt)));
        if(length(dat1)>0){
            n=dset$CASE_COUNT[which(dset$INAME==input$intro_drug)][1]
        }
        else{
            n=0
        }
        colnames(df) = c(paste0(input$intro_drug, " (N=", n, ")"), "Min", "Q1", "Median",  "Q3", "Max", "Mean", "STD")
        
        dt <- datatable(df, rownames = FALSE, options = list(dom = 't', scrollX=T)) %>%
            DT::formatStyle(names(df),lineHeight='90%') 
        dt
    })
    
    # -------------------------------------------------PAGE 2---------------------------------------------------
    
    
    #-------------------------------------------Multiple Selection, box 3-------------------------------------------
    
    # reactive vals
    pts <- reactiveValues(data = data.frame(name = character(), cor = double()))
    pts_temp <- reactiveValues(data = data.frame(name = character(), cor = double()))
    atcs <- reactiveValues(data = data.frame(atc1 = character(), atc2 = character(), atc3 = character(), atc4 = character()))
    curr_level <- reactiveVal(value = 0)
    cc_1 <- reactiveVal(value=1000) # because sliderinput value change isn't recognized
    ptc_1 <- reactiveVal(value=10)
    
    # dt on plotly_click
    observeEvent(event_data("plotly_click", source = "A"),  {
        barData = event_data("plotly_click", source = "A");
        
        if(curr_level()==0){
            col = dset$ATC1;
            list = dset$ATC1;
        }
        else if(curr_level()==1){
            col = dset$ATC1;
            list = input$class_l1;
        }
        else if(curr_level()==2){
            col = dset$ATC2;
            list = input$class_l2;
        }
        else if(curr_level()==3){
            col = dset$ATC3;
            list = input$class_l3;
        }
        else if(curr_level()==4){
            col = dset$ATC4;
            list = input$class_l4;
        }
        pt1 <- subset(dset, select = c(INAME,CASE_COUNT, PT_COUNT, PRR, ATC1, ATC2, ATC3, ATC4),
                      subset=(PT_TERM == input$ae1 & col %in% list & CASE_COUNT>cc_1() & PT_COUNT>ptc_1()))
        pt2 <- subset(dset, select = c(INAME, PT_COUNT, PRR),
                      subset = (PT_TERM == barData$x & col %in% list & CASE_COUNT>cc_1() & PT_COUNT>ptc_1()))
        comb = merge(pt1, pt2, by="INAME") %>% distinct()
        comb = comb[, c(1, 2, 3, 9, 4, 10, 5, 6, 7, 8)];
        
        colnames(comb) = c("SUBSTANCE", "SUBSTANCE COUNT", paste(input$ae1, " COUNT"), paste(barData$x, " COUNT"), paste(input$ae1, " PRR"), 
                           paste(barData$x, " COUNT"), "ATC LEVEL 1", "ATC LEVEL 2", "ATC LEVEL 3", "ATC LEVEL4")
        
        
        
        output$drugs_dt <- renderDataTable({datatable(comb, selection = "none", options = list(
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
            autoWidth = TRUE,
            scrollX = TRUE,
            scrollY = '350px')
        )})
        
        output$download_6 <- downloadHandler(
            filename = function(){
                paste(input$ae1, " VS. ", barData$x, input$downloadType6)
            },
            content = function(file){
                if(input$downloadType6 == ".csv") {
                    write.csv(comb, file, row.names = FALSE)
                } else if(input$downloadType6 == ".json") {
                    exportJSON <- toJSON(comb)
                    write(exportJSON, file)
                } else if(input$downloadType6 == ".xlsx") {
                    write_xlsx(comb, path=file)
                } else if(input$downloadType6 == ".txt") {
                    write.table(comb, file, row.names=FALSE)
                }
            }
        )
        showModal(modalDialog(
            size = "l",
            title = paste0(barData$x, " vs.", input$ae1, " (r=", round(barData$y, digits=2), ")"),
            div(style="float:right", downloadButton("download_6", "Download")),
            div(style="float:right", selectInput("downloadType6", label=NULL, 
                                                 choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
            # div(style="display: inline-block; float: right", downloadButton("mult_comp_csv", "CSV")),
            # div(style="display: inline-block; float: right", downloadButton("mult_comp_txt", "TXT")),
            # div(style="display: inline-block; float: right", downloadButton("mult_comp_xlsx", "XLSX")),
            # tags$br(),
            # tags$br(),
            DT::dataTableOutput("drugs_dt"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Exit")
            )
        ));
    })
    
    
    # reactive filter inputs
    atc_l1 <- reactive({
        return(unique(atcs$data$atc1));
    })
    output$class1 = renderUI({
        selectInput("class_l1", "Level 1 Class", atc_l1(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    atc_l2 <- reactive({
        dat = atcs$data;
        if(!is.null(input$class_l1)){
            list = dat$atc2[which(dat$atc1 %in% input$class_l1)]
            if(length(list)>0) return(list)
            else return(list)
        }
    })
    output$class2 = renderUI({
        selectInput("class_l2", "Level 2 Class", atc_l2(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    atc_l3 <- reactive({
        dat = atcs$data;
        if(!is.null(input$class_l2)){
            list = dat$atc3[which(dat$atc2 %in% input$class_l2)]
            if(length(list)>0) return(list)
            else return(list)
        }
        
    })
    output$class3 = renderUI({
        selectInput("class_l3", "Level 3 Class", atc_l3(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    atc_l4 <- reactive({
        dat = atcs$data;
        if(!is.null(input$class_l3)){
            list = dat$atc4[which(dat$atc3 %in% input$class_l3)]
            if(length(list)>0) return(list)
            else return(list)
        }
    })
    output$class4 = renderUI({
        selectInput("class_l4", "Level 4 Class", atc_l4(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    
    
    # update pts$data for atc filtering
    
    # helper to render based on all inputs (sliders, classes, numobs)
    rerender <- function() {
        if(curr_level()==0){
            col = dset$ATC1;
            list = dset$ATC1;
        }
        else if(curr_level()==1){
            col = dset$ATC1;
            list = input$class_l1;
        }
        else if(curr_level()==2){
            col = dset$ATC2;
            list = input$class_l2;
        }
        else if(curr_level()==3){
            col = dset$ATC3;
            list = input$class_l3;
        }
        else if(curr_level()==4){
            col = dset$ATC4;
            list = input$class_l4;
        }
        pts_temp$data = data.frame(name = character(), cor = double());
        for(ae in input$other_ae){
            pt1 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, L10_PRR, ATC1),
                          subset=(PT_TERM == input$ae1 & col %in% list & CASE_COUNT>cc_1() & PT_COUNT>ptc_1()))
            pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, L10_PRR, ATC1),
                          subset = (PT_TERM == ae & col %in% list & CASE_COUNT>cc_1() & PT_COUNT>ptc_1()))
            comb = merge(pt1, pt2, by="INAME") %>% distinct()
            if(length(comb$INAME) >= input$num_obs) {
                pts_temp$data = rbind(pts_temp$data, data.frame(name = ae, cor = cor(comb$L10_PRR.x, comb$L10_PRR.y)));
            }
        }
    }
    
    
    
    observeEvent(input$class_l1, {
        if(length(input$class_l1) == 0){
            curr_level(0);
        }
        else curr_level(1);
        rerender();
    })
    observeEvent(input$class_l2, {
        if(length(input$class_l2) == 0){
            curr_level(1);
        }
        else curr_level(2);
        rerender();
    })
    observeEvent(input$class_l3, {
        if(length(input$class_l3) == 0){
            curr_level(2);
        }
        else curr_level(3);
        rerender();
    })
    observeEvent(input$class_l4, {
        if(length(input$class_l4) == 0){
            curr_level(3);
        }
        else curr_level(4);
        rerender();
    })
    
    # apply filtering on "filter" button
    observeEvent(input$filt, {
        rerender()
        pts$data = pts_temp$data
        toggleModal(session, "filter", toggle="close")
    })
    
    # since update slider input doesn't work properly
    observeEvent(input$casecount_box1, {
        cc_1(input$casecount_box1)
    })
    
    observeEvent(input$ptcount_box1, {
        ptc_1(input$ptcount_box1)
    })
    observeEvent(input$reset, {
        curr_level(0)
        if(!is.null(input$class_l1)) updateSelectInput(session, "class_l1", "Level 1 Class", atc_l1(), selected=NULL)
        if(!is.null(input$class_l2)) updateSelectInput(session, "class_l2", "Level 2 Class", atc_l2(), selected=NULL)
        if(!is.null(input$class_l3)) updateSelectInput(session, "class_l3", "Level 3 Class", atc_l3(), selected=NULL)
        if(!is.null(input$class_l4)) updateSelectInput(session, "class_l4", "Level 4 Class", atc_l4(), selected=NULL)
        updateSliderInput(session, inputId="casecount_box1",label="Minimum Case Count",min=100,max=50000,value=1000)
        updateSliderInput(session, inputId="ptcount_box1",label="Minimum Adverse Event Count",min=5,max=100,value=10)
        cc_1(1000)
        ptc_1(10)
        rerender()
        pts$data = pts_temp$data
    })
    
    
    #-------------------------------------choose substances to compare to functionality-------------------------------------
    
    # on change, clear input of other_ae
    observeEvent(input$ae1, {
        rerender();
        pts$data = pts_temp$data;
    })
    
    
    observeEvent(input$other_ae, {
        ae_list = input$other_ae
        if(dim(pts$data)[1] < length(ae_list)){
            new_ae = tail(ae_list, n=1);
            pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PT_TERM, L10_PRR, ATC1, ATC2, ATC3, ATC4),
                          subset=(PT_TERM == input$ae1 & CASE_COUNT>cc_1() & PT_COUNT>ptc_1()))
            pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, L10_PRR, ATC1, ATC2, ATC3, ATC4),
                          subset = (PT_TERM == new_ae & CASE_COUNT>cc_1() & PT_COUNT>ptc_1()))
            comb = merge(pt1, pt2, by="INAME") %>% distinct()
            if(length(comb$INAME) >= input$num_obs){
                pts$data = rbind(pts$data, data.frame(name = new_ae, cor = cor(comb$L10_PRR.x, comb$L10_PRR.y))) %>% distinct();
                atcs$data = rbind(atcs$data, data.frame(atc1 = comb$ATC1.x, atc2 = comb$ATC2.x, atc3 = comb$ATC3.x, atc4 = comb$ATC4.x));
            }
        }
        else{
            del = setdiff(pts$data$name, ae_list);
            pts$data = pts$data[which(pts$data$name != del),];
        }
    }, ignoreNULL = FALSE)
    
    observeEvent(input$num_obs, {
        rerender()
        pts$data = pts_temp$data
    })
    
    #----------------------------------------------------plot-------------------------------------------------------
    
    output$mult_ae1 <- renderPlotly({
        dat = pts$data
        shiny::validate(
            need(nrow(dat)>0, "No Data Available")
        )
        if(nrow(dat)>5) margin_size=160
        else margin_size=30
        plot_ly(dat, x = ~reorder(name, -cor), y = ~round(cor, digits = 2), type = "bar", color = ~cor>0, colors = c("#e82a2a", "#1b3fcf"), 
                source = "A", name = "<extra><extra>") %>% 
            layout(
                yaxis = list(title = "Pearson&#39;s Correlation Coefficient"),
                xaxis = list(title = "Adverse Event"),
                showlegend = FALSE,
                margin = list(b=margin_size)
            ) %>%
            plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
    })
    
    #-------------------------------------------------------box 1, page 2-------------------------------------------------
    
    output$casecount <- renderUI({
        dat <- data_unfilt()
        numericInput(inputId="casecount",label="Substance Count",min=min(dat$CASE_COUNT.x),
                     max=max(dat$CASE_COUNT.x),value=max(1000, min(dat$CASE_COUNT.x)), step=1)
    })
    
    output$ptcount <- renderUI({
        dat <- data_unfilt()
        numericInput(inputId="ptcount",label="Adverse Event Count",min=min(dat$PT_COUNT.x),max=max(dat$PT_COUNT.x),value=max(5, min(dat$PT_COUNT.x)), step=1)
    })
    
    
    # CLASS LEVEL 1
    list1 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat <- data_unfilt()
            return(dat$ATC1.x[which(dat$CASE_COUNT.x>ifelse(is.null(input$casecount),1000,input$casecount) & dat$PT_COUNT.x>ifelse(is.null(input$ptcount),10,input$ptcount))])
        }
    })
    output$list1 = renderUI({
        pickerInput("atc1", "Level 1 Class", unique(list1()), multiple=TRUE, selected=NULL)
    })
    
    
    # CLASS LEVEL 2
    list2 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat = data_unfilt()
            if(!is.null(input$atc1)){
                dat = dat[which(dat$ATC1.x %in% input$atc1),]
            }
            return(dat$ATC2.x[which(dat$CASE_COUNT.x>ifelse(is.null(input$casecount),1000,input$casecount) & dat$PT_COUNT.x>ifelse(is.null(input$ptcount),10,input$ptcount))])
        }
    })
    output$list2 = renderUI({
        pickerInput("atc2", "Level 2 Class", unique(list2()), multiple=TRUE, selected=NULL)
    })
    
    
    # CLASS LEVEL 3
    list3 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat = data_unfilt()
            if(!is.null(input$atc2)){
                dat = dat[which(dat$ATC2.x %in% input$atc2),]
            }
            return(dat$ATC3.x[which(dat$CASE_COUNT.x>ifelse(is.null(input$casecount),1000,input$casecount) & dat$PT_COUNT.x>ifelse(is.null(input$ptcount),10,input$ptcount))])
        }
    })
    output$list3 = renderUI({
        pickerInput("atc3", "Level 3 Class", unique(list3()), multiple=TRUE, selected=NULL)
    })
    
    
    # CLASS LEVEL 4
    list4 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat = data_unfilt()
            if(!is.null(input$atc3)){
                dat = dat[which(dat$ATC3.x %in% input$atc3),]
            }
            return(dat$ATC4.x[which(dat$CASE_COUNT.x>ifelse(is.null(input$casecount),1000,input$casecount) & dat$PT_COUNT.x>ifelse(is.null(input$ptcount),10,input$ptcount))])
        }
    })
    output$list4 = renderUI({
        pickerInput("atc4", "Level 4 Class", unique(list4()), multiple=TRUE, selected=NULL)
    })
    
    
    # reactive data based on widgets (case count, pt count)
    data <- reactive({
        pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PT_TERM, PRR, L10_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset=(PT_TERM == input$xcol & CASE_COUNT>ifelse(is.null(input$casecount),1000,input$casecount) & PT_COUNT>ifelse(is.null(input$ptcount),10,input$ptcount)))
        pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, PRR, L10_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset = (PT_TERM == input$ycol & CASE_COUNT > ifelse(is.null(input$casecount),1000,input$casecount) & PT_COUNT > ifelse(is.null(input$ptcount),10,input$ptcount)))
        merge(pt1,pt2, by="INAME") %>% distinct()
    })
    
    #UNFILTERED data so that select input doesn't clear everytime a restriction is changed
    data_unfilt <- reactive({
        pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PT_TERM, PRR, L10_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset=(PT_TERM == input$xcol))
        pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, PRR, L10_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset = (PT_TERM == input$ycol))
        merge(pt1,pt2, by="INAME") %>% distinct()
    })
    
    
    # data filtered by CLASS
    data_filt <- reactive({
        return(data()[which(atc_col() %in% class()),])
    })
    
    # renders a ggplot of adverse events
    output$scatterPlot <- renderPlot({
        xmin <- min(data()$L10_PRR.x)
        xmax <- max(data()$L10_PRR.x)
        ymin <- min(data()$L10_PRR.y)
        ymax <- max(data()$L10_PRR.y)
        
        
        ggplot(data(), aes(x = L10_PRR.x, y = L10_PRR.y)) + 
            geom_point(color="#392dc2", size = 2) +
            labs(
                x = input$xcol, 
                y = input$ycol,
                title = paste(input$ycol, " vs. ", input$xcol),
                subtitle = "PRR Values are Log-Transformed in Both Axes"
            ) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size=15), axis.title = element_text(size = 13), 
                  plot.subtitle = element_text(hjust = 0.5, size=13), axis.text = element_text(size=14)) + 
            xlim(c(xmin, xmax)) +
            ylim(c(ymin, ymax)) +
            gghighlight(atc_col() %in% class()) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = (max(data()$L10_PRR.x)-min(data()$L10_PRR.x)))) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = (max(data()$L10_PRR.y)-min(data()$L10_PRR.y))))
    })
    
    observeEvent(input$reset_ae, {
        updateSliderInput(session, inputId="casecount",label="Substance Count",min=100,max=50000,value=1000)
        updateSliderInput(session, inputId="ptcount",label="Adverse Event Count",min=5,max=100,value=10)
    })
    
    atc_col <- reactive({
        
        if(!is.null(input$atc4)){
            atc_col = data()$ATC4.x
        }
        else if(!is.null(input$atc3)) atc_col = data()$ATC3.x
        else if(!is.null(input$atc2)) atc_col = data()$ATC2.x
        else if(!is.null(input$atc1)) atc_col = data()$ATC1.x
        else atc_col = data()$ATC1.x
        return (atc_col)
    })
    
    class <- reactive({
        if(!is.null(input$atc4)){
            class = input$atc4
        }
        else if(!is.null(input$atc3)) class = input$atc3
        else if(!is.null(input$atc2)) class = input$atc2
        else if(!is.null(input$atc1)) class = input$atc1
        else class = data()$ATC1.x
        return (class)
    })
    
    
    # returns user-inputted subset of data
    getSubset <- function(){
        pt1_pt2 <- data()
        if(input$alldata == FALSE) pt1_pt2 <- data_filt()
        
        pt1_pt2$PRR.x = round(as.numeric(pt1_pt2$PRR.x), digits = 2)
        pt1_pt2$L10_PRR.x = round(as.numeric(pt1_pt2$L10_PRR.x), digits = 2)
        pt1_pt2$PRR.y = round(as.numeric(pt1_pt2$PRR.y), digits = 2)
        pt1_pt2$L10_PRR.y = round(as.numeric(pt1_pt2$L10_PRR.y), digits = 2)
        
        subset <- subset(pt1_pt2,select=c(-PT_TERM.x,-PT_TERM.y,-CASE_COUNT.y, -L10_PRR.x, -L10_PRR.y, 
                                          -ATC1.y, -ATC2.y, -ATC3.y, -ATC4.y))
        if(nrow(subset) > 1){
            row.names(subset) = 1 : nrow(subset)
        }
        return (subset)
    }
    
    
    ord_subset <- function(){
        subset <- getSubset()
        return (data.frame("Substance" = subset$INAME, "Substance Count"=subset$CASE_COUNT.x, 
                           "count1"=subset$PT_COUNT.x, "prr1"=subset$PRR.x, 
                           "count2"=subset$PT_COUNT.y, "prr2"=subset$PRR.y, 
                           "ATC Level 1" = subset$ATC1.x, "ATC Level 2" = subset$ATC2.x, 
                           "ATC Level 3" = subset$ATC3.x, "ATC Level 4" = subset$ATC4.x))
    }
    
    # renders datatable of subsetted data
    output$table1<-renderDataTable({
        x = input$xcol
        y = input$ycol
        
        
        dt <- datatable(ord_subset(), 
                        selection = "single", 
                        rownames = FALSE,
                        colnames = c("SUBSTANCE", "SUBSTANCE COUNT", paste(x, " COUNT"), paste(x, " PRR"), paste(y, " COUNT"), paste(y, " PRR"),
                                     "ATC LEVEL 1", "ATC LEVEL 2", "ATC LEVEL 3", "ATC LEVEL 4"),
                        options = list(
                            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
                            autoWidth = TRUE,
                            scrollX = TRUE,
                            scrollY = '350px')
        )
    })
    
    
    # calls popModal pop-up on click of table row
    observeEvent(input$table1_rows_selected, {
        rows = input$table1_rows_selected
        iname <- getSubset()$INAME[rows]
        atc1 = getSubset()$ATC1.x[rows]
        atc2 = getSubset()$ATC2.x[rows]
        atc3 = getSubset()$ATC3.x[rows]
        atc4 = getSubset()$ATC4.x[rows]
        popModal(iname, atc1, atc2, atc3, atc4)
    })
    
    
    # creates pop up of 2d and 3d chemical structures for selected substances
    popModal <- function(name, atc1, atc2, atc3, atc4){
        
        #get cid from pubchem
        cidpath <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", name, "/cids/json")
        cid_r <- GET(url = cidpath)
        cid <- content(cid_r)$IdentifierList$CID[[1]]
        
        # if protein, display still protein image
        if(is.null(cid)){
            showModal(modalDialog(
                title = name,
                size="m",
                strong("ATC Classification: "),
                atc1,
                ", ",
                atc2,
                ", ",
                atc3,
                ", ",
                atc4,
                HTML('<img id = "img2" src="https://ginas.ncats.nih.gov/ginas/app/assets/ginas/images/protein.svg", width=565>'),
                footer = tagList(
                    modalButton("Exit")),
                easyClose = TRUE,
            ))
        }
        
        # if not protein, display mol using cid, uses script.js
        else{
            str = paste0('
            <script>lastClicked = ""; </script>
                        <div class="btn-group btn-group-justified">
                            <a class="btn btn-default button-obj" id = "threed" onclick = "display_mol(this.id,', cid, ')" autofocus>3d</a>
                            <a class="btn btn-default button-obj" id = "twod" onclick = "display_smiles(this.id, &#39;', name, '&#39;)">2d</a>
                            <a class="btn btn-default button-obj" id = "cpk" onclick = "display_cpk(this.id)">CPK Color Code</a>
                        </div>
                        <div id="img"></div>
                        <script>
                            document.getElementById("threed").click();
                        </script>')
            showModal(
                modalDialog(
                    title = name,
                    size="m",
                    strong("ATC Classification: "),
                    atc1,
                    ", ",
                    atc2,
                    ", ",
                    atc3,
                    ", ",
                    atc4,
                    tags$br(),
                    tags$br(),
                    HTML(str),
                    easyClose = TRUE,
                    footer = tagList(
                        modalButton("Exit"))
                )
            )
        }
    }
    
    
    # creates popover with 2d structure and name on hover of data point
    output$hover_info <- renderUI({
        
        if(dim(data())[1] == 0){
            return()
        }
        
        hover <- input$plot_hover
        dat <- data()
        point <- nearPoints(dat, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
        
        if (nrow(point) == 0) return(NULL)
        
        iname = point$INAME
        x = round(point$L10_PRR.x, digits = 2)
        y = round(point$L10_PRR.y, digits = 2)
        
        name = iname
        
        
        cidpath <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", name, "/cids/json")
        cid_r <- GET(url = cidpath)
        cid <- content(cid_r)$IdentifierList$CID[[1]]
        
        
        style <- paste0("position:absolute; padding: 5px; pointer-events: none; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", hover$coords_css$x - 95, "px; top:", hover$coords_css$y, "px;")
        
        if(is.null(cid)){
            wellPanel(
                style = style,
                HTML(paste0(" (", x, ", ", y, ")", "</br>", name, '</br>',
                            '<img id = "img2" src="https://ginas.ncats.nih.gov/ginas/app/assets/ginas/images/protein.svg", width=85>'))
            )
        }
        
        else{
            str = paste0(" (", x, ", ", y, ")", "</br>", name, '</br>',
                         "<img src='https://cactus.nci.nih.gov/chemical/structure/", name, "/image' width = 85>")
            wellPanel(
                style = style,
                HTML(str)
            )
        }
        
        
    })
    
    
    
    # calls popModal on click of data point in plot
    output$click_info <- renderUI({
        if(dim(data())[1] == 0){
            return()
        }
        click <- input$plot_click
        dat <- data()
        point <- nearPoints(dat, click, threshold = 7, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        iname = point$INAME
        
        atc1 = point$ATC1.x
        atc2 = point$ATC2.x
        atc3 = point$ATC3.x
        atc4 = point$ATC4.x
        
        popModal(iname, atc1, atc2, atc3, atc4)
    })
    
    
    output$cor1a <- renderText({
        pt1_pt2 <- data_filt()
        paste0("r=", round(cor(pt1_pt2$L10_PRR.x,pt1_pt2$L10_PRR.y),digit=2),", N=",nrow(pt1_pt2),"\n")
        
    })
    
    output$cor1b <- renderText({
        pt1_pt2 <- data()
        paste0("(no class filtering) r=", round(cor(pt1_pt2$L10_PRR.x,pt1_pt2$L10_PRR.y),digit=2)," N=",nrow(pt1_pt2),"\n")
        
    })
    
    output$download_2 <- downloadHandler(
        filename = function(){
            paste(input$ycol, " vs. ", input$xcol, input$downloadType2);
        },
        content = function(file){
            if(input$downloadType2 == ".csv") {
                write.csv(ord_subset(), file, row.names = FALSE)
            } else if(input$downloadType2 == ".json") {
                exportJSON <- toJSON(ord_subset())
                write(exportJSON, file)
            } else if(input$downloadType2 == ".xlsx") {
                write_xlsx(ord_subset(), path=file)
            } else if(input$downloadType2 == ".txt") {
                write.table(ord_subset(), file, row.names=FALSE)
            }
        }
    )
    
    
    # -------------------------------------------------------PAGE 3--------------------------------------------------------
    
    
    #---------------------------------------Compare multiple substances plot----------------------------------------
    
    subs <- reactiveValues(data = data.frame(name = character(), cor = double()));
    
    
    #actual plot
    output$subs_bar <- renderPlotly({
        dat = subs$data %>% distinct()
        shiny::validate(
            need(nrow(dat)>0, "No Data Available")
        )
        if(nrow(dat)>5) margin_size=160
        else margin_size=30
        plot_ly(dat, x = ~reorder(name, -cor), y = ~round(cor, digits = 2), type = "bar", color = ~cor>0, colors = c("#e82a2a", "#1b3fcf"), 
                source = "B", name = "<extra><extra>") %>% 
            layout(
                xaxis = list(title = "Substance"),
                yaxis = list(title = "Pearson's Correlation Coefficient"),
                showlegend = FALSE,
                margin = list(b = margin_size)
            );
    })
    
    
    #rerender
    observeEvent(input$num_ae, {
        rerender_subs();
    })
    observeEvent(input$min_ae, {
        rerender_subs();
    })
    
    # update subs for plot
    rerender_subs <- function(){
        subs$data <- data.frame(name = character(), cor = double());
        for(sub in input$sub2){
            sub1 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L10_PRR),
                           subset=(INAME == input$sub1 & PT_COUNT>input$min_ae))
            sub2 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L10_PRR),
                           subset=(INAME == sub & PT_COUNT>input$min_ae))
            comb = merge(sub1, sub2, by="PT_TERM") %>% distinct()
            if(length(comb$PT_TERM) >= input$min_ae){
                subs$data = rbind(subs$data, data.frame(name = sub, cor = cor(comb$L10_PRR.x, comb$L10_PRR.y)))
            }
        }
    }
    
    #when sub1 changes
    observeEvent(input$sub1, {
        rerender_subs();
    })
    
    #when sub2 changes
    observeEvent(input$sub2, {
        subs_list = input$sub2
        if(dim(subs$data)[1] < length(subs_list)){
            new_sub = tail(subs_list, n=1);
            sub1 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L10_PRR),
                           subset=(INAME == input$sub1 & PT_COUNT>input$min_ae))
            sub2 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L10_PRR),
                           subset=(INAME == new_sub & PT_COUNT>input$min_ae))
            comb = merge(sub1, sub2, by="PT_TERM") %>% distinct()
            if(length(comb$PT_TERM) < input$num_ae){ }
            else{
                subs$data = rbind(subs$data, data.frame(name = new_sub, cor = cor(comb$L10_PRR.x, comb$L10_PRR.y)));
            }
        }
        else{
            del = setdiff(pts$data$name, subs_list);
            subs$data = subs$data[which(subs$data$name != del),];
        }
    })
    
    
    #onclick data
    observeEvent(event_data("plotly_click", source = "B"),  {
        barData = event_data("plotly_click", source = "B");
        sub1 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L10_PRR),
                       subset=(INAME == input$sub1 & PT_COUNT>input$min_ae))
        sub2 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L10_PRR),
                       subset=(INAME == barData$x & PT_COUNT>input$min_ae))
        comb = merge(sub1, sub2, by="PT_TERM") %>% distinct()
        
        comb = comb[,c(1, 2, 6, 4, 8)]
        
        
        output$ae_dt <- renderDataTable({datatable(comb, selection = "none", options = list(
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
            # autoWidth = TRUE,
            # scrollX = TRUE,
            scrollY = '350px')
        )})
        
        output$download_4 <- downloadHandler(
            filename = function(){
                paste(input$sub1, " VS. ", barData$x, input$downloadType4);
            },
            content = function(file){
                if(input$downloadType4 == ".csv") {
                    write.csv(comb, file, row.names = FALSE)
                } else if(input$downloadType4 == ".json") {
                    exportJSON <- toJSON(comb)
                    write(exportJSON, file)
                } else if(input$downloadType4 == ".xlsx") {
                    write_xlsx(comb, path=file)
                } else if(input$downloadType4 == ".txt") {
                    write.table(comb, file, row.names=FALSE)
                }
            }
        )
        
        showModal(modalDialog(
            size = "l",
            title = paste(barData$x, "vs. ", input$sub1, " (r=", round(barData$y, digits=2), ")"),
            div(style="float:right", downloadButton("download_4", "Download")),
            div(style="float:right", selectInput("downloadType4", label=NULL, 
                                                 choices=c("CSV"=".csv", "TXT"=".txt", "XLSX"=".xlsx", "JSON"=".json"), selected=".csv", width=80)),
            
            
            # div(style="display: inline-block; float: right", downloadButton("subs_csv", "CSV")),
            # " ",
            # div(style="display: inline-block; float: right", downloadButton("subs_txt", "TXT")),
            # div(style="display: inline-block; float: right", downloadButton("subs_xlsx", "XLSX")),
            # tags$br(),
            # tags$br(),
            DT::dataTableOutput("ae_dt"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Exit")
            )
        ));
    })
    
    # ---------------------------------------------- page 3; box 1, 2-----------------------------------------------------
    
    output$ptcount2 <- renderUI({
        d1<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L10_PRR),
                   subset=(INAME==input$xcol2))
        d2<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L10_PRR),
                   subset=(INAME==input$ycol2))
        dat <- merge(d1,d2,by="PT_TERM") %>% distinct()
        numericInput(inputId="ptcount2",label="Adverse Event Count",min=max(5, min(dat$PT_COUNT.x)),max=max(dat$PT_COUNT.x),value=10)
    })
    
    output$download_3 <- downloadHandler(
        filename = function(){
            paste(input$ycol2, " vs. ", input$xcol2, ".csv");
        },
        content = function(file){
            if(input$downloadType3 == ".csv") {
                write.csv(getSubset2(), file, row.names = FALSE)
            } else if(input$downloadType3 == ".json") {
                exportJSON <- toJSON(getSubset2())
                write(exportJSON, file)
            } else if(input$downloadType3 == ".xlsx") {
                write_xlsx(getSubset2(), path=file)
            } else if(input$downloadType3 == ".txt") {
                write.table(getSubset2(), file, row.names=FALSE)
            }
        }
    )
    
    #change to >= ?
    data2 <- reactive({
        d1<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L10_PRR),
                   subset=(INAME==input$xcol2 & PT_COUNT>ifelse(is.null(input$ptcount2), 5, input$ptcount2)))
        d2<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L10_PRR),
                   subset=(INAME==input$ycol2 & PT_COUNT>ifelse(is.null(input$ptcount2), 5, input$ptcount2)))
        merge(d1,d2,by="PT_TERM") %>% distinct()
    })
    
    
    output$scatterPlot2 <- renderPlot({
        xmin <- min(data2()$L10_PRR.x)
        xmax <- max(data2()$L10_PRR.x)
        ymin <- min(data2()$L10_PRR.y)
        ymax <- max(data2()$L10_PRR.y)
        
        ggplot(data2(), aes(x = L10_PRR.x, y = L10_PRR.y)) + 
            geom_point(color="#113569", size = 2) +
            labs(
                x = input$xcol2, 
                y = input$ycol2,
                title = paste(input$ycol2, " vs. ", input$xcol2),
                subtitle = "PRR Values are Log-Transformed in Both Axes"
            ) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size=15), plot.subtitle = element_text(hjust = 0.5, size=13),
                  axis.text = element_text(size=14)) +  
            xlim(c(xmin, xmax)) +
            ylim(c(ymin, ymax)) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = (max(data2()$L10_PRR.x)-min(data2()$L10_PRR.x)))) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = (max(data2()$L10_PRR.y)-min(data2()$L10_PRR.y))))
    })
    
    
    # returns user-inputted subset of data, rounded
    getSubset2 <- function(){
        d1_d2 <- data2()
        
        d1_d2$PRR.x = round(as.numeric(d1_d2$PRR.x), digits = 2)
        d1_d2$L10_PRR.x = round(as.numeric(d1_d2$L10_PRR.x), digits = 2)
        d1_d2$PRR.y = round(as.numeric(d1_d2$PRR.y), digits = 2)
        d1_d2$L10_PRR.y = round(as.numeric(d1_d2$L10_PRR.y), digits = 2)
        
        subset <- subset(d1_d2,select=c(-L10_PRR.x, -L10_PRR.y, -PT_TERM.1.x, -PT_TERM.1.y))
        
        if(nrow(subset) > 1){
            row.names(subset) = 1 : nrow(subset)
        }
        return (subset)
    }
    
    
    # renders datatable of subsetted data
    output$table2 <- renderDataTable({
        x = input$xcol2
        y = input$ycol2
        
        datatable(getSubset2(), 
                  selection = "single", 
                  rownames = FALSE,
                  colnames = c("Adverse Event", paste(x, " Count"), paste(x, " PRR"), paste(y, " Count"), paste(y, " PRR")),
                  options = list(
                      lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
                      autoWidth = TRUE,
                      scrollX = TRUE,
                      scrollY = '350px',
                      columnDefs = list(list(width = '5%', targets = list(1))))
        )
    })
    
    output$download2 <- downloadHandler(
        filename = function(){
            "drugs.csv"
        },
        content = function(file){
            write.csv(dset, file, sep=",", row.names = FALSE)
        }
    )
    
    output$cor2 <- renderText({
        d1_d2 <- data2()
        paste0("r=",format(cor(d1_d2$L10_PRR.x,d1_d2$L10_PRR.y),digit=2), ", N=", nrow(d1_d2),"\n")
    })
    
    output$hover_coords <- renderUI({
        if(dim(data2())[1] == 0){
            return()
        }
        
        hover <- input$plot_hover2
        dat <- data2()
        point <- nearPoints(dat, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
        
        if (nrow(point) == 0) return(NULL)
        
        ptterm = point$PT_TERM
        x = round(point$L10_PRR.x, digits = 2)
        y = round(point$L10_PRR.y, digits = 2)
        
        style <- paste0("position:absolute; padding: 5px; pointer-events: none; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", hover$coords_css$x - 95, "px; top:", hover$coords_css$y, "px;")
        str = paste0(" (", x, ", ", y, ")", "</br>", ptterm)
        wellPanel(
            style = style,
            HTML(str)
        )  
    })
    
    observeEvent(input$reset_subs, {
        updateSliderInput(session, inputId="ptcount2",label="Adverse Event Count",min=5,max=100,value=10)
    })
    
    #------------------------------------------------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------- PAGE 4 ----------------------------------------------------------------
    
    class_dat_global <- reactive({
        if(input$class_op=="User-selected drugs"){
            data <- dset[which(dset$INAME %in% input$custom_list),]
        }
        else if(input$class_op=="ATC Class") {
            req(!is.null(input$cc_1))
            if(str_sub(as.character(input$cc_1), 1, 1)=="*") class=str_sub(as.character(input$cc_1), 2)
            else class=input$cc_1
            if(input$cc_level=="1") data<-unique(dset[which(dset$ATC1==class),])
            else if(input$cc_level=="2") data<-unique(dset[which(dset$ATC2==class),])
            else if(input$cc_level=="3") data<-unique(dset[which(dset$ATC3==class),])
            else if(input$cc_level=="4") data<-unique(dset[which(dset$ATC4==class),])
        }
        else{
            req(input$drugs_file)
            infile <- input$drugs_file
            if(tolower(tools::file_ext(infile$datapath)) == "xlsx"){
                tryCatch(
                    {
                        data <- read.xslx(input$drugs_file$datapath,
                                          sheet = 1,
                                          startRow = 1,
                                          cols=1)[,1]
                    },
                    error = function(e) {
                        # return a safeError if a parsing error occurs
                        stop(safeError(e))
                    }
                )
            }
            else if(tolower(tools::file_ext(infile$datapath)) == "txt"){
                tryCatch(
                    {
                        data <- dset[which(dset$INAME %in% readLines(input$drugs_file$datapath)),]
                    },
                    error = function(e) {
                        # return a safeError if a parsing error occurs
                        stop(safeError(e))
                    }
                )
            }
            else{
                tryCatch(
                    {
                        data <- read.csv(input$drugs_file$datapath,
                                         header = FALSE)[, 1]
                    },
                    error = function(e) {
                        # return a safeError if a parsing error occurs
                        stop(safeError(e))
                    }
                )
            }
        }
    })
    
    dat4 <- reactive({
        req(length(class_dat_global()$PRR)>0)
        d1<-class_dat_global()[, c("PT_TERM","PT_COUNT","PRR")]
        
        d1 <- d1 %>%
            group_by(PT_TERM) %>%
            summarise(
                PRR = weighted.mean(PRR, PT_COUNT),
                PT_COUNT = sum(PT_COUNT)
            ) %>%
            as.data.frame()
        
        d2<-subset(dset,select=c(PT_TERM,PT_COUNT,PRR),
                   subset=(INAME==input$cc_2)) # deleted toupper(input$cc_2)
        
        merge(d1,d2,by="PT_TERM") %>% distinct()
    })
    
    observeEvent(input$cc_2, {
        if(!(input$cc_2 %in% dset$INAME)){
            hideElement(id="column1")
            showNotification("Not enough data available", type="error", duration=1)
        }
        else{
            showElement(id="column1")
        }
    })
    
   
    
    output$cc_1 <- renderUI({
        if(length(dset$ATC1[which(dset$INAME==input$cc_2)])>1){
            excl=dset[which(dset$INAME==input$cc_2)[1],]
        }
        else{
            excl=dset[which(dset$INAME==input$cc_2),]
        }
        if(input$cc_level=="2"){
            selectizeInput("cc_1", "Class", c(l2[l2!=excl$ATC2], paste0("*", excl$ATC2)), multiple=FALSE, width="100%")
        }
        else if(input$cc_level=="3"){
            selectizeInput("cc_1", "Class", c(l3[l3!=excl$ATC3], paste0("*", excl$ATC3)), multiple=FALSE, width="100%")
        }
        else if(input$cc_level=="4"){
            selectizeInput("cc_1", "Class", c(l4[l4!=excl$ATC4], paste0("*", excl$ATC4)), multiple=FALSE, width="100%", options = list(maxOptions=600))
        }
        else{
            selectizeInput("cc_1", "Class", c(l1[l1!=excl$ATC1], paste0("*", excl$ATC1)), multiple=FALSE, width="100%")
        }
    })
    
    output$cc_2 <- renderUI({
        selectizeInput("cc_2", "Drug", choices=vars2, width="100%", options=list(maxOptions=12000), selected=vars2[11])
    })
    
    
    #pg 4 box 3: stacked ae bar chart
    output$bar4 <- renderPlotly({
        shiny::validate(
            need(length(class_dat_global()$PRR)>0, "No Data Available")
        )
        
        if(input$sortby4=="Class"){
            plot_ly(dat4(), x=~reorder(PT_TERM, -PRR.x), y=~PRR.x, type="bar", name="Class", 
                    width= 20*nrow(dat4()), source='F') %>%
                add_trace(y=~PRR.y, name=input$cc_2) %>% 
                layout(bargap=.1, yaxis = list(title = 'PRR', showline = TRUE), xaxis = list(title='Adverse Events', tickangle=90, showline = TRUE), 
                       barmode = 'stack', legend = list(x=0), autosize=TRUE, margin = list(l = 20, r = 20, b = 200, t = 10)) %>%
                plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
        }
        else if(input$sortby4==input$cc_2){
            plot_ly(dat4(), x=~reorder(PT_TERM, -PRR.y), y=~PRR.x, type="bar", name=input$cc_2, width= 20*nrow(dat4()), source='F') %>%
                add_trace(y=~PRR.y, name="Class") %>% 
                layout(bargap=.1, yaxis = list(title = 'PRR', showline = TRUE), xaxis = list(title='Adverse Events', tickangle=90, showline = TRUE), 
                       barmode = 'stack', legend = list(x=0), autosize=TRUE, margin = list(l = 20, r = 20, b = 200, t = 10)) %>%
                plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
        }
        
        else{
            plot_ly(dat4(), x=~reorder(PT_TERM, -(PRR.y+PRR.x)), y=~PRR.x, type="bar", name=input$cc_2, width=20*nrow(dat4()), source='F') %>%
                add_trace(y=~PRR.y, name="Class") %>% 
                layout(bargap=.1, yaxis = list(title = 'PRR', showline = TRUE), xaxis = list(title='Adverse Events', tickangle=90, showline = TRUE), 
                       barmode = 'stack', legend = list(x=0), autosize=TRUE, margin = list(l = 20, r = 20, b = 200, t = 10)) %>%
                plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
        }
        
    })
    
    
    output$sortby4 <- renderUI({
        selectizeInput("sortby4", "Sort by", c("Total", "Class", input$cc_2), selected="Total")
    })
    
    
    # pg 4 box 4: drug correlations
    output$drug_cor <- renderPlotly({
        shiny::validate(
            need(length(class_dat_global()$PRR)>0, "No Data Available")
        )
        drug_list <- unique(class_dat_global()$INAME)
        
        df = data.frame(classDrug=character(), cor=double(), stringsAsFactors=FALSE)
        for(i in 1:length(drug_list)){
            drug1 = input$cc_2
            drug2 = drug_list[i]
            
            d1<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L10_PRR),
                       subset=(INAME==drug1))
            d2<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L10_PRR),
                       subset=(INAME==drug2))
            d1_d2 <- merge(d1,d2,by="PT_TERM") %>% distinct()
            
            df = rbind(df, data.frame(classDrug=drug2, cor=cor(d1_d2$PRR.x, d1_d2$PRR.y)))
        }
        if(nrow(df)>5) margin_size=160
        else margin_size=30
        
        
        plot_ly(df, x=~reorder(classDrug, -cor), y=~cor, type="bar", color = ~cor>0, colors = c("#e82a2a", "#1b3fcf"), 
                source = "G", name = "<extra><extra>", hoverinfo='text', text=~paste(classDrug, ",", round(cor, 2)),
                autosize=TRUE, width= max(max(1000, 20*nrow(df)), session$clientData$output_bar4_width)) %>% 
            layout(
                yaxis = list(title = "Pearson&#39;s Correlation Coefficient"),
                xaxis = list(title = "Drug in Selected Class"),
                showlegend = FALSE,
                margin = list(b=margin_size)
            ) %>%
            plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
    })
    
    observeEvent(input$class_op, priority=100, {
        hideElement("atc_div")
        hideElement("cstm")
        hideElement("upload_div")
        if(input$class_op=="ATC Class"){
            showElement("atc_div")
        }
        else if(input$class_op=="User-selected drugs"){
            showElement("cstm")
        }
        else{
            showElement("upload_div")
        }
    })
    
    
    output$boxplots <- renderPlotly({
        dat <- class_dat_global()
        n=length(unique(dat$INAME))
        dat = rbind(dat, dset[which(dset$INAME==input$cc_2),])
        
        if(nrow(dat)>5) margin_size=160
        else margin_size=30
        
        levels <- dat %>%
            group_by(INAME) %>%
            summarise(m = -median(PRR)) %>%
            arrange(m) %>%
            pull(INAME)
        
        plot_ly(dat, x = ~factor(INAME, levels), y=~L10_PRR, type = "box", source='H', width= max(max(20*n, 1000), session$clientData$output_bar4_width), 
                outline=FALSE, hoverinfo="y", boxpoints=FALSE, color = ~INAME==input$cc_2, colors = c("blue", "red")) %>%
            layout(
                margin = list(b=margin_size),
                yaxis = list(title="log-10 PRR"),
                xaxis = list(title = "Drug"),
                showlegend = FALSE
            ) %>%
            plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
    })
    
    
    output$drugperc <- renderDataTable(
        data.frame("PRR"=round(unique(dset[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, input$pcentile_input/100)),])$PRR, 2), "LOG_10"=round(unique(dset[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, input$pcentile_input/100)),])$L10_PRR, 2), "ADVERSE_EVENT"=unique(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, input$pcentile_input/100))])), rownames=FALSE, selection="single", class="compact")
    
    
    
    output$dload4_1 <- downloadHandler(
        filename = function(){
            paste0(input$cc_2, "-related adverse events (", input$pcentile_input, "th percentile of class)", input$downloadType4_1)
        },
        content = function(file){
            if(input$downloadType4_1 == ".csv") {
                write.csv(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .95))], file, row.names = FALSE)
            } else if(input$downloadType4_1 == ".json") {
                exportJSON <- toJSON(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .95))])
                write(exportJSON, file)
            } else if(input$downloadType4_1 == ".xlsx") {
                write_xlsx(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .95))], path=file)
            } else if(input$downloadType4_1 == ".txt") {
                write.table(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .95))], file, row.names=FALSE)
            }
        }
    )
    
    output$dload42 <- downloadHandler(
        filename = function(){
            paste(input$cc_2, "-related adverse events (90th percentile)", input$downloadType42)
        },
        content = function(file){
            if(input$downloadType42 == ".csv") {
                write.csv(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .9))], file, row.names = FALSE)
            } else if(input$downloadType42 == ".json") {
                exportJSON <- toJSON(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .9))])
                write(exportJSON, file)
            } else if(input$downloadType42 == ".xlsx") {
                write_xlsx(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .9))], path=file)
            } else if(input$downloadType42 == ".txt") {
                write.table(dset$PT_TERM[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, .9))], file, row.names=FALSE)
            }
        }
        
    )
    
    x_val <- reactiveVal(NULL)
    
    observeEvent(input$drugperc_rows_selected, ignoreNULL = FALSE, {
        x_val(unique(dset[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, input$pcentile_input/100)), "L10_PRR"])[input$drugperc_rows_selected])
    })
    
    
    vline <- function(x = 0, color = "red") {
        list(
            type = "line", 
            y0 = 0, 
            y1 = 1, 
            yref = "paper",
            x0 = x_val(), 
            x1 = x_val(), 
            line = list(color = color)
        )
    }
    
    output$histogram <- renderPlotly({
        shiny::validate(
            need(length(class_dat_global()$PRR)>0, "No Data Available")
        )

        dat <- summaryBy(L10_PRR ~ PT_TERM, data = class_dat_global(), 
                         FUN = list(median))
        
        plot_ly(dat, type="histogram", x=~L10_PRR.median, marker=list(color="peachpuff"), source="I") %>%
            layout(
                showlegend = FALSE,
                xaxis=list(title="Median PRR")
            ) %>%
            plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE) %>%
            layout(shapes = list(vline(x_val())), title="Frequency of Median PRRs of Class Drugs")
    })
    
    output$drug_name<-renderUI({
        paste(input$cc_2, " - related Adverse Events")
    })
    
    val_upd <- reactiveVal(0)
    pcent_upd <- reactiveVal(0)
    
    output$perc_val <- renderUI({
        numericInput("val", "Value", value=isolate(round(quantile(class_dat_global()$PRR, input$pcentile_input/100), 2)), width="75px", step=.01)
    })
    
    observeEvent(input$pcentile_input, {
        pcent_upd(1)
        if(val_upd()==1) val_upd(0)
        else updateNumericInput(session, "val", value=ifelse(input$pcentile_input/100>1, 0, round(quantile(class_dat_global()$PRR, input$pcentile_input/100), 2)))
    }, ignoreInit = TRUE)
    
    observeEvent(input$val, {
        val_upd(1)
        if(pcent_upd()==1) pcent_upd(0)
        else updateNumericInput(session, "pcentile_input", value=round(100*ecdf(class_dat_global()$PRR) (input$val), 2))
    })
    
    output$perc_count <- renderUI({
        i=input$val
        j=input$pcentile_input # to make it dependent
        dat<-class_dat_global()$PRR
        len <- length(unique(dset[which(dset$INAME==input$cc_2 & dset$PRR>=quantile(class_dat_global()$PRR, input$pcentile_input/100)),])$PT_TERM)
        paste("Count: ", len)
    })
}

