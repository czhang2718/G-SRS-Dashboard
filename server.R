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

function(input, output, session) {
    
    # ------------------------------------------------INTRO PG--------------------------------------------------
    
    #right-side horizontal bar chart
    output$top_ae <- renderPlotly({
        aes = dset[which(dset$INAME == input$intro_drug), c(3, 7, 11)];
        validate(
            need(nrow(aes)>0, "No Data Available")
        )
        if(input$sort_by == "PT COUNT"){
            plot_ly(aes, x=~PT_COUNT, y=~reorder(PT_TERM, PT_COUNT), type = "bar", orientation = "h", height = 520 + 10*nrow(aes), 
                    color = I("orange"), source = "C") %>%
                layout(showlegend = FALSE, autosize=FALSE, yaxis = list(title="", automargin = TRUE), xaxis = list(automargin=TRUE)) %>%
                plotly::config(displaylogo = FALSE)
        }
        else {
            plot_ly(aes, x=~PRR, y=~reorder(PT_TERM, PRR), type = "bar", orientation = "h", height = 520 + 10*nrow(aes), 
                    source = "D") %>%
                layout(showlegend = FALSE, autosize=FALSE, yaxis = list(title="", automargin = TRUE)) %>%
                plotly::config(displaylogo = FALSE)
        }
    })
    
    #pie
    output$pie_chart <- renderPlotly({
        total_rows = nrow(dset[which(dset$INAME==input$intro_drug),])
        validate(
            need(total_rows>0, "No Data Available")
        )
        g1 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR<1),])
        g2 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=1 & dset$PRR<5),])
        g3 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=5 & dset$PRR<10),])
        g4 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=10 & dset$PRR<100),])
        g5 = nrow(dset[which(dset$INAME==input$intro_drug & dset$PRR>=100),])
        df <- data.frame("label"=c("<1", "1-5", "5-10", "10-100", ">100"), "vals" = c(g1, g2, g3, g4, g5))
        plot_ly(df, labels=~label, values=~vals, type="pie", source="E") %>%
            plotly::config(displaylogo = FALSE)
    })
    
    #summary table
    output$sum_table <- renderDataTable({
        
        #mean, median, standard deviation, count, iqr
        dat1 <- dset$PRR[which(dset$INAME==input$intro_drug)]
        mean_prr = round(mean(dat1), 2)
        median_prr = round(median(dat1), 2)
        sd_prr = round(sd(dat1), 2)
        iqr_prr = round(IQR(dat1), 2)
        
        dat2 <- dset$PT_COUNT[which(dset$INAME==input$intro_drug)]
        mean_count = round(mean(dat2), 2)
        median_count = round(median(dat2), 2)
        sd_count = round(sd(dat2), 2)
        iqr_count = round(IQR(dat2), 2)
        df <- cbind(data.frame("1" =c("PRR", "Count"), "2"=c(mean_prr, mean_count), "3"=c(median_prr, median_count),
                    "4" = c(sd_prr, sd_count), "5"=c(iqr_prr, iqr_count)));
        colnames(df) = c(input$intro_drug, "Mean", "Median", "Standard Deviation", "Interquartile Range")
        
        dt <- datatable(df, rownames = FALSE, options = list(dom = 't', autoWidth=F, scrollX=T))
        dt
    })
    
    # -------------------------------------------------PAGE 2---------------------------------------------------
    
    pts <- reactiveValues(data = data.frame(name = character(), cor = double()));
    pts_temp <- reactiveValues(data = data.frame(name = character(), cor = double()));
    atcs <- reactiveValues(data = data.frame(atc1 = character(), atc2 = character(), atc3 = character(), atc4 = character()));
    curr_level <- reactiveVal(value = 0);
    
    #--------------------------------------------dt on plotly_click---------------------------------------------
    observeEvent(event_data("plotly_click", source = "A"),  {
        barData = event_data("plotly_click", source = "A");
        pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PRR, ATC1, ATC2, ATC3, ATC4),
                      subset=(PT_TERM == input$ae1 & CASE_COUNT>input$casecount_box1 & PT_COUNT>input$ptcount_box1))
        pt2 <- subset(dset, select = c(INAME, PT_COUNT, PRR),
                      subset = (PT_TERM == toupper(barData$x) & CASE_COUNT>input$casecount_box1 & PT_COUNT>input$ptcount_box1))
        comb = merge(pt1, pt2, by="INAME") %>% distinct()
        comb = comb[, c(1, 2, 3, 9, 4, 10, 5, 6, 7, 8)];
        
        output$drugs_dt <- renderDataTable({datatable(comb, selection = "none", options = list(
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
            autoWidth = TRUE,
            scrollX = TRUE,
            scrollY = '350px')
            )})
        
        output$mult_comp_csv<- downloadHandler(
            filename = function(){
                paste(input$ae1, " VS. ", barData$x, ".csv");
            },
            content = function(file){
                write.csv(comb, file, row.names=FALSE)
            }
        )
        output$mult_comp_txt <- downloadHandler(
            filename = function(){
                paste(input$ae1, " VS. ", barData$x, ".txt");
            },
            content = function(file){
                write.table(comb, file, row.names=FALSE)
            }
        )
        output$mult_comp_xlsx <- downloadHandler(
            filename = function(){
                paste(input$ae1, " VS. ", barData$x, ".xlsx");
            },
            content = function(file){
                write_xlsx(comb, path=file)
            }
        )
        
        showModal(modalDialog(
            size = "l",
            title = paste(toTitleCase(tolower(barData$x)), "vs. ", toTitleCase(tolower(input$ae1)), " (r=", round(barData$y, digits=2), ")"),
            div(style="display: inline-block; float: right", downloadButton("mult_comp_csv", "CSV")),
            " ",
            div(style="display: inline-block; float: right", downloadButton("mult_comp_txt", "TXT")),
            div(style="display: inline-block; float: right", downloadButton("mult_comp_xlsx", "XLSX")),
            tags$br(),
            tags$br(),
            DT::dataTableOutput("drugs_dt")
        ));
    })
    
    
    
    
    #----------------------------------reactive filter inputs---------------------------------------------
    atc_l1 <- reactive({
        return(toTitleCase(tolower(unique(atcs$data$atc1))));
    })
    output$class1 = renderUI({
        selectInput("class_l1", "Level 1 Class", atc_l1(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    atc_l2 <- reactive({
        dat = atcs$data;
        if(!is.null(input$class_l1)){
            list = dat$atc2[which(dat$atc1 %in% toupper(input$class_l1))]
            if(length(list)>0) return(toTitleCase(tolower(list)))
            else return(list)
        }
    })
    output$class2 = renderUI({
        selectInput("class_l2", "Level 2 Class", atc_l2(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    atc_l3 <- reactive({
        dat = atcs$data;
        if(!is.null(input$class_l2)){
            list = dat$atc3[which(dat$atc2 %in% toupper(input$class_l2))]
            if(length(list)>0) return(toTitleCase(tolower(list)))
            else return(list)
        }
        
    })
    output$class3 = renderUI({
        selectInput("class_l3", "Level 3 Class", atc_l3(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    atc_l4 <- reactive({
        dat = atcs$data;
        if(!is.null(input$class_l3)){
            list = dat$atc4[which(dat$atc3 %in% toupper(input$class_l3))]
            if(length(list)>0) return(toTitleCase(tolower(list)))
            else return(list)
        }
    })
    output$class4 = renderUI({
        selectInput("class_l4", "Level 4 Class", atc_l4(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    #-------------------------------------------update pts$data bc atc filtering----------------------------------------
    
    # helper to render based on all inputs
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
            atc_col = dset$ATC1;
            pt1 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, L2_PRR, ATC1),
                          subset=(PT_TERM == input$ae1 & col %in% toupper(list) & CASE_COUNT>input$casecount_box1 & PT_COUNT>input$ptcount_box1))
            pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, L2_PRR, ATC1),
                          subset = (PT_TERM == ae & col %in% toupper(list) & CASE_COUNT>input$casecount_box1 & PT_COUNT>input$ptcount_box1))
            comb = merge(pt1, pt2, by="INAME") %>% distinct()
            if(length(comb$INAME) >= input$num_obs) {
                pts_temp$data = rbind(pts_temp$data, data.frame(name = ae, cor = cor(comb$L2_PRR.x, comb$L2_PRR.y)));
            }
        }
    }

    
    
    observeEvent(input$class_l1, {
        if(length(input$class_l1) == 0){
            curr_level = 0;
        }
        rerender();
    })
    observeEvent(input$class_l2, {
        if(length(input$class_l2) == 0){
            curr_level = 1;
        }
        rerender();
    })
    observeEvent(input$class_l3, {
        if(length(input$class_l3) == 0){
            curr_level = 2;
        }
        rerender();
    })
    observeEvent(input$class_l4, {
        if(length(input$class_l4) == 0){
            curr_level = 3;
        }
        rerender();
    })
    
    # apply filtering on "filter" button
    observeEvent(input$filt, {
        rerender();
        pts$data = pts_temp$data;
        toggleModal(session, "filter", toggle="close");
    })
    
    
    #-------------------------------------choose substances to comp to funtionality-------------------------------------
    
    # on change, clear input of other_ae
    observeEvent(input$ae1, {
        rerender();
        pts$data = pts_temp$data;
    })
    
    
    observeEvent(input$other_ae, {
        ae_list = input$other_ae
        if(dim(pts$data)[1] < length(ae_list)){
            new_ae = tail(ae_list, n=1);
            pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PT_TERM, L2_PRR, ATC1, ATC2, ATC3, ATC4),
                          subset=(PT_TERM == input$ae1 & CASE_COUNT>input$casecount_box1 & PT_COUNT>input$ptcount_box1))
            pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, L2_PRR, ATC1, ATC2, ATC3, ATC4),
                          subset = (PT_TERM == new_ae & CASE_COUNT>input$casecount_box1 & PT_COUNT>input$ptcount_box1))
            comb = merge(pt1, pt2, by="INAME") %>% distinct()
            if(length(comb$INAME) < input$num_obs){ }
            else{
                pts$data = rbind(pts$data, data.frame(name = new_ae, cor = cor(comb$L2_PRR.x, comb$L2_PRR.y)));
                atcs$data = rbind(atcs$data, data.frame(atc1 = comb$ATC1.x, atc2 = comb$ATC2.x, atc3 = comb$ATC3.x, atc4 = comb$ATC4.x));
            }
        }
        else{
            del = setdiff(pts$data$name, ae_list);
            pts$data = pts$data[which(pts$data$name != del),];
        }
    })
    
    observeEvent(input$num_obs, {
        rerender();
        pts$data = pts_temp$data;
    })
    
    #----------------------------------------------------plot-------------------------------------------------------
    
    output$single_ae1 <- renderPlotly({
        dat = pts$data
        validate(
            need(nrow(dat)>0, "No Data Available")
        )
        plot_ly(dat, x = ~reorder(name, -cor), y = ~round(cor, digits = 3), type = "bar", color = ~cor>0, colors = c("#e82a2a", "#1b3fcf"), 
                source = "A", name = "<extra><extra>") %>% 
            layout(
                yaxis = list(title = "Correlation"),
                xaxis = list(title = "", tickangle = 90),
                showlegend = FALSE
            ) %>%
            plotly::config(displaylogo = FALSE)
    })
    
    #-------------------------------------------------------box 2 and 3; page 2-------------------------------------------------
    
    # CLASS LEVEL 1
    list1 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            return(toTitleCase(tolower(data_unfilt()$ATC1.x)))
        }
    })
    output$list1 = renderUI({
        selectInput("atc1", "Level 1 Class", list1(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    
    # CLASS LEVEL 2
    list2 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat = data_unfilt()
            if(!is.null(input$atc1)){
                dat = dat[which(dat$ATC1.x %in% toupper(input$atc1)),]
            }
            return(toTitleCase(tolower(dat$ATC2.x)))
        }
    })
    output$list2 = renderUI({
        selectInput("atc2", "Level 2 Class", list2(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    
    # CLASS LEVEL 3
    list3 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat = data_unfilt()
            if(!is.null(input$atc2)){
                dat = dat[which(dat$ATC2.x %in% toupper(input$atc2)),]
            }
            return(toTitleCase(tolower(dat$ATC3.x)))
        }
    })
    output$list3 = renderUI({
        selectInput("atc3", "Level 3 Class", list3(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    
    # CLASS LEVEL 4
    list4 <- reactive({
        if(!is.null(input$xcol) && !is.null(input$ycol)){
            dat = data_unfilt()
            if(!is.null(input$atc3)){
                dat = dat[which(dat$ATC3.x %in% toupper(input$atc3)),]
            }
            return(toTitleCase(tolower(dat$ATC4.x)))
        }
    })
    output$list4 = renderUI({
        selectInput("atc4", "Level 4 Class", list4(), multiple=TRUE, selected=NULL, selectize=TRUE)
    })
    
    
    # reactive data based on widgets (case count, pt count)
    data <- reactive({
        pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PT_TERM, PRR, L2_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset=(PT_TERM == input$xcol & CASE_COUNT>input$casecount & PT_COUNT>input$ptcount))
        pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, PRR, L2_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset = (PT_TERM == input$ycol & CASE_COUNT > input$casecount & PT_COUNT > input$ptcount))
        merge(pt1,pt2, by="INAME") %>% distinct()
    })
    
    #UNFILTERED data so that select input doesn't clear everytime a restriction is changed
    data_unfilt <- reactive({
        pt1 <- subset(dset, select=c(INAME,CASE_COUNT, PT_COUNT, PT_TERM, PRR, L2_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset=(PT_TERM == input$xcol))
        pt2 <- subset(dset, select = c(INAME, CASE_COUNT, PT_COUNT, PT_TERM, PRR, L2_PRR, ATC1, ATC2, ATC3, ATC4),
                      subset = (PT_TERM == input$ycol))
        merge(pt1,pt2, by="INAME") %>% distinct()
    })
    
    
    # data filtered by CLASS
    data_filt <- reactive({
        return(data()[which(toupper(atc_col()) %in% toupper(class())),])
    })
    
    # renders a ggplot of adverse events
    output$scatterPlot <- renderPlot({
        xmin <- min(data()$L2_PRR.x)
        xmax <- max(data()$L2_PRR.x)
        ymin <- min(data()$L2_PRR.y)
        ymax <- max(data()$L2_PRR.y)
        
        
        
        ggplot(data(), aes(x = L2_PRR.x, y = L2_PRR.y)) + 
            geom_point(color="#392dc2", size = 2) +
            labs(
                x = input$xcol, 
                y = input$ycol,
                title = paste(input$ycol, " vs. ", input$xcol),
                subtitle = "PRR Values are Log-Transformed in Both Axes"
            ) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size=15), axis.title = element_text(size = 13), plot.subtitle = element_text(hjust = 0.5, size=13)) + 
            xlim(c(xmin, xmax)) +
            ylim(c(ymin, ymax)) +
            gghighlight(toupper(atc_col()) %in% toupper(class()))
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
        pt1_pt2$L2_PRR.x = round(as.numeric(pt1_pt2$L2_PRR.x), digits = 2)
        pt1_pt2$PRR.y = round(as.numeric(pt1_pt2$PRR.y), digits = 2)
        pt1_pt2$L2_PRR.y = round(as.numeric(pt1_pt2$L2_PRR.y), digits = 2)
        
        subset <- subset(pt1_pt2,select=c(-PT_TERM.x,-PT_TERM.y,-CASE_COUNT.y, -L2_PRR.x, -L2_PRR.y, 
                                          -ATC1.y, -ATC2.y, -ATC3.y, -ATC4.y))
        if(nrow(subset) > 1){
            row.names(subset) = 1 : nrow(subset)
        }
        return (subset)
    }
    
    
    ord_subset <- function(){
        subset <- getSubset()
        return (data.frame("Substance" = toTitleCase(tolower(subset$INAME)), "Substance Count"=subset$CASE_COUNT.x, 
                                 "count1"=subset$PT_COUNT.x, "prr1"=subset$PRR.x, 
                                 "count2"=subset$PT_COUNT.y, "prr2"=subset$PRR.y, 
                                 "ATC Level 1" = toTitleCase(tolower(subset$ATC1.x)), "ATC Level 2" = toTitleCase(tolower(subset$ATC2.x)), 
                                 "ATC Level 3" = toTitleCase(tolower(subset$ATC3.x)), "ATC Level 4" = toTitleCase(tolower(subset$ATC4.x))))
    }
    
    # renders datatable of subsetted data
    output$table1<-renderDataTable({
        x = toTitleCase(tolower(input$xcol))
        y = toTitleCase(tolower(input$ycol))
        
        
        dt <- datatable(ord_subset(), 
                        selection = "single", 
                        rownames = FALSE,
                        colnames = c("Substance", "Substance Count", paste(x, " Count"), paste(x, " PRR"), paste(y, " Count"), paste(y, " PRR"),
                                     "ATC Level 1", "ATC Level 2", "ATC Level 3", "ATC Level 4"),
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
        name = paste(substring(iname, 1, 1), tolower(substring(iname, 2, nchar(toString(iname)))), sep="")
        popModal(name, atc1, atc2, atc3, atc4)
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
                toTitleCase(tolower(atc1)),
                ", ",
                toTitleCase(tolower(atc2)),
                ", ",
                toTitleCase(tolower(atc3)),
                ", ",
                toTitleCase(tolower(atc4)),
                HTML('<img id = "img2" src="https://ginas.ncats.nih.gov/ginas/app/assets/ginas/images/protein.svg", width=565>'),
                footer = tagList(
                    modalButton("Exit"))
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
                    toTitleCase(tolower(atc1)),
                    ", ",
                    toTitleCase(tolower(atc2)),
                    ", ",
                    toTitleCase(tolower(atc3)),
                    ", ",
                    toTitleCase(tolower(atc4)),
                    tags$br(),
                    tags$br(),
                    HTML(str),
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
        x = round(point$L2_PRR.x, digits = 2)
        y = round(point$L2_PRR.y, digits = 2)
        
        name = paste(substring(iname, 1, 1), tolower(substring(iname, 2, nchar(toString(iname)))), sep="")
        
        
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
        
        name = paste(substring(iname, 1, 1), tolower(substring(iname, 2, nchar(toString(iname)))), sep="")
        popModal(name, atc1, atc2, atc3, atc4)
    })
    
    
    output$cor1a <- renderText({
        pt1_pt2 <- data_filt()
        paste0(" ", "Correlation (filtered data)=", format(cor(pt1_pt2$L2_PRR.x,pt1_pt2$L2_PRR.y),digit=4),","," N =",nrow(pt1_pt2),"\n")
        
    })
    
    output$cor1b <- renderText({
        pt1_pt2 <- data()
        paste("Correlation (unfiltered data)=", format(cor(pt1_pt2$L2_PRR.x,pt1_pt2$L2_PRR.y),digit=4),","," N =",nrow(pt1_pt2),"\n")
        
    })
    
    
    output$download_csv <- downloadHandler(
        filename = function(){
            paste(input$ycol, " vs. ", input$xcol, ".csv");
        },
        content = function(file){
            write.csv(ord_subset(), file, row.names=FALSE)
        }
    )
    output$download_txt <- downloadHandler(
        filename = function(){
            paste(input$ycol, " vs. ", input$xcol, ".txt");
        },
        content = function(file){
            write.table(ord_subset(), file, row.names=FALSE)
        }
    )
    output$download_xlsx <- downloadHandler(
        filename = function(){
            paste(input$ycol, " vs. ", input$xcol, ".xlsx"); 
        },
        content = function(file){
            write_xlsx(ord_subset(), path=file)
        }
    )
    

    
    # -------------------------------------------------------PAGE 3--------------------------------------------------------
    
    
    #---------------------------------------Compare multiple substances plot----------------------------------------
    
    subs <- reactiveValues(data = data.frame(name = character(), cor = double()));
    
    #actual plot
    
    output$subs_bar <- renderPlotly({
        dat = subs$data;
        plot_ly(dat, x = ~reorder(name, -cor), y = ~cor, type = "bar", color = ~cor>0, colors = c("#e82a2a", "#1b3fcf"), 
                source = "B", name = ~ifelse(cor > 0, "> 0", "<= 0")) %>% 
            layout(
                xaxis = list(title = "Substance"),
                yaxis = list(title = "Correlation"),
                showlegend = FALSE
            );
    })
    
    
    #rerender
    observeEvent(input$num_subs, {
        rerender_subs();
    })
    observeEvent(input$min_ae, {
        rerender_subs();
    })
    
    # update subs for plot
    rerender_subs <- function(){
        subs$data <- data.frame(name = character(), cor = double());
        for(sub in input$sub2){
            sub1 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L2_PRR),
                           subset=(INAME == input$sub1 & PT_COUNT>input$min_ae))
            sub2 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L2_PRR),
                           subset=(INAME == sub & PT_COUNT>input$min_ae))
            comb = merge(sub1, sub2, by="PT_TERM") %>% distinct()
            if(length(comb$PT_TERM) < input$num_subs){ }
            else{
                subs$data = rbind(subs$data, data.frame(name = sub, cor = cor(comb$L2_PRR.x, comb$L2_PRR.y)));
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
            sub1 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L2_PRR),
                          subset=(INAME == input$sub1 & PT_COUNT>input$min_ae))
            sub2 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L2_PRR),
                           subset=(INAME == new_sub & PT_COUNT>input$min_ae))
            comb = merge(sub1, sub2, by="PT_TERM") %>% distinct()
            if(length(comb$PT_TERM) < input$num_subs){ }
            else{
                subs$data = rbind(subs$data, data.frame(name = new_sub, cor = cor(comb$L2_PRR.x, comb$L2_PRR.y)));
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
        sub1 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L2_PRR),
                       subset=(INAME == input$sub1 & PT_COUNT>input$min_ae))
        sub2 <- subset(dset, select=c(PT_TERM, PT_COUNT, PT_TERM, PRR, L2_PRR),
                       subset=(INAME == barData$x & PT_COUNT>input$min_ae))
        comb = merge(sub1, sub2, by="PT_TERM") %>% distinct()
        
        comb = comb[,c(1, 2, 6, 4, 8)]
        
        
        output$ae_dt <- renderDataTable({datatable(comb, selection = "none", options = list(
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
            # autoWidth = TRUE,
            # scrollX = TRUE,
            scrollY = '350px')
        )})
        
        output$subs_csv<- downloadHandler(
            filename = function(){
                paste(input$sub1, " VS. ", barData$x, ".csv");
            },
            content = function(file){
                write.csv(comb, file, row.names=FALSE)
            }
        )
        output$subs_txt <- downloadHandler(
            filename = function(){
                paste(input$sub1, " VS. ", barData$x, ".txt");
            },
            content = function(file){
                write.table(comb, file, row.names=FALSE)
            }
        )
        output$subs_xlsx <- downloadHandler(
            filename = function(){
                paste(input$sub1, " VS. ", barData$x, ".xlsx");
            },
            content = function(file){
                write_xlsx(comb, path=file)
            }
        )
        
        showModal(modalDialog(
            size = "l",
            title = paste(toTitleCase(tolower(barData$x)), "vs. ", toTitleCase(tolower(input$sub1)), " (r=", round(barData$y, digits=2), ")"),
            div(style="display: inline-block; float: right", downloadButton("subs_csv", "CSV")),
            " ",
            div(style="display: inline-block; float: right", downloadButton("subs_txt", "TXT")),
            div(style="display: inline-block; float: right", downloadButton("subs_xlsx", "XLSX")),
            tags$br(),
            tags$br(),
            DT::dataTableOutput("ae_dt")
        ));
    })
    
    # ---------------------------------------------- box 2, 3-----------------------------------------------------
    output$download_csv2 <- downloadHandler(
        filename = function(){
            paste(input$ycol2, " vs. ", input$xcol2, ".csv");
        },
        content = function(file){
            write.csv(getSubset2(), file, row.names=FALSE)
        }
    )
    output$download_txt2 <- downloadHandler(
        filename = function(){
            paste(input$ycol2, " vs. ", input$xcol2, ".txt");
        },
        content = function(file){
            write.table(getSubset2(), file, row.names=FALSE)
        }
    )
    output$download_xlsx2 <- downloadHandler(
        filename = function(){
            paste(input$ycol2, " vs. ", input$xcol2, ".xlsx");
        },
        content = function(file){
            write_xlsx(getSubset2(), path=file)
        }
    )
    
    #change to >= ?
    data2 <- reactive({
        d1<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L2_PRR),
                   subset=(INAME==input$xcol2 & PT_COUNT>input$ptcount2))
        d2<-subset(dset,select=c(PT_TERM,PT_COUNT,PT_TERM,PRR,L2_PRR),
                   subset=(INAME==input$ycol2 & PT_COUNT>input$ptcount2))
        merge(d1,d2,by="PT_TERM") %>% distinct()
    })
    
    
    output$scatterPlot2 <- renderPlot({
        xmin <- min(data2()$L2_PRR.x)
        xmax <- max(data2()$L2_PRR.x)
        ymin <- min(data2()$L2_PRR.y)
        ymax <- max(data2()$L2_PRR.y)
        
        ggplot(data2(), aes(x = L2_PRR.x, y = L2_PRR.y)) + 
            geom_point(color="#113569", size = 2) +
            labs(
                x = input$xcol2, 
                y = input$ycol2,
                title = paste(input$ycol2, " vs. ", input$xcol2),
                subtitle = "PRR Values are Log-Transformed in Both Axes"
            ) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size=15), plot.subtitle = element_text(hjust = 0.5, size=13)) +  
            xlim(c(xmin, xmax)) +
            ylim(c(ymin, ymax))
    })
    
    
    # returns user-inputted subset of data, rounded
    getSubset2 <- function(){
        d1_d2 <- data2()
        
        d1_d2$PRR.x = round(as.numeric(d1_d2$PRR.x), digits = 2)
        d1_d2$L2_PRR.x = round(as.numeric(d1_d2$L2_PRR.x), digits = 2)
        d1_d2$PRR.y = round(as.numeric(d1_d2$PRR.y), digits = 2)
        d1_d2$L2_PRR.y = round(as.numeric(d1_d2$L2_PRR.y), digits = 2)
        
        subset <- subset(d1_d2,select=c(-L2_PRR.x, -L2_PRR.y, -PT_TERM.1.x, -PT_TERM.1.y))
        
        if(nrow(subset) > 1){
            row.names(subset) = 1 : nrow(subset)
        }
        return (subset)
    }
    
    
    # renders datatable of subsetted data
    output$table2 <- renderDataTable({
        x = toTitleCase(tolower(input$xcol2))
        y = toTitleCase(tolower(input$ycol2))
        
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
        paste("Correlation =",format(cor(d1_d2$L2_PRR.x,d1_d2$L2_PRR.y),digit=4),","," N =",nrow(d1_d2),"\n")
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
        x = round(point$L2_PRR.x, digits = 2)
        y = round(point$L2_PRR.y, digits = 2)
        
        name = paste(substring(ptterm, 1, 1), tolower(substring(ptterm, 2, nchar(toString(ptterm)))), sep="")
        style <- paste0("position:absolute; padding: 5px; pointer-events: none; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", hover$coords_css$x - 95, "px; top:", hover$coords_css$y, "px;")
        str = paste0(" (", x, ", ", y, ")", "</br>", name)
        wellPanel(
            style = style,
            HTML(str)
        )  
    })
    
}
