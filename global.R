#load main dataset
dset <- read.csv("DSET_ALL.csv")

# page 1
dset_PTs <- read.delim("PTs.txt",header=TRUE)
vars<-dset_PTs$PT_TERM

# page 2
dset_SUBs <- read.csv("SUBs.csv")
vars2<-dset_SUBs$INAME

#page 4
l1 <- read.delim("l1.txt", header=TRUE)$c1
l2 <- read.delim("l2.txt", header=TRUE)$c2
l3 <- read.delim("l3.txt", header=TRUE)$c3
l4 <- read.delim("l4.txt", header=TRUE)$c4

# collapse other boxes when a new one is opened
collapseInput <- function(inputId, boxId) {
  tags$script(
    gsub("%s", boxId,
         "$('#%s').closest('.box').on('hidden.bs.collapse', function () {
        if('%s' === 'box1a') col_1a = true;
        else if('%s' === 'box1b') col_1b = true;
        else if('%s' === 'box1c') col_1c = true;
      })
      
      
      $('#%s').closest('.box').on('shown.bs.collapse', function () {
        if('%s' === 'box1a') col_1a = false;
        else if('%s' === 'box1b') col_1b = false;
        else if('%s' === 'box1c') col_1c = false;
        if(!col_1a){
          if('%s' != 'box1a'){
              $('#box1a').closest('.box').find('[data-widget=collapse]').click(); 
              col_1a = true;
            }
        }
        if(!col_1b){
          if('%s' != 'box1b'){
              $('#box1b').closest('.box').find('[data-widget=collapse]').click(); 
              col_1b = true;
            }
        }
        if(!col_1c){
          if('%s' != 'box1c'){
              $('#box1c').closest('.box').find('[data-widget=collapse]').click(); 
              col_1c = true;
            }
        }
      })
      ")
  )
}
