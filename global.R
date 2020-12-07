library(shinyjs)

load_data <-function() {
  hide('startbutton')
  Sys.sleep(0)
  show('startbutton')
}

#load main dataset
dset <- read.csv("DSET_ALL2.csv")
heat_test <- read.csv("HEAT_DSET.csv")

# page 1
dset_PTs <- read.delim("PTs.txt",header=TRUE)
vars<-dset_PTs$PT_TERM

# page 2
dset_SUBs <- read.csv("SUBs.csv")
vars2<-dset_SUBs$INAME

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

# plotHeight <- 800
# obj <- list("heat"=heat_test, "iris"=iris)

# test heat map matrix

# drugs = c("SOYBEAN OIL",
#           "ALLOPURINOL",
#           "TRIAMTERENE",
#           "FLUOROMETHOLONE",
#           "PHENPROCOUMON",
#           "MAGNESIUM CHLORIDE",
#           "TEMOZOLOMIDE")
# print(drugs)
# min_prr=0
used <- new.env(hash = TRUE)  # map for used pt terms
# used[["hi"]][["no"]]=TRUE
# print(used[["hi"]][["no"]])
mp <- new.env(hash=TRUE)
for(i in 1:nrow(dset)){
  mp[[dset$INAME[i]]][[dset$PT_TERM[i]]]=dset$PRR[i]
}
# print(mp[["TRIAMTERENE"]][["MENIERE'S DISEASE"]])
# pt=c();
# 
# # print(length(unique(dset$PT_TERM)))
# 
# x <- NULL
# ptm <- proc.time()
# num_pt=0;
# for(ptt in unique(dset$PT_TERM)){
#   if(length(used[[ptt]])>0) next;
#   prrs=c()
#   # print(paste("PT", ptt))
#   for(drug in drugs){
#     # print(drug)
#     # print(length(dset$PRR[which(dset$INAME==drug & dset$PT_TERM==ptt)])>0)
#     if(length(mp[[drug]][[ptt]])==0) next;
#     if(mp[[drug]][[ptt]]>=min_prr) prrs=c(prrs, mp[[drug]][[ptt]])
#     else break;
#   }
#   used[[ptt]]=TRUE;
#   # print(length(prrs))
#   if(length(prrs)==length(drugs)){
#     num_pt=num_pt+1;
#     x=rbind(x, prrs);
#     pt=c(pt, ptt);
#   }
# }
# 
# proc.time() - ptm
# 
# mat<-matrix(nrow=num_pt, ncol=length(drugs))
# rownames(mat)=pt
# colnames(mat)=drugs
# print(num_pt)
# print(length(drugs))
# for(i in 1:num_pt){
#   for(j in 1:length(drugs)){
#     mat[i, j]=x[i, j]
#   }
# }
# 
# print(mat)
# 
# # 
# # x <- NULL
# # x=rbind(x, c(3, 4, 2));
# # x=rbind(x, c(3, 1, 1));
# # print(x[1, 2])
# 
# print(length(dset$PT_TERM[which(dset$INAME=="sfsd")]))
# 
