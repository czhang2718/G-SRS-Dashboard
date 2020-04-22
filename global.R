#load main dataset
dset <- read.csv("DSET_ALL.csv")

# page 1
dset_PTs <- read.delim("PTs.txt",header=TRUE)
vars<-dset_PTs$PT_TERM

# page 2
dset_SUBs <- read.csv("SUBs.csv")
vars2<-dset_SUBs$INAME