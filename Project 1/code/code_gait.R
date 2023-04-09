rm(list=ls())
library("readxl")
getwd()
setwd("C:/Users/HwanKam/Desktop/statistical_consulting/first/activity")

experiment <- sort(unzip(paste(getwd(),"experiment.zip",sep="/"),exdir=getwd(), list=TRUE)$Name)
control <- sort(unzip(paste(getwd(),"control.zip",sep="/"), exdir=getwd(), list=TRUE)$Name)


letter <- letters[seq( from = 1, to = 20 )]

for (i in 1:20) {
  assign(paste("exp_data",letter[i],sep="_") ,read_xlsx(paste(getwd(),experiment[i],sep="/"), range=cell_rows(24:124))[,c(1:9, 13:21,28:29,31:35, 37:38,40:87)])
} 

for (i in 1:20) {
  assign(paste("cont_data",letter[i],sep="_") ,read_xlsx(paste(getwd(),control[i],sep="/"), range=cell_rows(24:124))[,c(1:9, 13:21,28:29,31:35, 37:38,40:87)])
} 



list_exp <- mget(ls(pattern = "exp_data"))
list_cont <- mget(ls(pattern = "cont_data"))



for (j in 1:100){ 
  
  exp_stack <- 0
  cont_stack <- 0 
  
  for (i in 1:20){
    exp_stack <- rbind(exp_stack,list_exp[[i]][j,])
    cont_stack <- rbind(cont_stack,list_cont[[i]][j,])
  }
  
  exp_stack <- exp_stack[2:21,] 
  cont_stack <- cont_stack[2:21,]
  assign(paste("exp_stack", sprintf("%03d", j),sep="_"), exp_stack) 
  assign(paste("cont_stack", sprintf("%03d", j),sep="_"), cont_stack)
  
  ##assign(paste("exp_stack",i,sep="_"), exp_stack) 
  ##assign(paste("cont_stack",i,sep="_"), cont_stack)
  
}

exp_stack_list = mget(ls(pattern = "exp_stack_"))
cont_stack_list = mget(ls(pattern = "cont_stack_"))

lst1 <- lapply(exp_stack_list, function(x) x[order(x$V2),])

mat <- matrix(NA, nrow=100, ncol=74)


for (i in 1:100) {
  
  for (k in 1:74) {
    
    diff <- exp_stack_list[[i]][,k] - cont_stack_list[[i]][,k]
    
    if(diff == 0) {
      mat[i,k] <- 0
    }
    
    else{
      
      if(shapiro.test(as.numeric(unlist(diff)))$p.value > 0.05) {
        mat[i,k] <- ifelse(t.test(diff)$p.value < 0.05, 1,0) 
      }
      
      else {
        mat[i,k] <- ifelse(wilcox.test(unlist(diff))$p.value < 0.05, 1,0)
      }
      
    }
    

    
  }
  
}

for (i in 1:74) {
  jpeg(filename=colnames(exp_stack_list[[1]][,i]),width=300,height=600,unit="px",bg="transparent")
  
  plot(mat[,i])
  abline(v=c(2,10,30,50,60,73,87))
  
  dev.off()
}




