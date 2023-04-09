rm(list=ls())
library(shiny);library(shinydashboard);library(readxl);library(dplyr);library(plyr);library(ggplot2);library(lazyeval)


experiment <- sort(unzip(paste(getwd(),"experiment.zip",sep="/"), list=TRUE)$Name)
control <- sort(unzip(paste(getwd(),"control.zip",sep="/"), list=TRUE)$Name)
experiment <- experiment[24:44]
control <- control[24:44]

letter <- letters[seq( from = 1, to = 20 )]

for (i in 1:20) {
  assign(paste("exp_data",letter[i],sep="_") ,read_xlsx(paste(getwd(),"experiment",experiment[i],sep="/"), range=cell_rows(9:109))[,c(4,6,8,10,12)])
} 

for (i in 1:20) {
  assign(paste("cont_data",letter[i],sep="_") ,read_xlsx(paste(getwd(),"control",control[i],sep="/"), range=cell_rows(9:109))[,c(4,6,8,10,12)])
} 

list_exp <- mget(ls(pattern = "exp_data"))
list_cont <- mget(ls(pattern = "cont_data"))

for (i in 1:20) {
  list_exp[[i]] <- list_exp[[i]][ , order(names(list_exp[[i]]))]
  colnames(list_exp[[i]]) <- c("v1","v2","v3","v4","v5")
}

for (i in 1:20) {
  list_cont[[i]] <- list_cont[[i]][ , order(names(list_cont[[i]]))]
  colnames(list_cont[[i]]) <- c("v1","v2","v3","v4","v5")
}



for (j in 1:100){ 
  
  exp_stack <- 0
  cont_stack <- 0 
  
  for (i in 1:20){
    exp_stack <- rbind(exp_stack,list_exp[[i]][j,])
    cont_stack <- rbind(cont_stack,list_cont[[i]][j,])
  }
  
  exp_stack <- exp_stack[2:21,] 
  cont_stack <- cont_stack[2:21,]
  assign(paste("exp_stack",sprintf("%03d", j),sep="_"), exp_stack) 
  assign(paste("cont_stack",sprintf("%03d", j),sep="_"), cont_stack)
  
}

exp_stack_list = mget(ls(pattern = "exp_stack_"))
cont_stack_list = mget(ls(pattern = "cont_stack_"))

mat <- matrix(NA, nrow=100, ncol=5)

for (i in 1:100){
  
  for (k in 1:5){
    
    diff <- exp_stack_list[[i]][,k] - cont_stack_list[[i]][,k]
    
    if(diff == 0) {
      mat[i,k] <- 0
    }
    
    else {
      
      if(shapiro.test(as.numeric(unlist(diff)))$p.value > 0.05) {
        mat[i,k] <- ifelse(t.test(diff)$p.value < 0.05, 1,0) 
      }
      else {
        mat[i,k] <- ifelse(wilcox.test(unlist(diff))$p.value < 0.05, 1,0)
      }
      
      
    }
    

  }
  
}

getwd()
setwd("/Users/user5/Desktop/code_emg/aa")
  colnames(exp_stack_list[[1]][,1])
for (i in 1:5) {
  jpeg(filename=colnames(exp_stack_list[[1]][,i]),width=300,height=600,unit="px",bg="transparent")
  
  plot(mat[,i])
  abline(v=c(2,10,30,50,60,73,87))
  
  dev.off()
}

