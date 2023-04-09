rm(list=ls())

install.packages("semTools")
install.packages("psych")
install.packages("foreign")
install.packages("processR")
install.packages("ggplot2")
install.packages("psy")
install.packages("ltm")
install.packages("QuantPsyc")

library(foreign)
library(processR)
library(semTools)
library(psych)
library(ggplot2)
library(psy)
library(dplyr)
library(ltm)
library(QuantPsyc)

data<-read.csv("kor_data_20150057.csv", header=T, sep=",")
data$q1_1

##모형 그리기
labels1<-list(X="경계선성격특성", M="폭력 허용도", Y="데이트폭력 가해행동")
moderator1<-list(name="SES", site=list(c("b")))
drawConcept(labels=labels1, moderator = moderator1, nodemode=1, node.pos=list(W=c(1,0.9)))

##데이터 전처리
colnames(data)
n<-length(data$sq1)

dfx <- dplyr::select(data,q1_1_1,q1_1_2,q1_1_3,q1_1_4,q1_1_5,q1_1_6,q1_1_7,q1_1_8,q1_1_9,q1_1_10,q1_1_11,q1_1_13,q1_1_15,q1_1_19,q1_1_21,q1_1_22,q1_1_23,q1_1_24)
dfx[,"q1_1_12"] <- (rep(5,n)-data$q1_1_12)
dfx[,"q1_1_14"] <- (rep(5,n)-data$q1_1_14)
dfx[,"q1_1_16"] <- (rep(5,n)-data$q1_1_16)
dfx[,"q1_1_17"] <- (rep(5,n)-data$q1_1_17)
dfx[,"q1_1_18"] <- (rep(5,n)-data$q1_1_18)
dfx[,"q1_1_20"] <- (rep(5,n)-data$q1_1_20) ## Q1-1에서 12,14,16,17,18,20번은 역코딩

dim(dfx) # 24

x_personal<-rowSums(dfx)/24

dfy1<-dplyr::select(data,q7_1,q7_2,q7_3,q7_4,q7_5,q7_6,q7_7,q7_8,q7_9,q7_10,q7_11)
dfy2<-dplyr::select(data,q7_2_1,q7_2_2,q7_2_3,q7_2_4,q7_2_5,q7_2_6,q7_2_7)
dfy3<-dplyr::select(data,q7_3_1,q7_3_2,q7_3_3,q7_3_4,q7_3_5,q7_3_6,q7_3_7,q7_3_8,q7_3_9,q7_3_10,q7_3_11,q7_3_12)
dfy4<-dplyr::select(data,q7_4_1,q7_4_2,q7_4_3,q7_4_4,q7_4_5,q7_4_6,q7_4_7,q7_4_8,q7_4_9,q7_4_10,q7_4_11)

y_physical<-rowSums(dfy3)/12 #신체적 폭력
y_mental<-rowSums(dfy2)/7 #심리적 폭력
y_sexual<-rowSums(dfy4)/11 #성폭력
y_control<-rowSums(dfy1)/11 #행동통제

dfm<-dplyr::select(data,q6_1,q6_2,q6_3,q6_4,q6_5,q6_6,q6_7,q6_8)
m_violence<-rowSums(dfm)/8 #폭력허용도

w_income<-data$dq8_1 #연봉
w_education<-data$dq4_1 #학력

dfc1<-dplyr::select(data,q2_2_1,q2_2_2,q2_2_3,q2_2_4,q2_2_5,q2_2_6,q2_2_7,q2_2_8,q2_2_9)
dfc2<-dplyr::select(data,q2_1_1,q2_1_2,q2_1_3,q2_1_4,q2_1_5,q2_1_6,q2_1_7,q2_1_8,q2_1_9) 
dfc3<-dplyr::select(data,q1_1,q1_2,q1_3,q1_4,q1_5,q1_6,q1_7,q1_8,q1_9,q1_10) #3,5,8,9,10은 역코딩
dfc3[,"q1_3"] <- (rep(5,n)-data$q1_3)
dfc3[,"q1_5"] <- (rep(5,n)-data$q1_5)
dfc3[,"q1_8"] <- (rep(5,n)-data$q1_8)
dfc3[,"q1_9"] <- (rep(5,n)-data$q1_9)
dfc3[,"q1_10"] <- (rep(5,n)-data$q1_10)

c_parents<-rowSums(dfc1)/9  #부모폭력노출
c_abuse<-rowSums(dfc2)/9 #아동학대경험
c_selfesteem<-rowSums(dfc3)/10  #자아존중감
c_age<-data$dq2_1 #나이
c_job<-data$dq6_1 #직업

vars<-data.frame(cbind(x_personal,m_violence,w_income,w_education,y_control,y_mental,y_physical,y_sexual,c_parents,c_abuse,c_selfesteem,c_age,c_job))

#기술통계

for(j in 1:11){
  print(summary(vars[,j]))
  print(sd(vars[,j]))
}

#상관분석
Corr<-matrix(0,nrow=11,ncol=11)
for(i in 1:11){
  for(j in 1:11){
    Corr[i,j]<-cor(vars[,i],vars[,j])
  }
}
Corr


##신뢰도 분석

descript(dfx)['alpha'] #alpha=0.8619, q1_1_12 제거시 alpha 가장 많이 오름
cronbach(dfx[,-12]) #유의미하게 오르지 않음, 모든 변수 포함
descript(dfy1)['alpha']
descript(dfy2)['alpha']
descript(dfy3)['alpha']
descript(dfy4)['alpha']
descript(dfm)['alpha']

##경로분석

#sink('output.txt')

process(data=vars, y="y_physical",x="x_personal", m="m_violence", w="w_income", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_mental",x="x_personal", m="m_violence", w="w_income", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_sexual",x="x_personal", m="m_violence", w="w_income", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_control",x="x_personal", m="m_violence", w="w_income", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_physical",x="x_personal", m="m_violence", w="w_education", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_mental",x="x_personal", m="m_violence", w="w_education", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_sexual",x="x_personal", m="m_violence", w="w_education", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))
process(data=vars, y="y_control",x="x_personal", m="m_violence", w="w_education", model=14, cov=c("c_parents","c_abuse","c_selfesteem","c_age","c_job"))

##표준화계수
lm.beta(lm(m_violence~x_personal+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))

lm.beta(lm(y_physical~x_personal+m_violence+w_income+m_violence:w_income+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
lm.beta(lm(y_mental~x_personal+m_violence+w_income+m_violence:w_income+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
lm.beta(lm(y_sexual~x_personal+m_violence+w_income+m_violence:w_income+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
lm.beta(lm(y_control~x_personal+m_violence+w_income+m_violence:w_income+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))

lm.beta(lm(y_physical~x_personal+m_violence+w_education+m_violence:w_education+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
lm.beta(lm(y_mental~x_personal+m_violence+w_education+m_violence:w_education+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
lm.beta(lm(y_sexual~x_personal+m_violence+w_education+m_violence:w_education+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
lm.beta(lm(y_control~x_personal+m_violence+w_education+m_violence:w_education+c_parents+c_abuse+c_selfesteem+c_age+c_job, data=vars))
