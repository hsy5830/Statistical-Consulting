data<-read.csv("kor_data_20150057.csv", header=T, sep=",")
labels1<-list(X="경계선성격특성", M="폭력 허용도", Y="데이트폭력 가해행동")
moderator1<-list(name="SES", site=list(c("b")))

##변수 정리
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

sumq11<-data$q1_1_1+data$q1_1_2+data$q1_1_3+data$q1_1_4+data$q1_1_5+data$q1_1_6+data$q1_1_7+data$q1_1_8+data$q1_1_9+data$q1_1_10+
  data$q1_1_11+(rep(5,n)-data$q1_1_12)+data$q1_1_13+(rep(5,n)-data$q1_1_14)+data$q1_1_15+(rep(5,n)-data$q1_1_16)+(rep(5,n)-data$q1_1_17)+(rep(5,n)-data$q1_1_18)+data$q1_1_19+(rep(5,n)-data$q1_1_20)+data$q1_1_21+data$q1_1_22+data$q1_1_23+data$q1_1_24

x_personal<-sumq11/24

dfy1<-dplyr::select(data,q7_1,q7_2,q7_3,q7_4,q7_5,q7_6,q7_7,q7_8,q7_9,q7_10,q7_11)
dfy2<-dplyr::select(data,q7_2_1,q7_2_2,q7_2_3,q7_2_4,q7_2_5,q7_2_6,q7_2_7)
dfy3<-dplyr::select(data,q7_3_1,q7_3_2,q7_3_3,q7_3_4,q7_3_5,q7_3_6,q7_3_7,q7_3_8,q7_3_9,q7_3_10,q7_3_11,q7_3_12)
dfy4<-dplyr::select(data,q7_4_1,q7_4_2,q7_4_3,q7_4_4,q7_4_5,q7_4_6,q7_4_7,q7_4_8,q7_4_9,q7_4_10,q7_4_11)

y_physical<-rowSums(dfy3)/12
y_mental<-rowSums(dfy2)/7
y_sexual<-rowSums(dfy4)/11
y_control<-rowSums(dfy1)/11

dfm<-dplyr::select(data,q6_1,q6_2,q6_3,q6_4,q6_5,q6_6,q6_7,q6_8)
m_violence<-rowSums(dfm)/8

w_income<-data$dq8_1
w_education<-data$dq4_1

##
summary(x_personal)
sd(x_personal)

summary(m_violence)
sd(m_violence)

summary(w_income)
summary(w_education)

summary(y_physical)
summary(y_mental)
summary(y_sexual)
summary(y_control)