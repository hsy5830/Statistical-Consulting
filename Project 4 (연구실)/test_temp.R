data <- read.csv("data.csv")

## Data
panto_right_m <- data[which(data$recoder == "pantograph" & data$part == "right" & data$sex == "M"), "avg"]
panto_left_m <- data[which(data$recoder == "pantograph" & data$part == "left" & data$sex == "M"), "avg"]
arcus_right_m <- data[which(data$recoder == "arcus digma" & data$part == "right" & data$sex == "M"), "avg"]
arcus_left_m <- data[which(data$recoder == "arcus digma" & data$part == "left" & data$sex == "M"), "avg"]

panto_right_f <- data[which(data$recoder == "pantograph" & data$part == "right" & data$sex == "F"), "avg"]
panto_left_f <- data[which(data$recoder == "pantograph" & data$part == "left" & data$sex == "F"), "avg"]
arcus_right_f <- data[which(data$recoder == "arcus digma" & data$part == "right" & data$sex == "F"), "avg"]
arcus_left_f <- data[which(data$recoder == "arcus digma" & data$part == "left" & data$sex == "F"), "avg"]


## 정규성 검정
shapiro.test(panto_right_m)
shapiro.test(panto_left_m)
shapiro.test(arcus_right_m)
shapiro.test(arcus_left_m) ###

hist(log(arcus_left_m), breaks = 10, main = "Histogram of left Bennett angle of male(ARCUS digma II)", xlab = "Bennett angle")

shapiro.test(panto_right_f)
shapiro.test(panto_left_f) ###

hist(log(panto_left_f), breaks = 10, main = "Histogram of left Bennett angle of female(Pantograph)", xlab = "Bennett angle")

shapiro.test(arcus_right_f)
shapiro.test(arcus_left_f)


## panto vs. arcus with respect to sex and part
## paired t test for test 1-4
test1 <- function(part, sex){
  x_panto <- data[which(data$recoder == "pantograph" & data$part == part & data$sex == sex), c("avg", "recoder")]
  x_arcus <- data[which(data$recoder == "arcus digma" & data$part == part & data$sex == sex), c("avg", "recoder")]
  
  diff <- x_panto$avg - x_arcus$avg
  ifelse(shapiro.test(diff)$p.value > 0.05,
         print("Normality assumption achieved"),
         print("Normality assumption failed"))
  
  return(t.test(diff))
}

t1 <- test1("right", "F"); t1
t2 <- test1("left", "F"); t2
t3 <- test1("right", "M"); t3
t4 <- test1("left", "M"); t4


## male vs. female with respect to recoder and part
## two sample t test for test 5-8
test2 <- function(recoder, part){
  x_male <- data[which(data$recoder == recoder & data$part == part & data$sex == "M"), c("avg", "sex")]
  x_female <- data[which(data$recoder == recoder & data$part == part & data$sex == "F"), c("avg", "sex")]
  tmp <- rbind(x_male, x_female)
  
  pval <- bartlett.test(log(tmp$avg), tmp$sex)$p.value
  vareq <- ifelse(pval > 0.05, TRUE, FALSE)
  ifelse(vareq,
         print("Normality assumption achieved"),
         print("Normality assumption failed"))

  return(t.test(x_female$avg, x_male$avg, var.equal = vareq))
}

t5 <- test2("pantograph", "right"); t5
t6 <- test2("pantograph", "left"); t6
t7 <- test2("arcus digma", "right"); t7
t8 <- test2("arcus digma", "left"); t8


## right vs. left with respect to sex and recoder
## paired t test for test 9-12
test3 <- function(sex, recoder){
  x_right <- data[which(data$recoder == recoder & data$part == "right" & data$sex == sex), c("avg", "part")]
  x_left <- data[which(data$recoder == recoder & data$part == "left" & data$sex == sex), c("avg", "part")]
  
  diff <- x_right$avg - x_left$avg
  ifelse(shapiro.test(diff)$p.value > 0.05,
         print("Normality assumption achieved"),
         print("Normality assumption failed"))
  
  return(t.test(diff))
}

t9 <- test3("F", "pantograph"); t9
t10 <- test3("F", "arcus digma"); t10
t11 <- test3("M", "pantograph"); t11
t12 <- test3("M", "arcus digma"); t12 ###

# wilcoxon signed rank test for t12 (paired-t)
wilcox.test(arcus_right_m - arcus_left_m, alternative = "two.sided", conf.int = T, level = 0.95, exact = F)
library(psych)
describe(arcus_right_m)
describe(arcus_left_m)

## 남녀 구분하지 않고 합쳐서 기구를 비교한 검정
## paired t test
pooled_test <- function(part){
  x_panto <- data[which(data$recoder == "pantograph" & data$part == part), c("avg", "recoder")]
  x_arcus <- data[which(data$recoder == "arcus digma" & data$part == part), c("avg", "recoder")]
  tmp <- rbind(x_panto, x_arcus)
  
  diff <- x_panto$avg - x_arcus$avg
  ifelse(shapiro.test(diff)$p.value > 0.05,
         print("Normality assumption achieved"),
         print("Normality assumption failed"))
  
  return(t.test(diff))
}

pooled_test("right")
pooled_test("left")


## 좌우 평균 내어서 기구를 비교한 검정
## two sample t test
avg_test <- function(sex){
  panto_right <- data[which(data$recoder == "pantograph" & data$sex == sex & data$part == "right"), "avg"]
  panto_left <- data[which(data$recoder == "pantograph" & data$sex == sex & data$part == "left"), "avg"]
  panto_avg <- panto_right + panto_left
  
  arcus_right <- data[which(data$recoder == "arcus digma" & data$sex == sex & data$part == "right"), "avg"]
  arcus_left <- data[which(data$recoder == "arcus digma" & data$sex == sex & data$part == "left"), "avg"]
  arcus_avg <- arcus_right + arcus_left
  
  diff <- panto_avg - arcus_avg
  ifelse(shapiro.test(diff)$p.value > 0.05,
         print("Normality assumption achieved"),
         print("Normality assumption failed"))
  
  return(t.test(diff))
}

avg_test("F")
avg_test("M")


## Boxplot
library(ggplot2)

# 기구/성별/좌우
ggplot(data, aes(x = recoder, y = avg, fill=sex)) + 
  geom_boxplot() +
  facet_wrap(~part)

# 기구별 비교
ggplot(data, aes(x = recoder, y = avg)) + geom_boxplot()

# 성별 비교
ggplot(data, aes(x = sex, y=avg)) + geom_boxplot()

# 좌우 비교
ggplot(data, aes(x = part, y=avg)) + geom_boxplot()


## Correlation Analysis
library(sjPlot)

# 남여 좌우
# data0 <- as.data.frame(cbind(c(panto_left_m, arcus_left_m), c(panto_right_m, arcus_right_m),
#                              c(panto_left_f, arcus_left_f), c(panto_right_f, arcus_right_f)))
# names(data0) <- c("male_left", "male_right", "female_left", "female_right")
# tab_corr(data0, p.numeric = T)

# 남자 좌우
data0 <- as.data.frame(cbind(c(panto_left_m, arcus_left_m),
                             c(panto_right_m, arcus_right_m)))
names(data0) <- c("male_left", "male_right")
tab_corr(data0, p.numeric = T, triangle = "upper")

# 여자 좌우
data0 <- as.data.frame(cbind(c(panto_left_f, arcus_left_f),
                             c(panto_right_f, arcus_right_f)))
names(data0) <- c("female_left", "female_right")
tab_corr(data0, p.numeric = T, triangle = "upper")

# 남자 기구별
data0 <- as.data.frame(cbind(c(panto_left_m, panto_right_m),
                             c(arcus_left_m, arcus_right_m)))
names(data0) <- c("male_panto", "male_arcus")
tab_corr(data0, p.numeric = T, triangle = "upper")

# 여자 기구별
data0 <- as.data.frame(cbind(c(panto_left_f, panto_right_f),
                             c(arcus_left_f, panto_right_f)))
names(data0) <- c("female_panto", "female_arcus")
tab_corr(data0, p.numeric = T, triangle = "upper")

# 전체 케이스
data0 <- as.data.frame(cbind(panto_right_m, panto_left_m, 
                             arcus_right_m, arcus_left_m,
                             panto_right_f, panto_left_f,
                             arcus_right_f, arcus_left_f))
tab_corr(data0, p.numeric = T, triangle = "upper")
