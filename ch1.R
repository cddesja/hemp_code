## ----eval = F------------------------------------------------------------
## 2 + 2

## ----eval = T, echo = F, background = "gray95"---------------------------
2 + 2

## ----eval = F------------------------------------------------------------
## # The following R code calculates the sum of 2 and 2
## 2 + 2

## ----eval = F------------------------------------------------------------
## 6 + 20
## 89 - 53
## 424 * 68
## 1110 / 37

## ----eval = F------------------------------------------------------------
## log(10)

## ----eval = F------------------------------------------------------------
## x <- 1

## ----eval = T, echo = F, background = "gray95"---------------------------
x <- 1

## ----eval = F------------------------------------------------------------
## x

## ----eval = T, echo = F, background = "gray95"---------------------------
x

## ----eval = F------------------------------------------------------------
## X

## ----eval = F------------------------------------------------------------
## x

## ----eval = T, echo = F, background = "gray95"---------------------------
x

## ------------------------------------------------------------------------
x <- c(1, 5, 3, 6, 9)

## ----eval = F------------------------------------------------------------
## gender <- c("male", "male", "male", "female", "female")

## ----eval = F------------------------------------------------------------
## x_f <- as.factor(x)
## gender_f <- as.factor(gender)

## ----eval = F------------------------------------------------------------
## y <- c(9, 3, 4)
## z <- c(2, 6, 3)
## yz <- c(y, z)

## ----eval = F------------------------------------------------------------
## function_name(argument1, argument2, ...)

## ----eval = F------------------------------------------------------------
## log(10, base = 10)

## ----eval = F------------------------------------------------------------
## example(log)

## ----eval=F--------------------------------------------------------------
## install.packages("mirt")

## ----eval = F------------------------------------------------------------
## update.packages("mirt")

## ----eval=F--------------------------------------------------------------
## library("mirt")

## ----eval = F------------------------------------------------------------
## library(help = mirt)

## ----eval = F------------------------------------------------------------
## ls("package:mirt")

## ----eval = F------------------------------------------------------------
## data(package = "mirt")

## ----eval = F------------------------------------------------------------
## install.packages("devtools")
## library("devtools")

## ----eval = F------------------------------------------------------------
## install_github("cddesja/hemp")
## library("hemp")

## ----eval = F------------------------------------------------------------
## ?fixef

## ----eval = F------------------------------------------------------------
## lme4::fixef(x)

## ----eval = F------------------------------------------------------------
## library(hemp)
## data(interest)

## ----eval = F------------------------------------------------------------
## data(HSQ)

## ----echo = F------------------------------------------------------------
data(HSQ)

## ----eval = F------------------------------------------------------------
## ls()

## ------------------------------------------------------------------------
write.csv(interest, file = "interest.csv", row.names = FALSE)

## ----eval = F------------------------------------------------------------
## getwd()

## ----eval = F------------------------------------------------------------
## interest_new <- read.csv("interest.csv", header = TRUE,
##                          stringsAsFactors = FALSE)

## ----eval = F------------------------------------------------------------
## interest_new <- read.csv("C:/Users/User/Desktop/interest.csv",
##                          header = TRUE,
##                          stringsAsFactors = FALSE)

## ----eval = F------------------------------------------------------------
## library("foreign")
## ?read.spss
## ?read.xport

## ----eval = F------------------------------------------------------------
## head(rse, 3)

## ----eval = T, echo = F, background = "gray95"---------------------------
head(rse, 3)

## ----eval = F------------------------------------------------------------
## tail(rse)

## ----eval = F------------------------------------------------------------
## names(rse)

## ----eval = T, echo = F, background = "gray95"---------------------------
names(rse)

## ----eval = F------------------------------------------------------------
## rse$country

## ----eval = F------------------------------------------------------------
## str(rse)

## ----eval = F------------------------------------------------------------
## rse$country <- as.character(rse$country)

## ----eval = F------------------------------------------------------------
## str(rse$country)
## class(rse$country)

## ----eval = F------------------------------------------------------------
## rse$id <- 1:nrow(rse)
## head(rse)

## ----eval----------------------------------------------------------------
rse$id <- NULL

## ----eval = F------------------------------------------------------------
## rse[2, 4]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse[2, 4]

## ----eval = F------------------------------------------------------------
## rse[2, 3:5]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse[2, 3:5]

## ----eval = F------------------------------------------------------------
## rse[2, c(3, 4, 5)]

## ----eval = F------------------------------------------------------------
## rse[4, "age"]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse[4, "age"]

## ----eval = F------------------------------------------------------------
## rse$age[4]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse$age[4]

## ----eval = F------------------------------------------------------------
## rse[rse$country == "TW",]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse[rse$country == "TW",]

## ----eval = F------------------------------------------------------------
## rse[rse$country == "TW" & rse$age < 35,]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse[rse$country == "TW" & rse$age < 35,]

## ----eval = F------------------------------------------------------------
## rse[rse$person == 424, "age"]
## rse[rse$person == 424, "age"] <- 35
## rse[rse$person == 424, "age"]

## ----eval = T, echo = F, background = "gray95"---------------------------
rse[rse$person == 424, "age"]
rse[rse$person == 424, "age"] <- 35
rse[rse$person == 424, "age"]

## ----eval = FALSE--------------------------------------------------------
## rse_long <- reshape(data = rse,
##                     direction = "long",
##                     varying = 1:10,
##                     timevar = "question",
##                     v.names = "response",
##                     idvar = "person")
## head(rse_long)

## ----echo = FALSE, background = "gray95"---------------------------------
rse_long <- reshape(data = rse,
                    direction = "long",
                    varying = 1:10,
                    timevar = "question",
                    v.names = "response",
                    idvar = "person")
head(rse_long)

## ----eval = F------------------------------------------------------------
## rse_wide <- reshape(data = rse_long,
##                     direction = "wide",
##                     idvar = "person",
##                     timevar = "question",
##                     v.names = "response")

## ----eval = F------------------------------------------------------------
## rse_long <- rse_long[order(rse_long$person, rse_long$question),]
## head(rse_long)

## ----echo = F, background = "gray95"-------------------------------------
rse_long <- rse_long[order(rse_long$person, rse_long$question),]
head(rse_long)

## ----eval = F------------------------------------------------------------
## summary(rse)

## ----eval = F------------------------------------------------------------
## summary(rse$Q1)

## ----eval = T, echo = F, background = "gray95"---------------------------
summary(rse$Q1)

## ----eval = F------------------------------------------------------------
## table(rse$country)

## ----eval = T, echo = F, background = "gray95"---------------------------
table(rse$country)

## ----eval = F------------------------------------------------------------
## mean(rse$Q1)
## var(rse$Q1)
## sd(rse$Q1)

## ----eval = T, echo = F, background = "gray95"---------------------------
mean(rse$Q1)
var(rse$Q1)
sd(rse$Q1)

## ----eval = F------------------------------------------------------------
## rse_sub <- subset(rse, select = Q1:Q10)
## rse_cor <- cor(rse_sub)
## round(rse_cor, 2)

## ----eval = T, echo = F, background = "gray95"---------------------------
rse_sub <- subset(rse, select = Q1:Q10)
rse_cor <- cor(rse_sub)
round(rse_cor, 2)

## ----eval = F------------------------------------------------------------
## cor.test(rse$Q1, rse$Q2)

## ----echo = F, background = "gray95"-------------------------------------
cor.test(rse$Q1, rse$Q2)

## ----eval = F------------------------------------------------------------
## rse_gender <- subset(rse, gender == 1 | gender == 2)
## t.test(age ~ gender, data = rse_gender)

## ----eval = F------------------------------------------------------------
## mod1 <- lm(Q1 ~ Q2 + Q3, data = rse)
## names(mod1)
## summary(mod1)

## ----eval = T, echo = F, background = "gray95"---------------------------
mod1 <- lm(Q1 ~ Q2 + Q3, data = rse)
names(mod1)
summary(mod1)

## ----eval = F------------------------------------------------------------
## pred_values <- predict(mod1)
## resid_values <- resid(mod1)

## ----"resid", fig.cap="Default diagnostic plots from a regression plot.", fig.pos= "h!", eval = F----
## par(mfrow = c(2, 2), mar=c(2, 4.1, 2, 2))
## plot(mod1)

## ----eval = F------------------------------------------------------------
## plot(vocab ~ mathmtcs, data = interest)

## ----eval = F------------------------------------------------------------
## plot(vocab ~ mathmtcs, data = interest, col = gender)

## ----eval = F------------------------------------------------------------
## plot(vocab ~ mathmtcs, data = interest, col = gender)
## lines(lowess(interest$mathmtcs, interest$vocab))

## ----scatverb, message = FALSE, echo = TRUE, fig.cap="Scatter plot matrix of verbal measures in the interest data set.", fig.align = "center", fig.pos = "!t"----
verbal <- subset(interest, select = c(vocab, reading, sentcomp))
pairs(verbal)

## ----eval = F------------------------------------------------------------
## hist(interest$vocab)

## ----eval = F------------------------------------------------------------
## boxplot(interest$vocab)

## ----eval = F------------------------------------------------------------
## boxplot(interest$vocab ~ interest$gender)

## ----eval = F------------------------------------------------------------
## stem(interest$vocab)

## ----eval = T, echo = F, background = "gray95"---------------------------
stem(interest$vocab)

## ----eval = F------------------------------------------------------------
## xyplot(vocab ~ reading, data = interest)

## ----eval=FALSE----------------------------------------------------------
## xyplot(vocab ~ reading | gender, data = interest)

## ----vocabreading, message = FALSE, echo = FALSE, fig.cap="Scatter plot of vocab by reading conditional on gender.", fig.align = "center", fig.pos = "!t"----
bwtheme <- standard.theme("pdf", color=FALSE)
interest$gender_f <- ifelse(interest$gender == 1, "female", "male")
xyplot(vocab ~ reading | gender_f, data = interest, col = "black", par.settings = list(strip.background = list(col = "white")))

## ----eval = F------------------------------------------------------------
## install.packages("boot")
## install.packages("difR")
## install.packages("equate")
## install.packages("faoutlier")
## install.packages("GPArotation")
## install.packages("lattice")
## install.packages("lavaan")
## install.packages("lme4")
## install.packages("mirt")
## install.packages("psych")
## install.packages("semPlot")
## install.packages("shiny")
## install.packages("devtools")
## devtools::install_github("cddesja/hemp")

## ----eval = F------------------------------------------------------------
## citation()

## ----eval = F------------------------------------------------------------
## citation("package name")

## ----eval = F------------------------------------------------------------
## toBibtex(citation())
## toBibtex(citation("package name"))

