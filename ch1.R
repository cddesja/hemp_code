## -------------------------------------------------------------------
## Chapter 1 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------

# basic math ----
2 + 2
6 + 20
89 - 53
424 * 68
1110 / 37
log(10)

# assignment ----
x <- 1
x
X # case-sensitive
x <- c(1, 5, 3, 6, 9)
gender <- c("male", "male", "male", "female", "female")
x_f <- as.factor(x)
gender_f <- as.factor(gender)
y <- c(9, 3, 4)
z <- c(2, 6, 3)
yz <- c(y, z)

# functions ----
log(10, base = 10)
example(log)

# packages ----
install.packages("mirt")
update.packages("mirt")
library("mirt")
library(help = mirt)
ls("package:mirt")
data(package = "mirt")
install.packages("devtools")
library("devtools")

# HEMP package ----
install_github("cddesja/hemp")
library("hemp")

# masking ----
?fixef
lme4::fixef(x)

# working with data ----
library(hemp)
data(interest)
data(HSQ)
ls()
write.csv(interest, file = "interest.csv", row.names = FALSE)
getwd()
interest_new <- read.csv("interest.csv", header = TRUE,
                         stringsAsFactors = FALSE)
library("foreign")
?read.spss
?read.xport
head(rse, 3)
tail(rse)
names(rse)
rse$country
str(rse)
rse$country <- as.character(rse$country)
str(rse$country)
class(rse$country)
rse$id <- 1:nrow(rse)
head(rse)
rse$id <- NULL
rse[2, 4]
rse[2, 3:5]
rse[2, c(3, 4, 5)]
rse[4, "age"]
rse$age[4]
rse[rse$country == "TW", ]
rse[rse$country == "TW" & rse$age < 35, ]
rse[rse$person == 424, "age"]
rse[rse$person == 424, "age"] <- 35
rse[rse$person == 424, "age"]
rse_long <- reshape(data = rse,
                    direction = "long",
                    varying = 1:10,
                    timevar = "question",
                    v.names = "response",
                    idvar = "person")
head(rse_long)
rse_wide <- reshape(data = rse_long,
                    direction = "wide",
                    idvar = "person",
                    timevar = "question",
                    v.names = "response")
rse_long <- rse_long[order(rse_long$person, rse_long$question), ]
head(rse_long)


# descriptive statistics ----
summary(rse)
summary(rse$Q1)
table(rse$country)
mean(rse$Q1)
var(rse$Q1)
sd(rse$Q1)
rse_sub <- subset(rse, select = Q1:Q10)
rse_cor <- cor(rse_sub)
round(rse_cor, 2)
cor.test(rse$Q1, rse$Q2)
rse_gender <- subset(rse, gender == 1 | gender == 2)
t.test(age ~ gender, data = rse_gender)
mod1 <- lm(Q1 ~ Q2 + Q3, data = rse)
names(mod1)
summary(mod1)
pred_values <- predict(mod1)
resid_values <- resid(mod1)

# graphing with base R ----
par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))
plot(vocab ~ mathmtcs, data = interest)
plot(vocab ~ mathmtcs, data = interest, col = gender)
plot(vocab ~ mathmtcs, data = interest, col = gender)
lines(lowess(interest$mathmtcs, interest$vocab))
verbal <- subset(interest, select = c(vocab, reading, sentcomp))
pairs(verbal)
hist(interest$vocab)
boxplot(interest$vocab)
boxplot(interest$vocab ~ interest$gender)
stem(interest$vocab)

# graphing with lattice ----
library("lattice")
xyplot(vocab ~ reading, data = interest)
xyplot(vocab ~ reading | gender, data = interest)
bwtheme <- standard.theme("pdf", color = FALSE)
interest$gender_f <- ifelse(interest$gender == 1, "female", "male")
xyplot(vocab ~ reading | gender_f,
    data = interest, col = "black",
    par.settings = list(strip.background = list(col = "white")))

# install packages used in the book ----
install.packages("boot")
install.packages("difR")
install.packages("equate")
install.packages("faoutlier")
install.packages("GPArotation")
install.packages("lattice")
install.packages("lavaan")
install.packages("lme4")
install.packages("mirt")
install.packages("psych")
install.packages("semPlot")
install.packages("shiny")
install.packages("devtools")
devtools::install_github("cddesja/hemp")

# citating R & packages ----
citation()
citation("lattice")  # cite lattice
toBibtex(citation())
toBibtex(citation("lattice"))
