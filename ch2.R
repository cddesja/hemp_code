## ----eval=FALSE----------------------------------------------------------
## library("hemp")

## ----eval = TRUE---------------------------------------------------------
gender_nominal <- ifelse(interest$gender == 1, "female", "male")

## ----eval = TRUE---------------------------------------------------------
age_nominal <- cut(interest$age,
           breaks = seq(10, 70, by = 10))
age_ordinal <- ordered(age_nominal)

## ----eval = FALSE--------------------------------------------------------
## table(gender_nominal)

## ----eval = TRUE, echo = FALSE, background = "gray95"--------------------
table(gender_nominal)

## ----eval = FALSE--------------------------------------------------------
## table(age_ordinal)

## ----eval = TRUE, echo = FALSE, background = "gray95"--------------------
table(age_ordinal)

## ----eval = FALSE--------------------------------------------------------
## ftable(age_ordinal, gender_nominal)

## ----eval = TRUE, echo = FALSE, background = "gray95"--------------------
ftable(age_ordinal, gender_nominal)

## ----eval = F------------------------------------------------------------
## age_table <- table(age_ordinal)
## prop.table(age_table)

## ----eval = T, echo = F, background = "gray95"---------------------------
age_table <- table(age_ordinal)
prop.table(age_table)

## ----eval = F------------------------------------------------------------
## prop.table(table(age_ordinal, gender_nominal))

## ----eval = T, echo = F, background = "gray95"---------------------------
prop.table(table(age_ordinal, gender_nominal))

## ----eval=F--------------------------------------------------------------
## library("lattice")
## barchart(age_ordinal)

## ----eval = F------------------------------------------------------------
## dotplot(age_table)

## ----eval = F------------------------------------------------------------
## age_gender_table <- table(gender_nominal, age_ordinal)
## age_gender_df <- data.frame(age_gender_table)
## dotplot(age_ordinal ~ Freq | gender_nominal,
##         age_gender_df, xlab = "Frequency", ylab = "Age")

## ----"dot_plot_age", fig.cap="Dot plot of age by sex of examinees taking the interest inventory.", echo = F----
age_gender_table <- table(gender_nominal, age_ordinal)
age_gender_df <- data.frame(age_gender_table)
dotplot(age_ordinal ~ Freq | gender_nominal,
        age_gender_df, xlab = "Frequency", ylab = "Age", col = "black",
        par.settings = list(strip.background=list(col="white")))

## ----eval = F------------------------------------------------------------
## summary(interest$vocab)

## ----eval = T, echo = F, background = "gray95"---------------------------
summary(interest$vocab)

## ----"qq-vocab", fig.cap="Q--Q plot of the vocab variable in the interest data set."----
qqnorm(interest$vocab, ylab = "vocab")
qqline(interest$vocab)

## ----eval = F------------------------------------------------------------
## num_miss(SAPA)

## ----eval = T, echo = F, background = "gray95"---------------------------
num_miss(SAPA)

## ----eval = F------------------------------------------------------------
## split_half(SAPA, type = "alternate")

## ----eval = T, echo = F, background = "gray95"---------------------------
split_half(SAPA, type = "alternate")

## ----eval = F------------------------------------------------------------
## set.seed(1)
## split_half(SAPA, type = "random")

## ----eval = T, echo = F, background = "gray95"---------------------------
set.seed(1)
split_half(SAPA, type = "random")

## ----eval = F------------------------------------------------------------
## split_half(SAPA, type = "alternate", sb = TRUE)

## ----eval = T, echo = F, background = "gray95"---------------------------
split_half(SAPA, type = "alternate", sb = TRUE)

## ----eval = F------------------------------------------------------------
## test_length(SAPA, r = .95, r_type = "split")

## ----eval = T, echo = F, background = "gray95"---------------------------
test_length(SAPA, r = .95, r_type = "split")

## ----eval = F------------------------------------------------------------
## test_length(SAPA, r = .95, r_type = .862)

## ----eval = T, echo = F, background = "gray95"---------------------------
test_length(SAPA, r = .95, r_type = .862)

## ----eval = F------------------------------------------------------------
## coef_alpha(SAPA)

## ----eval = T, echo = F, background = "gray95"---------------------------
coef_alpha(SAPA)

## ----eval = F------------------------------------------------------------
## library("boot")

## ----echo = F------------------------------------------------------------
library("boot")

## ----cache = F-----------------------------------------------------------
alpha_fun <- function(data, row){
  coef_alpha(data[row, ])}

## ----cache = F, eval = F-------------------------------------------------
## alpha_boot <- boot(SAPA, alpha_fun, R = 1e4)
## alpha_boot

## ----eval = T, echo = F, background = "gray95"---------------------------
alpha_boot <- boot(SAPA, alpha_fun, R = 1e4)
alpha_boot

## ----"bootalpha", cache = F, fig.cap="Empirical distribution for coefficient alpha (n = 10,000)."----
plot(alpha_boot)

## ----eval = F------------------------------------------------------------
## boot.ci(alpha_boot, type = c("norm", "basic", "perc", "bca"))

## ----cache = F, eval = T, echo = F, background = "gray95"----------------
boot.ci(alpha_boot, type = c("norm", "basic", "perc", "bca"))

## ----eval = F------------------------------------------------------------
## cvr(N = 20, n_e = 17)

## ----eval = T, echo = F, background = "gray95"---------------------------
cvr(N = 20, n_e = 17)

## ----eval = F------------------------------------------------------------
## cor(interest[, c("vocab", "reading", "sentcomp")])

## ----eval = T, echo = F, background = "gray95"---------------------------
cor(interest[, c("vocab", "reading", "sentcomp")])

## ----eval = F------------------------------------------------------------
## mod_old <- lm(teacher ~ socdom, interest)
## mod_new <- lm(teacher ~ socdom + reading, interest)

## ----eval = T, echo = F, background = "gray95"---------------------------
mod_old <- lm(teacher ~ socdom, interest)
mod_new <- lm(teacher ~ socdom + reading, interest)

## ----eval = F------------------------------------------------------------
## summary(mod_new)$r.squared - summary(mod_old)$r.squared
## anova(mod_old, mod_new)

## ----eval = T, echo = F, background = "gray95"---------------------------
summary(mod_new)$r.squared - summary(mod_old)$r.squared
anova(mod_old, mod_new)

## ----eval = F------------------------------------------------------------
## item_diff <- colMeans(SAPA, na.rm = TRUE)
## round(item_diff, 3)

## ----eval = T, echo = F, background = "gray95"---------------------------
item_diff <- colMeans(SAPA, na.rm = TRUE)
round(item_diff, 3)

## ----eval = F------------------------------------------------------------
## total_score <- rowSums(SAPA, na.rm = TRUE)
## item_discr <- cor(SAPA, total_score,
##                   use = "pairwise.complete.obs")
## item_discr

## ----eval = T, echo = F, background = "gray95"---------------------------
total_score <- rowSums(SAPA)
item_discr <- cor(SAPA, total_score, 
                  use = "pairwise.complete.obs")
item_discr

## ----eval = F------------------------------------------------------------
## idi(SAPA, SAPA$reason.4, perc_cut = .27)

## ----eval = T, echo = F, background = "gray95"---------------------------
idi(SAPA, SAPA$reason.4, perc_cut = .27)

## ----eval = F------------------------------------------------------------
## iri(SAPA)

## ----eval = T, echo = F, background = "gray95"---------------------------
iri(SAPA)

## ----eval = F------------------------------------------------------------
## ivi(item = SAPA$reason.4, crit = SAPA$reason.17)

## ----eval = T, echo = F, background = "gray95"---------------------------
ivi(item = SAPA$reason.4, crit = SAPA$reason.17)

## ----eval = F------------------------------------------------------------
## distractors <- distract(multiplechoice)
## head(distractors)

## ----eval = T, echo = F, background = "gray95"---------------------------
distractors <- distract(multiplechoice)
head(distractors)

