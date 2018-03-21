## -------------------------------------------------------------------
## Chapter 2 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------

library("hemp")

# descriptives for ordinal and nominal scales ----
gender_nominal <- ifelse(interest$gender == 1, "female", "male")
age_nominal <- cut(interest$age,
           breaks = seq(10, 70, by = 10))
age_ordinal <- ordered(age_nominal)
table(gender_nominal)
table(age_ordinal)
ftable(age_ordinal, gender_nominal)
age_table <- table(age_ordinal)
prop.table(age_table)
prop.table(table(age_ordinal, gender_nominal))

# plotting ordinal and nominal data
library("lattice")
barchart(age_ordinal)
dotplot(age_table)
age_gender_table <- table(gender_nominal, age_ordinal)
age_gender_df <- data.frame(age_gender_table)
dotplot(age_ordinal ~ Freq | gender_nominal,
        age_gender_df, xlab = "Frequency", ylab = "Age", col = "black",
        par.settings = list(strip.background = list(col = "white")))

# descriptives and plots for ratio and interval scales ----
summary(interest$vocab)
qqnorm(interest$vocab, ylab = "vocab")
qqline(interest$vocab)

# CTT for SAPA data set ----
num_miss(SAPA)

# split-half reliability
split_half(SAPA, type = "alternate") # every other
set.seed(1)
split_half(SAPA, type = "random") # random

# apply Spearman-Brown correction
split_half(SAPA, type = "alternate", sb = TRUE)

# determine test length
test_length(SAPA, r = .95, r_type = "split")
test_length(SAPA, r = .95, r_type = .862)

# coefficient alpha
coef_alpha(SAPA)

# bootstrapped 95% CIs for coefficient alpha
library("boot")
alpha_fun <- function(data, row){
  coef_alpha(data[row, ])
}
alpha_boot <- boot(SAPA, alpha_fun, R = 1e4)
alpha_boot
plot(alpha_boot)
boot.ci(alpha_boot, type = c("norm", "basic", "perc", "bca"))

# content validity ratio
cvr(N = 20, n_e = 17)

# valdity evidence
cor(interest[, c("vocab", "reading", "sentcomp")])
mod_old <- lm(teacher ~ socdom, interest)
mod_new <- lm(teacher ~ socdom + reading, interest)
summary(mod_new)$r.squared - summary(mod_old)$r.squared
anova(mod_old, mod_new)

# item anlaysis ----

# difficulty
item_diff <- colMeans(SAPA, na.rm = TRUE)
round(item_diff, 3)
total_score <- rowSums(SAPA)

# discrimination
item_discr <- cor(SAPA, total_score,
                  use = "pairwise.complete.obs")
item_discr

# item discrimination index
idi(SAPA, SAPA$reason.4, perc_cut = .27)

# item reliability index
iri(SAPA)
ivi(item = SAPA$reason.4, crit = SAPA$reason.17)

# distractor functioning
distractors <- distract(multiplechoice)
head(distractors)
