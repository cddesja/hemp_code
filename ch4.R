## ------------------------------------------------------------------------
library("hemp")
cognition <- subset(interest, select = vocab:analyrea)

## ----eval = F------------------------------------------------------------
## summary(cognition)

## ----eval = T, echo = F, background = "gray95"---------------------------
summary(cognition)

## ----eval = F------------------------------------------------------------
## apply(cognition, 2, var)

## ----eval = T, echo = F, background = "gray95"---------------------------
apply(cognition, 2, var)

## ----eval = F------------------------------------------------------------
## library("lattice")
## cognition_l <- reshape(data = cognition,
##                        varying = 1:6,
##                        v.names = "score",
##                        timevar = "indicator",
##                        times = names(cognition),
##                        direction = "long")
## head(cognition_l[order(cognition_l$id),])

## ----eval = T, echo = F, background = "gray95"---------------------------
library("lattice")
cognition_l <- reshape(data = cognition,
                       varying = 1:6,
                       v.names = "score",
                       timevar = "indicator",
                       times = names(cognition),
                       direction = "long")
head(cognition_l[order(cognition_l$id),])

## ----eval = F------------------------------------------------------------
## histogram(~ score | indicator, data = cognition_l)

## ----"histcog", fig.cap="Histograms of indicators of cognition.", echo = F----
histogram(~ score | indicator, data = cognition_l, col = "white",
           par.settings = list(strip.background = list(col = c("white"))))

## ----"cogscat", fig.cap= "Scatterplot matrix of the cognition measures."----
pairs(cognition)

## ----"bolcog", fig.cap= "Bollen plot of cognition data."-----------------
bollen_plot(cognition, crit.value = .06)

## ----eval = F------------------------------------------------------------
## cognition[c(202, 53, 111),]

## ----eval = T, echo = F, background = "gray95"---------------------------
cognition[c(202, 53, 111),]

## ----eval = F------------------------------------------------------------
## apply(cognition, 2, min)

## ----eval = T, echo = F, background = "gray95"---------------------------
apply(cognition, 2, min)

## ----eval = F------------------------------------------------------------
## apply(cognition, 2, max)

## ----eval = T, echo = F, background = "gray95"---------------------------
apply(cognition, 2, max)

## ----eval = F------------------------------------------------------------
## correlations <- cor(cognition)
## round(correlations, 3)

## ----eval = T, echo = F, background = "gray95"---------------------------
correlations <- cor(cognition)
round(correlations, 3)

## ----eval = F------------------------------------------------------------
## cor_diff <- correlations - cor(cognition[-c(202,53,101),])
## round(cor_diff, 3)

## ----eval = T, echo = F, background = "gray95"---------------------------
cor_diff <- correlations - cor(cognition[-c(202,53,101),])
round(cor_diff, 3)

## ----eval = F------------------------------------------------------------
## eigen_decomp <- eigen(correlations)
## round(eigen_decomp$values, 3)

## ----eval = T, echo = F, background = "gray95"---------------------------
eigen_decomp <- eigen(correlations)
round(eigen_decomp$values, 3)

## ----fig.cap="Scree plot of the cognition dataset."----------------------
lattice_scree(cognition)

## ----results="hide", eval = FALSE, fig.keep="none"-----------------------
## library("psych")
## fa.parallel(cognition, fm = "ml")

## ----fig.cap="Parallel analysis of the cognition dataset."---------------
lattice_pa(cognition)

## ----eval = F------------------------------------------------------------
## one_factor <- factanal(cognition, factors = 1)
## one_factor

## ----eval = T, echo = F, background = "gray95"---------------------------
one_factor <- factanal(cognition, factors = 1)
one_factor

## ----eval = F------------------------------------------------------------
## two_factor <- factanal(cognition, factors = 2)
## two_factor

## ----eval = T, echo = F, background = "gray95"---------------------------
two_factor <- factanal(cognition, factors = 2)
two_factor

## ----eval = F------------------------------------------------------------
## install.packages("GPArotation")
## library("GPArotation")
## oblimin(loadings(two_factor))

## ----eval = T, echo = F, background = "gray95"---------------------------
library("GPArotation")
oblimin(loadings(two_factor))

## ------------------------------------------------------------------------
SAPA_subset <- subset(SAPA, select = c(letter.7:letter.58,
                                    rotate.3:rotate.8))

## ----eval = F------------------------------------------------------------
## fa.parallel(SAPA_subset, cor = "poly")

## ----eval = F------------------------------------------------------------
## SAPA_cor <- polychoric(SAPA_subset)
## factanal(covmat = SAPA_cor$rho, factors = 2, n.obs = nrow(SAPA))

## ----eval = FALSE--------------------------------------------------------
## install.packages("lavaan")
## library("lavaan")

## ------------------------------------------------------------------------
iq_mod <- '
  verb =~ info + comp + arith + simil + digit + vocab
  perf =~ pictcomp + parang + block + object + coding
'

## ------------------------------------------------------------------------
iq_fit <- cfa(iq_mod, data = wiscsem)

## ----eval = F------------------------------------------------------------
## inspect(iq_fit)

## ----eval = T, echo = F, background = "gray95"---------------------------
inspect(iq_fit)

## ----eval = F------------------------------------------------------------
## summary(iq_fit)

## ----eval = T, echo = F, background = "gray95"---------------------------
summary(iq_fit)

## ----eval = F------------------------------------------------------------
## unique_elements(iq_fit) - free_params(iq_fit)

## ----eval = T, echo = F, background = "gray95"---------------------------
unique_elements(iq_fit) - free_params(iq_fit)

## ----eval = FALSE--------------------------------------------------------
## summary(iq_fit, standardized = TRUE, fit.measures = TRUE)

## ----eval = F------------------------------------------------------------
## fitmeasures(iq_fit)

## ----eval = F------------------------------------------------------------
## inspect(iq_fit, "rsquare")

## ----eval = T, echo = F, background = "gray95"---------------------------
inspect(iq_fit, "rsquare")

## ----eval = FALSE--------------------------------------------------------
## summary(iq_fit, rsquare = TRUE)

## ------------------------------------------------------------------------
 iq_mod_nocode <- '
   verb =~ info + comp + arith + simil + digit + vocab
   perf =~ pictcomp + parang + block + object
 '
 iq_fit_nocode <- cfa(iq_mod_nocode, wiscsem)

## ----eval = F------------------------------------------------------------
##  nocode <- fitmeasures(iq_fit_nocode,
##                     fit.measures = c("rmsea", "tli", "cfi"))
##  code <- fitmeasures(iq_fit,
##                     fit.measures = c("rmsea", "tli", "cfi"))
##  rbind(nocode, code)

## ----eval = T, echo = F, background = "gray95"---------------------------
 nocode <- fitmeasures(iq_fit_nocode,
                    fit.measures = c("rmsea", "tli", "cfi"))
 code <- fitmeasures(iq_fit,
                    fit.measures = c("rmsea", "tli", "cfi"))
 rbind(nocode, code)

## ----eval = F------------------------------------------------------------
##  modind_iq <- modindices(iq_fit_nocode, sort. = TRUE)
##  modind_iq[modind_iq$mi > 3.84,]

## ----eval = T, echo = F, background = "gray95"---------------------------
 modind_iq <- modindices(iq_fit_nocode, sort. = TRUE)
 modind_iq[modind_iq$mi > 3.84,]

## ----eval = F------------------------------------------------------------
## iq_mod_mi <- '
##   verb =~ info + comp + arith + simil + digit + vocab
##   perf =~ pictcomp + parang + block + object + comp
## '
## iq_fit_mi <- cfa(iq_mod_mi, wiscsem)
## fitMeasures(iq_fit_mi, c("rmsea", "tli", "cfi", "srmr"))

## ----eval = T, echo = F, background = "gray95"---------------------------
iq_mod_mi <- '
  verb =~ info + comp + arith + simil + digit + vocab
  perf =~ pictcomp + parang + block + object + comp
'
iq_fit_mi <- cfa(iq_mod_mi, wiscsem)
fitMeasures(iq_fit_mi, c("rmsea", "tli", "cfi", "srmr"))

## ----eval = F------------------------------------------------------------
## anova(iq_fit_nocode, iq_fit_mi)

## ----eval = T, echo = F, background = "gray95"---------------------------
anova(iq_fit_nocode, iq_fit_mi)

## ----eval = F------------------------------------------------------------
## factor_scores <- predict(iq_fit_mi)
## head(factor_scores)

## ----eval = T, echo = F, background = "gray95"---------------------------
factor_scores <- predict(iq_fit_mi)
head(factor_scores)

## ----eval = F------------------------------------------------------------
## fitted(iq_fit_mi)

## ----eval = T, echo = F, background = "gray95"---------------------------
fitted(iq_fit_mi)

## ----eval = F------------------------------------------------------------
## residuals(iq_fit_mi)

## ----eval = T, echo = F, background = "gray95"---------------------------
residuals(iq_fit_mi)

## ------------------------------------------------------------------------
wiscsem_cat <- wiscsem
wiscsem_cat[ ,] <- lapply(wiscsem_cat[ ,], quart_cut)

## ------------------------------------------------------------------------
wiscsem_ord <- wiscsem_cat
wiscsem_ord[,] <- lapply(wiscsem_ord[ ,], ordered)

## ----eval = F------------------------------------------------------------
## iq_fit_ord2 <- cfa(iq_mod, wiscsem_cat,
##                    ordered = names(wiscsem_cat))

## ----eval = F------------------------------------------------------------
## iq_fit_ord2 <- cfa(iq_mod, wiscsem_cat,
##                 ordered = c("verb", "info"))

