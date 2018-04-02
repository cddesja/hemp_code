## -------------------------------------------------------------------
## Chapter 4 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------

library("hemp")
cognition <- subset(interest, select = vocab:analyrea)

# exploratory data analysis ----
summary(cognition)
apply(cognition, 2, var)
library("lattice")
cognition_l <- reshape(data = cognition,
                       varying = 1:6,
                       v.names = "score",
                       timevar = "indicator",
                       times = names(cognition),
                       direction = "long")
head(cognition_l[order(cognition_l$id), ])

# color version
histogram(~ score | indicator, data = cognition_l)

# textbook version
histogram(~ score | indicator, data = cognition_l, col = "white",
           par.settings = list(strip.background = list(col = c("white"))))

pairs(cognition)
bollen_plot(cognition, crit.value = .06)
cognition[c(202, 53, 111), ]
apply(cognition, 2, min)
apply(cognition, 2, max)
correlations <- cor(cognition)
round(correlations, 3)
cor_diff <- correlations - cor(cognition[-c(202, 53, 101), ])
round(cor_diff, 3)

# eigendecomposition ----
eigen_decomp <- eigen(correlations)
round(eigen_decomp$values, 3)
lattice_scree(cognition)
lattice_pa(cognition)

# exploratory factor analysis ----
one_factor <- factanal(cognition, factors = 1)
one_factor
two_factor <- factanal(cognition, factors = 2)
two_factor

# may need to install GPArotation package
install.packages("GPArotation")
library("GPArotation")
oblimin(loadings(two_factor))

# exploratory factor analysis with categorical data ----
SAPA_subset <- subset(SAPA, select = c(letter.7:letter.58,
                                    rotate.3:rotate.8))
fa.parallel(SAPA_subset, cor = "poly")
SAPA_cor <- polychoric(SAPA_subset)
factanal(covmat = SAPA_cor$rho, factors = 2, n.obs = nrow(SAPA))

# confirmatory factor analysis ----
install.packages("lavaan")
library("lavaan")

# define, fit, and investigate the model
iq_mod <- '
  verb =~ info + comp + arith + simil + digit + vocab
  perf =~ pictcomp + parang + block + object + coding
'
iq_fit <- cfa(iq_mod, data = wiscsem)
inspect(iq_fit)
summary(iq_fit)
unique_elements(iq_fit) - free_params(iq_fit)
summary(iq_fit, standardized = TRUE, fit.measures = TRUE)
fitmeasures(iq_fit)
inspect(iq_fit, "rsquare")
summary(iq_fit, rsquare = TRUE)

iq_mod_nocode <- '
   verb =~ info + comp + arith + simil + digit + vocab
   perf =~ pictcomp + parang + block + object
 '
iq_fit_nocode <- cfa(iq_mod_nocode, wiscsem)
nocode <- fitmeasures(iq_fit_nocode,
                      fit.measures = c("rmsea", "tli", "cfi"))
code <- fitmeasures(iq_fit,
                    fit.measures = c("rmsea", "tli", "cfi"))
rbind(nocode, code)
modind_iq <- modindices(iq_fit_nocode, sort. = TRUE)
modind_iq[modind_iq$mi > 3.84, ]
iq_mod_mi <- '
  verb =~ info + comp + arith + simil + digit + vocab
  perf =~ pictcomp + parang + block + object + comp
'
iq_fit_mi <- cfa(iq_mod_mi, wiscsem)
fitMeasures(iq_fit_mi, c("rmsea", "tli", "cfi", "srmr"))
anova(iq_fit_nocode, iq_fit_mi)
factor_scores <- predict(iq_fit_mi)
head(factor_scores)
fitted(iq_fit_mi)
residuals(iq_fit_mi)

# confirmatory factor analysis categorical data ----
wiscsem_cat <- wiscsem
wiscsem_cat[, ] <- lapply(wiscsem_cat[, ], quart_cut)
wiscsem_ord <- wiscsem_cat
wiscsem_ord[, ] <- lapply(wiscsem_ord[, ], ordered)
iq_fit_ord2 <- cfa(iq_mod, wiscsem_cat,
                   ordered = names(wiscsem_cat))
iq_fit_ord2 <- cfa(iq_mod, wiscsem_cat,
                ordered = c("verb", "info"))
