## -------------------------------------------------------------------
## Chapter 11 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------
library("hemp")
library("lavaan")

interest$gender_f <- ifelse(interest$gender == 1,
                                 "male", "female")
cog_mod <- "verb  =~ vocab + reading + sentcomp
            math =~ mathmtcs + geometry + analyrea"

cog_fit <- cfa(cog_mod, data = interest)
fitMeasures(cog_fit, fit.measures = c("cfi", "tli", "rmsea"))

# configural invariance ----
configural <- cfa(cog_mod, data = interest, group = "gender_f")
fitMeasures(configural, fit.measures = c("cfi", "tli", "rmsea"))

# weak invariance ----
weak_invariance <- cfa(cog_mod, data = interest, group = "gender_f",
                       group.equal = "loadings")
fitMeasures(weak_invariance,
            fit.measures = c("cfi", "tli", "rmsea"))
anova(weak_invariance, configural)

# strong invariance ----
strong_invariance <- cfa(cog_mod, data = interest,
                    group = "gender_f",
                    group.equal = c("loadings", "intercepts"))
fitMeasures(strong_invariance,
            fit.measures = c("cfi", "tli", "rmsea"))
anova(strong_invariance, weak_invariance)


# strict invariance ----
strict_invariance <- cfa(cog_mod, data = interest,
        group = "gender_f",
        group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(strict_invariance,
            fit.measures = c("cfi", "tli", "rmsea"))
anova(strict_invariance, strong_invariance)

# assessing partial invariance ----
lavTestScore(strong_invariance)
strong_invariance_p <- cfa(cog_mod, data = interest,
                    group = "gender_f",
                    group.equal = c("loadings", "intercepts"),
                    group.partial = "mathmtcs ~ 1")

# differential item functioning ----
install.packages("difR")
library("difR")
VerbAgg <- VerbAggWide[, c(3, 4:27)]
VerbAgg[, 2:25] <- apply(VerbAgg[, 2:25], 2,
                        function(x) ifelse(x == 0, 0, 1))
results_MH <- difMH(Data = VerbAgg, group = "Gender",
                    focal.name = "F")
results_MH
plot(results_MH)
results_MH <- difMH(Data = VerbAgg, group = "Gender",
                    focal.name = "F", purify = TRUE,
                    p.adjust.method = "BH")

# logistic regression ----
logistic_fit <- difLogistic(Data = VerbAgg, group = "Gender",
                      focal.name = "F",  type = "both")
logistic_fit
difLogistic(Data = VerbAgg, group = "Gender", focal.name = "F",
            type = "udif")
difLogistic(Data = VerbAgg, group = "Gender", focal.name = "F",
            type = "nudif")
plot(logistic_fit)
plot(logistic_fit, item = 6, plot = "itemCurve")

# item response theory likelihood ratio test ----
library("mirt")
twopl_mod <- "F = 1 - 24"
twopl_fit <- multipleGroup(data = VerbAgg[, 2:25],
model = twopl_mod, SE = TRUE, group = VerbAgg$Gender, verbose = FALSE)
results_irtlr <- DIF(MGmodel = twopl_fit, 
                     which.par = c("a1", "d"), scheme = "add")
irtlr_summary(results_irtlr, alpha = .05)
DIFitems <- which(colnames(VerbAgg[, 2:25]) %in%
                    irtlr_summary(results_irtlr, alpha = 0.05)$Item)
DIFitems
results_irtlr <- DIF(MGmodel = twopl_fit, which.par = "d",
                     scheme = "add", items2test = DIFitems)
irtlr_summary(results_irtlr, alpha = .05)
plot(twopl_fit, type = "trace", which.items = c(14, 16),
     par.settings = simpleTheme(lty = 1:2, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 2))