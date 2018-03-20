## ------------------------------------------------------------------------
library("hemp")
library("lavaan")

## ------------------------------------------------------------------------
interest$gender_f <- ifelse(interest$gender == 1, 
                                 "male", "female")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## cog_mod <- 'verb  =~ vocab + reading + sentcomp
##             math =~ mathmtcs + geometry + analyrea'
## 
## cog_fit <- cfa(cog_mod, data = interest)
## fitMeasures(cog_fit, fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
cog_mod <- 'verb  =~ vocab + reading + sentcomp
            math =~ mathmtcs + geometry + analyrea'

cog_fit <- cfa(cog_mod, data = interest)
fitMeasures(cog_fit, fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## configural <- cfa(cog_mod, data = interest, group = "gender_f")
## fitMeasures(configural, fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
configural <- cfa(cog_mod, data = interest, group = "gender_f")
fitMeasures(configural, fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## weak_invariance <- cfa(cog_mod, data = interest,
##                        group = "gender_f",
##                        group.equal = "loadings")
## fitMeasures(weak_invariance,
##             fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
weak_invariance <- cfa(cog_mod, data = interest, group = "gender_f",
                       group.equal = "loadings")
fitMeasures(weak_invariance,
            fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## anova(weak_invariance, configural)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
anova(weak_invariance, configural)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## strong_invariance <- cfa(cog_mod, data = interest,
##                     group = "gender_f",
##                     group.equal = c( "loadings", "intercepts"))
## fitMeasures(strong_invariance,
##             fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
strong_invariance <- cfa(cog_mod, data = interest,
                    group = "gender_f",
                    group.equal = c( "loadings", "intercepts"))
fitMeasures(strong_invariance,
            fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## anova(strong_invariance, weak_invariance)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
anova(strong_invariance, weak_invariance)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## strict_invariance <- cfa(cog_mod, data = interest,
##         group = "gender_f",
##         group.equal = c( "loadings", "intercepts", "residuals"))
## fitMeasures(strict_invariance,
##             fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
strict_invariance <- cfa(cog_mod, data = interest,
        group = "gender_f",
        group.equal = c( "loadings", "intercepts", "residuals"))
fitMeasures(strict_invariance,
            fit.measures = c("cfi", "tli", "rmsea"))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## anova(strict_invariance, strong_invariance)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
anova(strict_invariance, strong_invariance)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## lavTestScore(strong_invariance)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
lavTestScore(strong_invariance)

## ----eval = F------------------------------------------------------------
## strong_invariance_p <- cfa(cog_mod, data = interest,
##                     group = "gender_f",
##                     group.equal = c( "loadings", "intercepts"),
##                     group.partial = "mathmtcs ~ 1")

## ----message = FALSE, verbose = FALSE------------------------------------
VerbAgg <- VerbAggWide[, c(3, 4:27)]
VerbAgg[, 2:25] <- apply(VerbAgg[, 2:25], 2, 
                        function(x) ifelse(x == 0, 0, 1))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## library("difR")
## results_MH <- difMH(Data = VerbAgg, group = "Gender",
##                     focal.name = "F")
## results_MH

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## plot(results_MH)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## results_MH <- difMH(Data = VerbAgg, group = "Gender",
##                     focal.name = "F", purify = TRUE,
##                     p.adjust.method = "BH")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## logistic_fit <- difLogistic(Data = VerbAgg, group = "Gender",
##                       focal.name = "F",  type = "both")
## logistic_fit

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## difLogistic(Data = VerbAgg, group = "Gender", focal.name = "F",
##             type = "udif")
## difLogistic(Data = VerbAgg, group = "Gender", focal.name = "F",
##             type = "nudif")

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## plot(logistic_fit)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## plot(logistic_fit, item = 6, plot = "itemCurve")

## ----message = FALSE, verbose = FALSE, echo = TRUE, eval = FALSE---------
## library("mirt")
## twopl_mod <- "F = 1 - 24"
## twopl_fit <- multipleGroup(data = VerbAgg[, 2:25],
##                            model = twopl_mod, SE = TRUE,
##                            group = VerbAgg$Gender)
## results_irtlr <- DIF(MGmodel = twopl_fit,
##                      which.par = c("a1", "d"),
##                      scheme = "add")
## irtlr_summary(results_irtlr, alpha = 0.05)

## ----eval = T, echo = F, background = "gray95"---------------------------
library("mirt")
twopl_mod <- "F = 1 - 24"
twopl_fit <- multipleGroup(data = VerbAgg[,2:25], 
model = twopl_mod, SE = TRUE, group = VerbAgg$Gender, verbose=FALSE)
results_irtlr <- DIF(MGmodel=twopl_fit, which.par=c('a1', 'd'), scheme = "add")
irtlr_summary(results_irtlr, alpha = .05)

## ----message = FALSE, verbose = FALSE, echo = TRUE, eval = FALSE---------
## DIFitems <- which(colnames(VerbAgg[, 2:25]) %in%
##                   irtlr_summary(results_irtlr, alpha = .05)$Item)
## DIFitems

## ----eval = T, echo = F, background = "gray95"---------------------------
DIFitems <- which(colnames(VerbAgg[,2:25]) %in% irtlr_summary(results_irtlr, alpha = 0.05)$Item)
DIFitems

## ----message = FALSE, verbose = FALSE, echo = TRUE, eval = FALSE---------
## results_irtlr <- DIF(MGmodel = twopl_fit, which.par = "d",
##                      scheme = "add", items2test = DIFitems)
## irtlr_summary(results_irtlr, alpha = .05)

## ----eval = T, echo = F, background = "gray95"---------------------------
results_irtlr <- DIF(MGmodel = twopl_fit, which.par = "d", 
                     scheme = "add", items2test = DIFitems)
irtlr_summary(results_irtlr, alpha = .05)

## ----irtlr1, echo = FALSE, message = FALSE, fig.cap="Item characteristic curves showing uniform DIF in items 14 and 16.", fig.align = "center", fig.height = 4, fig.pos = "!ht"----
plot(twopl_fit, type = "trace", which.items = c(14, 16),
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:2, lwd = 2)),
auto.key=list(points=FALSE,lines=TRUE,columns=2))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(twopl_fit, type = "trace", which.items = c(14, 16),
##      par.settings = simpleTheme(lty = 1:2, lwd = 2),
##      auto.key = list(points = FALSE, lines = TRUE, columns = 2))

