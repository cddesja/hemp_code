## ----csem0, message = FALSE, echo = FALSE, fig.cap="The reciprocal relationship between TIF (the solid line) and cSEM (the dashed line).", fig.align = "center", fig.height = 4, fig.pos = "t!"----
model.1PL <- 'F = 1-16
              CONSTRAIN = (1-16, a1)'
results.1PL <- mirt(data=SAPA, model=model.1PL, SE=TRUE, verbose=FALSE)
plot(results.1PL, type = 'infoSE', theta_lim = c(-4,4), main = "",auto.key = list(points=TRUE, lines=TRUE, columns=2, space = "top"),
     par.settings = list(superpose.line = list(col = c("black", "black"), lty = 1:2)))

## ----eval = F------------------------------------------------------------
## install.packages("mirt")
## library("mirt")
## library("hemp")

## ----eval = T------------------------------------------------------------
onepl_mod <- "F = 1 - 16
              CONSTRAIN = (1 - 16, a1)"

## ----message = FALSE, verbose = FALSE, eval=T, echo = T, results='hide'----
onepl_fit <- mirt(data = SAPA, model = onepl_mod, SE = TRUE)
onepl_params <- coef(onepl_fit, IRTpars = TRUE, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, eval = F--------------------------
## onepl_items <- onepl_params$items
## head(onepl_items)

## ----eval = T, echo = F, background = "gray95"---------------------------
onepl_items <- onepl_params$items
head(onepl_items)

## ----eval = F------------------------------------------------------------
## onepl_se <- coef(onepl_fit, printSE = TRUE)
## names(onepl_se)

## ----eval = T, echo = F, background = "gray95"---------------------------
onepl_se <- coef(onepl_fit, printSE = TRUE)
names(onepl_se)

## ------------------------------------------------------------------------
onepl_se$rotate.8

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(onepl_fit, type = "trace", which.items = 1:2)

## ----icc1, message = FALSE, echo = FALSE, fig.cap="Item characteristic curves for items 1 (reason.4) and 2 (reason.16) for the 1PL model.", fig.align = "center", fig.height = 4, fig.pos = "t!"----
plot(onepl_fit, type = "trace", which.items = 1:2, main="", col.line = "black", par.settings = list(strip.background = list(col = "white")))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## itemplot(onepl_fit, type = "trace", item = 1)
## itemplot(onepl_fit, type = "trace", item = 2)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(onepl_fit, type = "infotrace", which.items = c(3, 5))

## ----iif1, message = FALSE, echo = FALSE, fig.cap="Item information functions for items 3 (reason.17) and 5 (letter.7) for the 1PL model.", fig.align = "center", fig.pos = "t!"----
plot(onepl_fit, type = "infotrace", which.items = c(3, 5), main="", col.line = "black", par.settings = list(strip.background = list(col = "white")))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(onepl_fit, type = "trace", which.items = 1:2,
##      facet_items = FALSE,
##      auto.key = list(points = FALSE, lines = TRUE, columns = 2),
##      par.settings = simpleTheme(lty = 1:2))

## ----icc0, message = FALSE, echo = FALSE, fig.cap="Combined ICCs for items 1 (reason.4) and 2 (reason.16) for the 1PL model.", fig.pos = "t!"----
plot(onepl_fit, type = "trace", which.items = 1:2,
     facet_items = FALSE,
     auto.key = list(points = FALSE,
     lines = TRUE, columns = 2, space = "top"), main = "",
     par.settings = simpleTheme(lty = 1:2, lwd = 2, col = "black"))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(onepl_fit, type = "infoSE", theta_lim = c(-3, 3))

## ----tifcsem, message = FALSE, echo = FALSE, fig.cap="The TIF and cSEM plot for the 1PL model.", fig.pos = "t!"----
plot(onepl_fit, type = "infoSE", theta_lim = c(-3, 3), main = "",
     par.settings = list(superpose.line = list(col = c("black", "black"), lty = 1:2)))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(onepl_fit, type = "info", theta_lim = c(-3, 3))
## plot(onepl_fit, type = "SE", theta_lim = c(-3, 3))

## ----message = FALSE, verbose = FALSE, eval = T, results='hide'----------
rasch_mod <- "F = 1 - 16"
rasch_fit <- mirt(data = SAPA, model = rasch_mod,
                      itemtype = "Rasch", SE = TRUE)

## ----message = FALSE, verbose = FALSE, echo = T, eval = F----------------
## rasch_params <- coef(rasch_fit, IRTpars = TRUE, simplify = TRUE)
## rasch_items <- rasch_params$items
## head(rasch_items)

## ----eval = T, echo = F, background = "gray95"---------------------------
rasch_params <- coef(rasch_fit, IRTpars = TRUE, simplify = TRUE)
rasch_items <- rasch_params$items
head(rasch_items)

## ----eval = F------------------------------------------------------------
## plot(rasch_fit, type = "trace", which.items = c(2, 16))

## ----icc2, message = FALSE, echo = F, fig.cap="Item characteristic curves for items 2 (reason.16) and 16 (rotate.8) for the Rasch model."----
plot(rasch_fit, type = "trace", which.items = c(2,16), col.line = "black", main = "",
     par.settings = list(strip.background = list(col = "white")))

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
twopl_mod <- "F = 1 - 16"
twopl_fit <- mirt(data = SAPA, model = twopl_mod,
                  itemtype = "2PL", SE = TRUE)
twopl_params <- coef(twopl_fit, IRTpars = TRUE, simplify = TRUE)
twopl_items <- twopl_params$items
twopl_items

## ----eval = T, echo = F, background = "gray95"---------------------------
twopl_items

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(twopl_fit, type = "trace", which.items = c(12, 14))

## ----icc3, echo = FALSE, message = FALSE, fig.cap="Item characteristic curves for items 12 (matrix.55) and 14 (rotate.4) for the 2PL model.", fig.align = "center", fig.height = 4, fig.pos = "!ht"----
plot(twopl_fit, type = "trace", which.items = c(12, 14), main= "", col.line = "black",
     par.settings = list(strip.background = list(col = "white")))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(twopl_fit, type = "trace", which.items = c(5, 16),
##      facet_items = FALSE, auto.key = list(points = FALSE,
##      lines = TRUE, columns = 2),
##      par.settings = simpleTheme(lty = 1:2))

## ----icc4, echo = FALSE, message = FALSE, fig.cap="Item characteristic curves for items 5 (letter.7) and 16 (rotate.8) for the 2PL model."----
plot(twopl_fit, type = "trace", which.items = c(5, 16),
     facet_items = FALSE, auto.key = list(points=FALSE,
     lines=TRUE, columns=2, space = "top"), main = "",
     par.settings = simpleTheme(lty=1:2,lwd=1, col="black"))

## ----message = FALSE, verbose = FALSE, eval = T, echo = T, results="hide"----
threepl_mod <- "F = 1-16"
threepl_fit <- mirt(data = SAPA, model = threepl_mod,
                    itemtype = "3PL", SE = TRUE)
threepl_params <- coef(threepl_fit, IRTpars = TRUE,
                       simplify = TRUE)
threepl_items <- threepl_params$items
head(threepl_items)

## ----eval = T, echo = F, background = "gray95"---------------------------
head(threepl_items)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(threepl_fit, type = "trace", which.items = c(1, 4),
##      facet_items = FALSE, auto.key = list(points = FALSE,
##      lines = TRUE, columns = 2),
##      par.settings = simpleTheme(lty = 1:2))

## ----icc5, echo = FALSE, message = FALSE, fig.cap="Item characteristic curves for reason.4 (item 1) and reason.19 (item 4) for the 3PL model.", fig.align = "center", fig.height = 4, fig.pos = "!ht"----
plot(threepl_fit, type = "trace", which.items = c(1, 4), main="",
     facet_items = FALSE, auto.key = list(points=FALSE,
     lines=TRUE,columns=2),par.settings=simpleTheme(lty=1:2,lwd=1,col="black"))

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
fourpl_mod <- "F = 1 - 16"
fourpl_fit <- mirt(data = SAPA, model = fourpl_mod,
                   itemtype = "4PL", SE = TRUE)
fourpl_params <- coef(fourpl_fit, IRTpars = TRUE,
                      simplify = TRUE)
fourpl_items <- fourpl_params$items
head(fourpl_items)

## ----eval = T, echo = F, background = "gray95"---------------------------
head(fourpl_items)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(fourpl_fit, type = "trace", which.items = c(13, 16))

## ----message = FALSE, warning=FALSE--------------------------------------
latent_mle <- fscores(twopl_fit, method = "ML",
                      full.scores = TRUE, full.scores.SE = TRUE)
latent_map <- fscores(twopl_fit, method = "MAP",
                      full.scores = TRUE, full.scores.SE = TRUE)
latent_eap <- fscores(twopl_fit, method = "EAP",
                      full.scores = TRUE, full.scores.SE = TRUE)

## ----message = FALSE, verbose = FALSE, eval = F, results = "hide"--------
## head(latent_mle)

## ----eval = T, echo = F, background = "gray95"---------------------------
head(latent_mle)

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
latent <- data.frame(MLE = latent_mle[ ,1],
                     MAP = latent_map[ ,1],
                     EAP = latent_eap[ ,1])
head(latent)

## ----eval = T, echo = F, background = "gray95"---------------------------
head(latent)

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
latent[c(73, 89, 103, 105), ]

## ----eval = T, echo = F, background = "gray95"---------------------------
latent[c(73, 89, 103, 105), ]

## ----message = FALSE, warning=FALSE--------------------------------------
latent_est <- latent[is.finite(latent$MLE), ]

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
apply(latent_est, 2, summary)
apply(latent_est, 2, sd)

## ----eval = T, echo = F, background = "gray95"---------------------------
apply(latent_est, 2, summary)
apply(latent_est, 2, sd)

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
cor(latent_est)

## ----eval = T, echo = F, background = "gray95"---------------------------
cor(latent_est)

## ----scatter1, message = FALSE, fig.cap="Scatterplot matrix of latent trait estimates.", fig.pos = "t!"----
pairs(latent_est)

## ----eval = F------------------------------------------------------------
## rmsd(latent_est$MLE, latent_est$MAP)

## ----eval = T, echo = F, background = "gray95"---------------------------
rmsd(latent_est$MLE, latent_est$MAP)

## ----eval = F------------------------------------------------------------
## rmsd(latent_est$MLE, latent_est$EAP)

## ----eval = T, echo = F, background = "gray95"---------------------------
rmsd(latent_est$MLE, latent_est$EAP)

## ----eval = F------------------------------------------------------------
## rmsd(latent_est$MAP, latent_est$EAP)

## ----eval = T, echo = F, background = "gray95"---------------------------
rmsd(latent_est$MAP, latent_est$EAP)

## ----message = FALSE, verbose = FALSE, eval = T, results = "hide"--------
rasch_itemfit <- itemfit(rasch_fit,
                     fit_stats = c("S_X2", "G2"),
                     impute = 10)
head(rasch_itemfit)

## ----eval = T, echo = F, background = "gray95"---------------------------
rasch_itemfit <- itemfit(rasch_fit,
                     fit_stats = c("S_X2", "G2"),
                     impute = 10)
head(rasch_itemfit)

## ----message = FALSE, warning=FALSE, eval = F----------------------------
## itemfit(rasch_fit,
##         fit_stats = c("Zh", "infit"),
##         impute = 10)

## ----message = FALSE, warning=FALSE, eval = FALSE------------------------
## itemfit(rasch_fit, empirical.plot = 1)

## ----itemfit, echo = FALSE, message = FALSE, fig.cap="Empirical plot for item 1.", fig.pos = "t!"----
itemfit(rasch_fit, empirical.plot = 1, par.settings = simpleTheme(lwd=1, col="black", col.line = "black"), empirical.CI = 0)

## ----eval = F------------------------------------------------------------
## SAPA_nomiss <- na.omit(SAPA)
## rasch_mod <- "F = 1 - 16"
## rasch_fit <- mirt(data = SAPA_nomiss, model = rasch_mod,
##                   itemtype = "Rasch", SE = TRUE,
##                   verbose = FALSE)
## rasch_personfit <- personfit(rasch_fit)
## head(rasch_personfit)

## ----eval = T, echo = F, background = "gray95"---------------------------
SAPA_nomiss <- na.omit(SAPA)
rasch_mod <- "F = 1 - 16"
rasch_fit <- mirt(data = SAPA_nomiss, model = rasch_mod,
                  itemtype = "Rasch", SE = TRUE,
                  verbose = FALSE)
rasch_personfit <- personfit(rasch_fit)
head(rasch_personfit)

## ----message = FALSE, warning=FALSE, eval=FALSE--------------------------
## hist(rasch_personfit$Zh, xlab="Zh Statistic")
## abline(v = -2, lwd = 2, lty = 2)

## ----personfit, echo = FALSE, message = FALSE, fig.cap="Distribution of the Zh statistic for the Rasch model.", fig.pos = "t!"----
hist(rasch_personfit$Zh, xlab="Zh Statistic", main="")
abline(v=-2, lwd=2, lty=2)

## ----eval = F------------------------------------------------------------
## rasch_misfits <- subset(rasch_personfit, Zh < -2)
## rownames(rasch_misfits)

## ----eval = T, echo = F, background = "gray95"---------------------------
rasch_misfits <- subset(rasch_personfit, Zh < -2)
rownames(rasch_misfits)

## ----eval = F------------------------------------------------------------
## nrow(rasch_misfits)

## ----eval = T, echo = F, background = "gray95"---------------------------
nrow(rasch_misfits)

## ----eval = F------------------------------------------------------------
## anova(onepl_fit, twopl_fit)

## ----eval = F------------------------------------------------------------
## anova(twopl_fit, threepl_fit)

## ----eval = F------------------------------------------------------------
## anova(threepl_fit, fourpl_fit)

