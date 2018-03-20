## ----eval = F------------------------------------------------------------
## library("hemp")
## library("mirt")

## ----eval = F, echo = T--------------------------------------------------
## pcm_mod <- "selfesteem = 1 - 10"
## pcm_fit <- mirt(data = rse[, 1:10], model = pcm_mod,
##                 itemtype = "Rasch", SE = TRUE)
## pcm_params <- coef(pcm_fit, IRTpars = TRUE, simplify = TRUE)

## ----eval = T, echo = F--------------------------------------------------
pcm_mod <- "selfesteem = 1 - 10"
pcm_fit <- mirt(data = rse[, 1:10], model = pcm_mod,
                itemtype = "Rasch", SE = TRUE, verbose = FALSE)
pcm_params <- coef(pcm_fit, IRTpars = TRUE, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, eval = F--------------------------
## pcm_items <- as.data.frame(pcm_params$items)
## pcm_items

## ----eval = T, echo = F, background = "gray95"---------------------------
pcm_items <- as.data.frame(pcm_params$items)
pcm_items

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(pcm_fit, type = "trace", which.items = c(2, 5),
##      par.settings = simpleTheme(lty = 1:4, lwd = 2),
##      auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----pcm1, echo = FALSE, message = FALSE, fig.cap="Option characteristic curves for items 2 and 5 for PCM.", fig.align = "center", fig.pos = "!t"----
plot(pcm_fit, type = "trace", which.items = c(2, 5), main="", 
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(pcm_fit, type = "infotrace", which.items = c(2, 5),
##      par.settings = simpleTheme(lwd = 2))

## ----pcm2, echo = FALSE, message = FALSE, fig.cap="Item information functions for items 2 and 5 for PCM.", fig.align = "center", fig.pos = "!ht"----
plot(pcm_fit, type = "infotrace", which.items = c(2, 5), main="", 
     par.settings = list(strip.background = list(col = "white"),
                         plot.line = list(col = "black", lwd = 2)))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(pcm_fit, type = "info", theta_lim = c(-6, 6))
## plot(pcm_fit, type = "SE", theta_lim = c(-6, 6))

## ----message = FALSE, verbose = FALSE, eval = F--------------------------
## rsm_mod <- "selfesteem = 1 - 10"
## rsm_fit <- mirt(data = rse[,1:10], model = rsm_mod,
##                 itemtype = "rsm")
## rsm_params <- coef(rsm_fit, simplify = TRUE)
## rsm_items <- as.data.frame(rsm_params$items)
## rsm_items

## ----eval = T, echo = F, background = "gray95"---------------------------
rsm_mod <- "selfesteem = 1 - 10"
rsm_fit <- mirt(data = rse[,1:10], model = rsm_mod,
                itemtype = "rsm",
                verbose = FALSE)
rsm_params <- coef(rsm_fit, simplify = TRUE)
rsm_items <- as.data.frame(rsm_params$items)
rsm_items

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(rsm_fit, type = "trace", which.items = c(2, 9),
##      par.settings = simpleTheme(lty = 1:4, lwd = 2),
##      auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----rsm1, echo = FALSE, message = FALSE, fig.cap="Option characteristic curves for items 2 and 9 for RSM.", fig.align = "center", fig.pos = "!t"----
plot(rsm_fit, type = "trace", which.items = c(2, 9), main="",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----rsm2, eval = FALSE, message = FALSE---------------------------------
## plot(rsm_fit, type = "info", theta_lim = c(-6, 6))
## plot(rsm_fit, type = "SE", theta_lim = c(-6, 6))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## gpcm_mod <- "selfesteem = 1 - 10"
## gpcm_fit <- mirt(data = rse[, 1:10], model = gpcm_mod,
##                  itemtype = "gpcm", SE = TRUE)
## gpcm_params <- coef(gpcm_fit, IRTpars = TRUE, simplify = TRUE)
## gpcm_items <- gpcm_params$items
## gpcm_items

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
gpcm_mod <- "selfesteem = 1 - 10"
gpcm_fit <- mirt(data = rse[, 1:10], model = gpcm_mod,
                 itemtype = "gpcm", SE = TRUE, verbose = FALSE)
gpcm_params <- coef(gpcm_fit, IRTpars = TRUE, simplify = TRUE)
gpcm_items <- gpcm_params$items
gpcm_items

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(gpcm_fit, type = "trace", which.items = c(6, 8),
##      par.settings = simpleTheme(lty = 1:4, lwd = 2),
##      auto.key=list(points = FALSE, lines = TRUE, columns = 4))

## ----gpcm2, echo = FALSE, message = FALSE, fig.cap="Option characteristic curves for items 6 and 8 for GCPM.", fig.align = "center", fig.pos = "t!"----
plot(gpcm_fit, type = "trace", which.items = c(6, 8), main="",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----gpcm1, eval = FALSE, message = FALSE--------------------------------
## plot(gpcm_fit, type = "info", theta_lim = c(-6, 6))
## plot(gpcm_fit, type = "SE", theta_lim = c(-6, 6))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## grm_mod <- "selfesteem = 1-10"
## grm_fit <- mirt(data = rse[, 1:10], model = grm_mod,
##                 itemtype = "graded", SE = TRUE)
## grm_params <- coef(grm_fit, IRTpars = TRUE, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, echo = FALSE----------------------
grm_mod <- "selfesteem = 1-10"
grm_fit <- mirt(data = rse[, 1:10], model = grm_mod,
                    itemtype = "graded", SE = TRUE, verbose=FALSE)
grm_params <- coef(grm_fit, IRTpars = TRUE, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## grm_items <- as.data.frame(grm_params$items)
## grm_items

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
grm_items <- as.data.frame(grm_params$items)
grm_items

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(grm_fit, type = "trace", which.items = c(5, 9),
##      par.settings = simpleTheme(lty = 1:4,lwd = 2),
##      auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----grm2, echo = FALSE, message = FALSE, fig.cap="Option characteristic curves for Q5 and Q9 for GRM.", fig.align = "center", fig.pos = "t!"----
plot(grm_fit, type = "trace", which.items = c(5, 9), main="",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----grm3, eval = FALSE, message = FALSE---------------------------------
## plot(grm_fit, type = "info", theta_lim = c(-6, 6))
## plot(grm_fit, type = "SE", theta_lim = c(-6, 6))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## nrm_mod <- "agression = 1 - 24"
## nrm_fit <- mirt(data = VerbAggWide[, 4:27], model = nrm_mod,
##                 itemtype = "nominal", SE = TRUE)
## nrm_params <- coef(nrm_fit, IRTpars = TRUE, simplify = TRUE)
## nrm_items <- as.data.frame(nrm_params$items)
## head(nrm_items, 4)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
nrm_mod <- 'agression = 1 - 24'
nrm_fit <- mirt(data = VerbAggWide[, 4:27], model = nrm_mod,
                itemtype = "nominal", SE = TRUE, verbose=FALSE)
nrm_params <- coef(nrm_fit, IRTpars = TRUE, simplify = TRUE)
nrm_items <- as.data.frame(nrm_params$items)
head(nrm_items, 4)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## sum_constraints <- data.frame(
##   discrimination = round(rowSums(nrm_items[, 1:3]), 3),
##   difficulty = round(rowSums(nrm_items[, 4:6]), 3))
## head(sum_constraints)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
sum_constraints <- data.frame(discrimination = round(rowSums(nrm_items[, 1:3]),3),
                  difficulty = round(rowSums(nrm_items[, 4:6]), 3))
head(sum_constraints)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(nrm_fit, type = "trace", which.items = c(3, 15),
##      par.settings = simpleTheme(lty = 1:3, lwd = 2),
##      auto.key = list(points = FALSE, lines = TRUE, columns = 3))

## ----nrm1, echo = FALSE, message = FALSE, fig.cap="Option characteristic curves for items 3 and 15 for NRM.", fig.align = "center", fig.pos = "t!"----
plot(nrm_fit, type = "trace", which.items = c(3, 15), main="",
par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:3, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 3))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## itemplot(nrm_fit, item = 15, CE = TRUE)

## ----nrm2, message = FALSE, echo = FALSE, fig.cap="Option characteristic curves for item 15 for NRM.", fig.align = "center", fig.pos = "!t"----
itemplot(nrm_fit, item = 15, CE = TRUE, main="", lwd = 2, col.line = "black",
         par.settings = list(strip.background = list(col = "white")))


## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## key = c(4, 3, 2, 3, 4, 3, 2, 3, 1,
##         4, 3, 2, 3, 3, 4, 2, 4, 3,
##         3, 2, 2, 1, 2, 1, 1, 2, 1)
## twoplnlm_mod <- "ability = 1 - 27"
## twoplnlm_fit <- mirt(data = multiplechoice,
##                      model = twoplnlm_mod, itemtype = "2PLNRM",
##                      SE = TRUE, key = key)
## twoplnlm_params <- coef(twoplnlm_fit, IRTpars = TRUE,
##                         simplify = TRUE)
## twoplnlm_items <- as.data.frame(twoplnlm_params$items)
## head(twoplnlm_items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
key = c(4, 3, 2, 3, 4, 3, 2, 3, 1,
        4, 3, 2, 3, 3, 4, 2, 4, 3,
        3, 2, 2, 1, 2, 1, 1, 2, 1)
twoplnlm_mod <- "ability = 1 - 27"
twoplnlm_fit <- mirt(data = multiplechoice,
                     model = twoplnlm_mod, itemtype = "2PLNRM",
                     SE = TRUE, key = key, verbose = FALSE)
twoplnlm_params <- coef(twoplnlm_fit, IRTpars = TRUE, simplify = TRUE)
twoplnlm_items <- as.data.frame(twoplnlm_params$items)
head(twoplnlm_items)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(twoplnlm_fit, type = "trace", which.items = c(8, 21),
##      par.settings = simpleTheme(lty = 1:4, lwd = 2),
##      auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----nlm2pl, echo = FALSE, message = FALSE, fig.cap="Option characteristic curves for items 8 and 21 for 2PL-NLM.", fig.align = "center", fig.pos = "t!"----
plot(twoplnlm_fit, type = "trace", which.items = c(8, 21), main="",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## threeplnlm_mod <- "ability = 1 - 27"
## threeplnlm_fit <- mirt(data = multiplechoice,
##                      model = threeplnlm_mod, itemtype = "3PLNRM",
##                      SE = TRUE, key = key)
## threeplnlm_params <- coef(threeplnlm_fit, IRTpars = TRUE,
##                           simplify = TRUE)
## threeplnlm_items <- as.data.frame(threeplnlm_params$items)
## head(threeplnlm_items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
threeplnlm_mod <- "ability = 1 - 27"
threeplnlm_fit <- mirt(data = multiplechoice,
                     model = threeplnlm_mod, itemtype = "3PLNRM",
                     SE = TRUE, key = key, verbose = FALSE)
threeplnlm_params <- coef(threeplnlm_fit, IRTpars = TRUE, simplify = TRUE)
threeplnlm_items <- as.data.frame(threeplnlm_params$items)
round(head(threeplnlm_items), 4)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(threeplnlm_fit, type = "trace", which.items = c(1, 17),
##      par.settings = simpleTheme(lty = 1:4, lwd = 2),
##      auto.key = list(points = FALSE, lines=TRUE, columns=4))

## ----nlm3pl, message = FALSE, echo = FALSE, fig.cap="Option characteristic curves for items 1 and 17 for 3PL-NLM.", fig.align = "center", fig.pos = "t!"----
plot(threeplnlm_fit, type = 'trace', which.items = c(1, 17), main="",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black", lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

