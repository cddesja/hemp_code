## -------------------------------------------------------------------
## Chapter 6 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------
library("hemp")
library("mirt")

# partial credit model ----
pcm_mod <- "selfesteem = 1 - 10"
pcm_fit <- mirt(data = rse[, 1:10], model = pcm_mod,
                itemtype = "Rasch", SE = TRUE, verbose = FALSE)
pcm_params <- coef(pcm_fit, IRTpars = TRUE, simplify = TRUE)
pcm_items <- as.data.frame(pcm_params$items)
pcm_items

# figure 6.2 ----
plot(pcm_fit, type = "trace", which.items = c(2, 5), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# color version
plot(pcm_fit, type = "trace", which.items = c(2, 5),
     par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# figure 6.3 ----
plot(pcm_fit, type = "infotrace", which.items = c(2, 5), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         plot.line = list(col = "black", lwd = 2)))

# color version
plot(pcm_fit, type = "infotrace", which.items = c(2, 5),
     par.settings = simpleTheme(lwd = 2))

plot(pcm_fit, type = "info", theta_lim = c(-6, 6))
plot(pcm_fit, type = "SE", theta_lim = c(-6, 6))

# rating scale model ----
rsm_mod <- "selfesteem = 1 - 10"
rsm_fit <- mirt(data = rse[, 1:10], model = rsm_mod,
                itemtype = "rsm")
rsm_params <- coef(rsm_fit, simplify = TRUE)
rsm_items <- as.data.frame(rsm_params$items)
rsm_items

# figure 6.5 ----
plot(rsm_fit, type = "trace", which.items = c(2, 9), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# color version
plot(rsm_fit, type = "trace", which.items = c(2, 9),
     par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

plot(rsm_fit, type = "info", theta_lim = c(-6, 6))
plot(rsm_fit, type = "SE", theta_lim = c(-6, 6))

# generalized partial credit model ----
gpcm_mod <- "selfesteem = 1 - 10"
gpcm_fit <- mirt(data = rse[, 1:10], model = gpcm_mod,
                 itemtype = "gpcm", SE = TRUE, verbose = FALSE)
gpcm_params <- coef(gpcm_fit, IRTpars = TRUE, simplify = TRUE)
gpcm_items <- gpcm_params$items
gpcm_items

# figure 6.6 ----
plot(gpcm_fit, type = "trace", which.items = c(6, 8), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# color version
plot(gpcm_fit, type = "trace", which.items = c(6, 8),
     par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

plot(gpcm_fit, type = "info", theta_lim = c(-6, 6))
plot(gpcm_fit, type = "SE", theta_lim = c(-6, 6))


# graded response model ----
grm_mod <- "selfesteem = 1-10"
grm_fit <- mirt(data = rse[, 1:10], model = grm_mod,
                    itemtype = "graded", SE = TRUE, verbose = FALSE)
grm_params <- coef(grm_fit, IRTpars = TRUE, simplify = TRUE)
grm_items <- as.data.frame(grm_params$items)
grm_items

# figure 6.8 ----
plot(grm_fit, type = "trace", which.items = c(5, 9), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# color version
plot(grm_fit, type = "trace", which.items = c(5, 9),
     par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

plot(grm_fit, type = "info", theta_lim = c(-6, 6))
plot(grm_fit, type = "SE", theta_lim = c(-6, 6))


# nominal response model ----
nrm_mod <- "agression = 1 - 24"
nrm_fit <- mirt(data = VerbAggWide[, 4:27], model = nrm_mod,
                itemtype = "nominal", SE = TRUE)
nrm_params <- coef(nrm_fit, IRTpars = TRUE, simplify = TRUE)
nrm_items <- as.data.frame(nrm_params$items)
head(nrm_items, 4)
sum_constraints <- data.frame(
  discrimination = round(rowSums(nrm_items[, 1:3]), 3),
  difficulty = round(rowSums(nrm_items[, 4:6]), 3))
head(sum_constraints)

# figure 6.9 ----
plot(nrm_fit, type = "trace", which.items = c(3, 15), main = "",
par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:3, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 3))

# color version
plot(nrm_fit, type = "trace", which.items = c(3, 15),
     par.settings = simpleTheme(lty = 1:3, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 3))

# figure 6.10 ----
itemplot(nrm_fit, item = 15, CE = TRUE, main = "", lwd = 2, col.line = "black",
         par.settings = list(strip.background = list(col = "white")))

# color version
itemplot(nrm_fit, item = 15, CE = TRUE)


# nested logit model ----
key <- c(4, 3, 2, 3, 4, 3, 2, 3, 1,
        4, 3, 2, 3, 3, 4, 2, 4, 3,
        3, 2, 2, 1, 2, 1, 1, 2, 1)
twoplnlm_mod <- "ability = 1 - 27"
twoplnlm_fit <- mirt(data = multiplechoice,
                     model = twoplnlm_mod, itemtype = "2PLNRM",
                     SE = TRUE, key = key, verbose = FALSE)
twoplnlm_params <- coef(twoplnlm_fit, IRTpars = TRUE, simplify = TRUE)
twoplnlm_items <- as.data.frame(twoplnlm_params$items)
head(twoplnlm_items)

# figure 6.11 ----
plot(twoplnlm_fit, type = "trace", which.items = c(8, 21), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# color version
plot(twoplnlm_fit, type = "trace", which.items = c(8, 21),
     par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

threeplnlm_mod <- "ability = 1 - 27"
threeplnlm_fit <- mirt(data = multiplechoice,
                     model = threeplnlm_mod, itemtype = "3PLNRM",
                     SE = TRUE, key = key, verbose = FALSE)
threeplnlm_params <- coef(threeplnlm_fit, IRTpars = TRUE, simplify = TRUE)
threeplnlm_items <- as.data.frame(threeplnlm_params$items)
round(head(threeplnlm_items), 4)

# figure 6.12 ----
plot(threeplnlm_fit, type = "trace", which.items = c(1, 17), main = "",
     par.settings = list(strip.background = list(col = "white"),
                         superpose.line = list(col = "black",
                                               lty = 1:4, lwd = 2)),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# color version
plot(threeplnlm_fit, type = "trace", which.items = c(1, 17),
     par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))
