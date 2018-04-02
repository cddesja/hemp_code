## -------------------------------------------------------------------
## Chapter 5 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------

# load and install mirt, load hemp
install.packages("mirt")
library("mirt")
library("hemp")

# figre 5.4 ----
model_1PL <- "F = 1-16
              CONSTRAIN = (1-16, a1)"
results_1PL <- mirt(data = SAPA, model = model.1PL, SE = TRUE, verbose = FALSE)
plot(results_1PL, type = "infoSE", theta_lim = c(-4, 4), main = "",
     auto.key = list(points = TRUE, lines = TRUE, columns = 2, space = "top"),
     par.settings = list(superpose.line = list(col = c("black", "black"),
                                               lty = 1:2)))

# one parameter logistic model ----
onepl_mod <- "F = 1 - 16
              CONSTRAIN = (1 - 16, a1)"
onepl_fit <- mirt(data = SAPA, model = onepl_mod, SE = TRUE)
onepl_params <- coef(onepl_fit, IRTpars = TRUE, simplify = TRUE)
onepl_items <- onepl_params$items
head(onepl_items)
onepl_se <- coef(onepl_fit, printSE = TRUE)
names(onepl_se)
onepl_se$rotate.8
plot(onepl_fit, type = "trace", which.items = 1:2)

# figure 5.5 ----
plot(onepl_fit, type = "trace", which.items = 1:2, main = "",
     col.line = "black",
     par.settings = list(strip.background = list(col = "white")))

# item characteristic curves and item information functions
itemplot(onepl_fit, type = "trace", item = 1)
itemplot(onepl_fit, type = "trace", item = 2)
plot(onepl_fit, type = "infotrace", which.items = c(3, 5))

# figure 5.6 ----
plot(onepl_fit, type = "infotrace", which.items = c(3, 5), main = "",
     col.line = "black",
     par.settings = list(strip.background = list(col = "white")))

# figure 5.7 ----
plot(onepl_fit, type = "trace", which.items = 1:2,
     facet_items = FALSE,
     auto.key = list(points = FALSE,
     lines = TRUE, columns = 2, space = "top"), main = "",
     par.settings = simpleTheme(lty = 1:2, lwd = 2, col = "black"))

plot(onepl_fit, type = "infoSE", theta_lim = c(-3, 3))

# figure 5.8 ----
plot(onepl_fit, type = "infoSE", theta_lim = c(-3, 3), main = "",
     par.settings = list(superpose.line = list(col = c("black", "black"),
                                               lty = 1:2)))

plot(onepl_fit, type = "info", theta_lim = c(-3, 3))
plot(onepl_fit, type = "SE", theta_lim = c(-3, 3))

# Rasch model ----
rasch_mod <- "F = 1 - 16"
rasch_fit <- mirt(data = SAPA, model = rasch_mod,
                      itemtype = "Rasch", SE = TRUE)
rasch_params <- coef(rasch_fit, IRTpars = TRUE, simplify = TRUE)
rasch_items <- rasch_params$items
head(rasch_items)

# figure 5.9 ----
plot(rasch_fit, type = "trace", which.items = c(2, 16), col.line = "black",
     main = "",
     par.settings = list(strip.background = list(col = "white")))

# color version
plot(rasch_fit, type = "trace", which.items = c(2, 16))


# two parameter logistic model
twopl_mod <- "F = 1 - 16"
twopl_fit <- mirt(data = SAPA, model = twopl_mod,
                  itemtype = "2PL", SE = TRUE)
twopl_params <- coef(twopl_fit, IRTpars = TRUE, simplify = TRUE)
twopl_items <- twopl_params$items
twopl_items

# figure 5.10
plot(twopl_fit, type = "trace", which.items = c(12, 14), main = "",
     col.line = "black",
     par.settings = list(strip.background = list(col = "white")))

# color version
plot(twopl_fit, type = "trace", which.items = c(12, 14))


# figure 5.11 ----
plot(twopl_fit, type = "trace", which.items = c(5, 16),
     facet_items = FALSE, auto.key = list(points = FALSE,
     lines = TRUE, columns = 2, space = "top"), main = "",
     par.settings = simpleTheme(lty = 1:2, lwd = 1, col = "black"))

# color version
plot(twopl_fit, type = "trace", which.items = c(5, 16),
     facet_items = FALSE, auto.key = list(points = FALSE,
                                          lines = TRUE, columns = 2),
     par.settings = simpleTheme(lty = 1:2))


# three parameter logistic model ----
threepl_mod <- "F = 1 - 16"
threepl_fit <- mirt(data = SAPA, model = threepl_mod,
                    itemtype = "3PL", SE = TRUE)
threepl_params <- coef(threepl_fit, IRTpars = TRUE,
                       simplify = TRUE)
threepl_items <- threepl_params$items
head(threepl_items)

# figure 5.12
plot(threepl_fit, type = "trace", which.items = c(1, 4), main = "",
     facet_items = FALSE, auto.key = list(points = FALSE,
     lines = TRUE, columns = 2),
     par.settings = simpleTheme(lty = 1:2, lwd = 1, col = "black"))

# color version
plot(threepl_fit, type = "trace", which.items = c(1, 4),
     facet_items = FALSE, auto.key = list(points = FALSE,
                                          lines = TRUE, columns = 2),
     par.settings = simpleTheme(lty = 1:2))

# four parameter logistic model ----
fourpl_mod <- "F = 1 - 16"
fourpl_fit <- mirt(data = SAPA, model = fourpl_mod,
                   itemtype = "4PL", SE = TRUE)
fourpl_params <- coef(fourpl_fit, IRTpars = TRUE,
                      simplify = TRUE)
fourpl_items <- fourpl_params$items
head(fourpl_items)
plot(fourpl_fit, type = "trace", which.items = c(13, 16))

# ability estimation ----
latent_mle <- fscores(twopl_fit, method = "ML",
                      full.scores = TRUE, full.scores.SE = TRUE)
latent_map <- fscores(twopl_fit, method = "MAP",
                      full.scores = TRUE, full.scores.SE = TRUE)
latent_eap <- fscores(twopl_fit, method = "EAP",
                      full.scores = TRUE, full.scores.SE = TRUE)
head(latent_mle)
latent <- data.frame(MLE = latent_mle[, 1],
                     MAP = latent_map[, 1],
                     EAP = latent_eap[, 1])
head(latent)
latent[c(73, 89, 103, 105), ]
latent_est <- latent[is.finite(latent$MLE), ]
apply(latent_est, 2, summary)
apply(latent_est, 2, sd)
cor(latent_est)
pairs(latent_est)
rmsd(latent_est$MLE, latent_est$MAP)
rmsd(latent_est$MLE, latent_est$EAP)
rmsd(latent_est$MAP, latent_est$EAP)

# model diagnostics ----
rasch_itemfit <- itemfit(rasch_fit,
                     fit_stats = c("S_X2", "G2"),
                     impute = 10)
head(rasch_itemfit)
itemfit(rasch_fit,
        fit_stats = c("Zh", "infit"),
        impute = 10)

# figure 5.14 ----
itemfit(rasch_fit, empirical.plot = 1)

# color version
itemfit(rasch_fit, empirical.plot = 1,
        par.settings = simpleTheme(lwd = 1,
                                   col = "black",
                                   col.line = "black"),
        empirical.CI = 0)

SAPA_nomiss <- na.omit(SAPA)
rasch_mod <- "F = 1 - 16"
rasch_fit <- mirt(data = SAPA_nomiss, model = rasch_mod,
                  itemtype = "Rasch", SE = TRUE,
                  verbose = FALSE)
rasch_personfit <- personfit(rasch_fit)
head(rasch_personfit)
hist(rasch_personfit$Zh, xlab = "Zh Statistic", main = "")
abline(v = -2, lwd = 2, lty = 2)
rasch_misfits <- subset(rasch_personfit, Zh < -2)
rownames(rasch_misfits)
nrow(rasch_misfits)

# model comparsion ----
anova(onepl_fit, twopl_fit)
anova(twopl_fit, threepl_fit)
anova(threepl_fit, fourpl_fit)
