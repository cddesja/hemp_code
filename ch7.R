## -------------------------------------------------------------------
## Chapter 7 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------
library("hemp")
library("mirt")

# multidimensional 2pl model ----
m2pl_mod <- "F1 = 1 - 6, 13 - 21, 23 - 24
             F2 = 7 - 20, 22 - 24
             COV = F1 * F2"
m2pl_fit <- mirt(data = mimic, model = m2pl_mod,
                 itemtype = "2PL", method = "EM",
                 SE = TRUE, verbose = F)
m2pl_params <- coef(m2pl_fit, simplify = TRUE)
head(m2pl_params$items)
m2pl_items <- data.frame(MDISC(m2pl_fit),
                         MDIFF(m2pl_fit))
colnames(m2pl_items) <- c("m2pl_mdisc", "m2pl_mdiff")
head(m2pl_items)
m2pl_params$cov
itemplot(m2pl_fit, type = "trace", item = 13)
itemplot(m2pl_fit, type = "tracecontour", item = 13)
itemplot(m2pl_fit, type = "info", item = 13)
itemplot(m2pl_fit, type = "infocontour", item = 13)
itemplot(m2pl_fit, type = "score", item = 13)
itemplot(m2pl_fit, type = "SE", item = 13)

plot(m2pl_fit, type = "info")
plot(m2pl_fit, type = "SE")

m2pl_map <- fscores(m2pl_fit, method = "MAP", full.scores = TRUE,
                    full.scores.SE = TRUE)
head(m2pl_map)
m2pl_eap <- fscores(m2pl_fit, method =  "EAP",
                    full.scores = TRUE,
                    full.scores.SE = TRUE)
head(m2pl_eap)
m2pl_scores <- data.frame(map1 = m2pl_map[, 1],
                          map2 = m2pl_map[, 2],
                          eap1 = m2pl_eap[, 1],
                          eap2 = m2pl_eap[, 2])
cor(m2pl_scores)

m2pl_mod_constrant <- "F1 = 1 - 6, 13 - 21, 23 - 24
             F2 = 7 - 20, 22 - 24
             COV = F1 * F2
             CONSTRAIN = (1 - 6, 21, a1), (7 - 12, 22, a2)"

m2pl_fit <- mirt(data = mimic, model = m2pl_mod,
                     itemtype = "PC2PL", SE = TRUE)

# multidimensional Rasch
mrasch_mod <- "F1 = 1 - 6, 13 - 21, 23 - 24
               F2 = 7 - 20, 22 - 24
               COV = F1 * F2"
mrasch_fit <- mirt(data = mimic, model = mrasch_mod,
                       itemtype = "Rasch", SE = TRUE, verbose = FALSE)
mrasch_params <- coef(mrasch_fit, simplify = TRUE)
head(mrasch_params$items, 10)
mrasch_mdiff <- MDIFF(mrasch_fit)
head(mrasch_mdiff)
mrasch_params$cov
itemplot(mrasch_fit, type = "trace", item = 13)
itemplot(mrasch_fit, type = "tracecontour", item = 13)
plot(mrasch_fit, type = "info")
plot(mrasch_fit, type = "SE")
plot(mrasch_fit, type = "score")

# multidimensional graded response model ---
mgrm_mod <- "Cognitive = 1 - 10
               Somatic = 11 - 20
               COV = Cognitive * Somatic"
mgrm_fit <- mirt(data = depression, model = mgrm_mod,
                     itemtype = "graded", SE = TRUE, verbose = FALSE)
mgrm_params <- coef(mgrm_fit, simplify = TRUE)
round(head(mgrm_params$items), 4)
mgrm_items <- cbind(MDISC(mgrm_fit),
                    MDIFF(mgrm_fit))
round(head(mgrm_items), 4)
mgrm_params$cov
itemplot(mgrm_fit, type = "trace", item = 7)
itemplot(mgrm_fit, type = "trace", item = 13)
plot(mgrm_fit, type = "info")
plot(mgrm_fit, type = "SE")
plot(mgrm_fit, type = "score")

# bifactor model ----
dimensions <- rep(c(1, 2), each = 10)
bifactor_fit <- bfactor(data = depression, model = dimensions, verbose = FALSE)
bifactor_params <- coef(bifactor_fit, simplify = TRUE)
round(head(bifactor_params$items), 4)
bifactor_items <- cbind(MDISC(bifactor_fit),
                        MDIFF(bifactor_fit))
round(head(bifactor_items), 4)
bifactor_params$cov
