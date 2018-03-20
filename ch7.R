## ----eval = F------------------------------------------------------------
## library("hemp")
## library("mirt")

## ------------------------------------------------------------------------
m2pl_mod <- 'F1 = 1 - 6, 13 - 21, 23 - 24
             F2 = 7 - 20, 22 - 24
             COV = F1 * F2'

## ----message = FALSE, verbose = FALSE, eval = F--------------------------
## m2pl_fit <- mirt(data = mimic, model = m2pl_mod,
##                  itemtype = "2PL", method = "EM",
##                  SE = TRUE)
## m2pl_params <- coef(m2pl_fit, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, eval = T, echo = F----------------
m2pl_fit <- mirt(data = mimic, model = m2pl_mod,
                 itemtype = "2PL", method = "EM",
                 SE = TRUE, verbose = F)
m2pl_params <- coef(m2pl_fit, simplify=TRUE)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## head(m2pl_params$items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
round(head(m2pl_params$items), 4)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_items <- data.frame(MDISC(m2pl_fit),
##                          MDIFF(m2pl_fit))
## colnames(m2pl_items) <- c("m2pl_mdisc", "m2pl_mdiff")
## head(m2pl_items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
m2pl_items <- data.frame(MDISC(m2pl_fit),
                         MDIFF(m2pl_fit))
colnames(m2pl_items) <- c("m2pl_mdisc", "m2pl_mdiff")
head(m2pl_items)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_params$cov

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
m2pl_params$cov

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## itemplot(m2pl_fit, type = "trace", item = 13)
## itemplot(m2pl_fit, type = "tracecontour", item = 13)

## ----m2pl2, message = FALSE, echo = FALSE, fig.cap="Item contour plot for item13 for the M2PL model.", fig.align = "center", fig.pos = "t!"----
m2pl.2 <- itemplot(m2pl_fit, type="tracecontour", item = 13, main="")
update(m2pl.2, legend = NULL)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## itemplot(m2pl_fit, type="info", item = 13)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## itemplot(m2pl_fit, type = "infocontour", item = 13)
## itemplot(m2pl_fit, type = "score", item = 13)
## itemplot(m2pl_fit, type = "SE", item = 13)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## plot(m2pl_fit, type = "info")
## plot(m2pl_fit, type = "SE")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_map <- fscores(m2pl_fit, method = "MAP", full.scores = TRUE,
##                     full.scores.SE = TRUE)
## head(m2pl_map)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
m2pl_map <- fscores(m2pl_fit, method = "MAP", full.scores = TRUE, 
                    full.scores.SE = TRUE)
head(m2pl_map)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_eap <- fscores(m2pl_fit, method =  "EAP",
##                     full.scores = TRUE,
##                     full.scores.SE = TRUE)
## head(m2pl_eap)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
m2pl_eap <- fscores(m2pl_fit, method =  "EAP",
                    full.scores = TRUE, 
                    full.scores.SE = TRUE)
head(m2pl_eap)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_scores <- data.frame(map1 = m2pl_map[, 1],
##                           map2 = m2pl_map[, 2],
##                           eap1 = m2pl_eap[, 1],
##                           eap2 = m2pl_eap[, 2])
## cor(m2pl_scores)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
m2pl_scores <- data.frame(map1 = m2pl_map[, 1], 
                          map2 = m2pl_map[, 2], 
                          eap1 = m2pl_eap[, 1], 
                          eap2 = m2pl_eap[, 2])
round(cor(m2pl_scores), 4)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_mod_constrant <- '
##              F1 = 1 - 6, 13 - 21, 23 - 24
##              F2 = 7 - 20, 22 - 24
##              COV = F1 * F2
##              CONSTRAIN = (1 - 6, 21, a1), (7 - 12, 22, a2)'

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## m2pl_fit <- mirt(data = mimic, model = m2pl_mod,
##                      itemtype = "PC2PL", SE = TRUE)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## mrasch_mod <- 'F1 = 1 - 6, 13 - 21, 23 - 24
##                F2 = 7 - 20, 22 - 24
##                COV = F1 * F2'
## mrasch_fit <- mirt(data = mimic, model = mrasch_mod,
##                        itemtype = "Rasch", SE = TRUE)
## mrasch_params <- coef(mrasch_fit, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, echo = FALSE----------------------
mrasch_mod <- 'F1 = 1 - 6, 13 - 21, 23 - 24
               F2 = 7 - 20, 22 - 24
               COV = F1 * F2'
mrasch_fit <- mirt(data = mimic, model = mrasch_mod,
                       itemtype = "Rasch", SE = TRUE, verbose = FALSE)
mrasch_params <- coef(mrasch_fit, simplify = TRUE)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## head(mrasch_params$items, 10)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
head(mrasch_params$items, 10)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## mrasch_mdiff <- MDIFF(mrasch_fit)
## head(mrasch_mdiff)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mrasch_mdiff <- MDIFF(mrasch_fit)
head(mrasch_mdiff)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## mrasch_params$cov

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mrasch_params$cov

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## itemplot(mrasch_fit, type = "trace", item = 13)
## itemplot(mrasch_fit, type = "tracecontour", item = 13)
## plot(mrasch_fit, type = "info")
## plot(mrasch_fit, type = "SE")
## plot(mrasch_fit, type = "score")

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## mgrm_mod <- 'Cognitive = 1 - 10
##                Somatic = 11 - 20
##                COV = Cognitive * Somatic'
## mgrm_fit <- mirt(data = depression, model = mgrm_mod,
##                      itemtype = "graded", SE = TRUE)
## mgrm_params <- coef(mgrm_fit, simplify = TRUE)
## head(mgrm_params$items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mgrm_mod <- 'Cognitive = 1 - 10
               Somatic = 11 - 20
               COV = Cognitive * Somatic'
mgrm_fit <- mirt(data = depression, model = mgrm_mod,
                     itemtype = "graded", SE = TRUE, verbose = FALSE)
mgrm_params <- coef(mgrm_fit, simplify = TRUE)
round(head(mgrm_params$items), 4)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## mgrm_items <- cbind(MDISC(mgrm_fit),
##                     MDIFF(mgrm_fit))
## head(mgrm_items)
## 

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mgrm_items <- cbind(MDISC(mgrm_fit),
                    MDIFF(mgrm_fit))
round(head(mgrm_items), 4)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## mgrm_params$cov

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mgrm_params$cov

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## itemplot(mgrm_fit, type = "trace", item = 7)
## itemplot(mgrm_fit, type = "trace", item = 13)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## plot(mgrm_fit, type = "info")
## plot(mgrm_fit, type = "SE")
## plot(mgrm_fit, type = "score")

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## dimensions <- rep(c(1, 2), each = 10)
## bifactor_fit <- bfactor(data = depression, model = dimensions)
## bifactor_params <- coef(bifactor_fit, simplify = TRUE)
## head(bifactor_params$items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
dimensions <- rep(c(1, 2), each = 10)
bifactor_fit <- bfactor(data = depression, model = dimensions, verbose = FALSE)
bifactor_params <- coef(bifactor_fit, simplify = TRUE)
round(head(bifactor_params$items), 4)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## bifactor_items <- cbind(MDISC(bifactor_fit),
##                         MDIFF(bifactor_fit))
## head(bifactor_items)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
bifactor_items <- cbind(MDISC(bifactor_fit),
                        MDIFF(bifactor_fit))
round(head(bifactor_items), 4)

## ----message = FALSE, verbose = FALSE, eval=FALSE------------------------
## bifactor_params$cov

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
bifactor_params$cov

