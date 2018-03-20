## ----eval = F------------------------------------------------------------
## vignette("equatevignette")

## ----eval=FALSE----------------------------------------------------------
## install.packages("equate")
## library("equate")
## library("hemp")

## ------------------------------------------------------------------------
hcre_data <- as.data.frame(table(hcre$score, hcre$form))
names(hcre_data) <- c("total", "form", "count")
hcre_x <- as.freqtab(hcre_data[hcre_data$form == "x",
                               c("total", "count")],
                     scales = 20:50)
hcre_y <- as.freqtab(hcre_data[hcre_data$form == "y",
                            c("total", "count")], 
                     scales = 20:50)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## rbind(form_x = summary(hcre_x), form_y = summary(hcre_y))

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
rbind(form_x = summary(hcre_x), form_y = summary(hcre_y))

## ----"hcrebar", fig.cap="Bar plot of the test scores on form X in the hcre data set.", fig.pos = "!t", small.mar=TRUE----
plot(hcre_x)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## mean_yx <- equate(hcre_x, hcre_y, type = "mean")
## mean_yx

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mean_yx <- equate(hcre_x, hcre_y, type = "mean")
mean_yx

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## head(mean_yx$concordance)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
head(mean_yx$concordance)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## form_yx <- mean_yx$concordance
## colnames(form_yx)[1] <- "total"
## hcre_xy <- merge(hcre_x, form_yx)
## head(hcre_xy)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
form_yx <- mean_yx$concordance
colnames(form_yx)[1] <- "total"
hcre_xy <- merge(hcre_x, form_yx)
head(hcre_xy)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## linear_yx <- equate(hcre_x, hcre_y, type = "linear")
## linear_yx

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
linear_yx <- equate(hcre_x, hcre_y, type = "linear")
linear_yx

## ------------------------------------------------------------------------
equi_yx <- equate(hcre_x, hcre_y, type = "equipercentile")

## ----"hcrecomp", fig.cap="Scatterplot of the adjusted X scores on form Y against the original form X scores. (The hollow circles correspond to the equipercentile equating and the Xs correspond to linear equating.)", fig.pos = "!t"----
plot(equi_yx$concordance$yx ~ equi_yx$concordance$scale,
     type = "p", xlab = "Form X scores",
     ylab = "Adjusted X Scores on Form Y", ylim = c(20, 55))
points(linear_yx$concordance$yx ~ linear_yx$concordance$scale,
       pch = 4)

## ------------------------------------------------------------------------
negd$total <- rowSums(negd[, 1:25])
negd$anchor <- rowSums(negd[, 26:35])
negd_x <- freqtab(negd[1:1000, c("total", "anchor")],
                  scales = list(0:25, 0:10))
negd_y <- freqtab(negd[1001:2000, c("total", "anchor")],
                  scales = list(0:25, 0:10))

## ----"ngedx_plot", fig.cap="Plot of the common anchor scores against total scores on form X.", fig.pos = "!t"----
plot(negd_x, xlab = "Total Scores Form X",
     ylab = "Common Anchor Scores Form X")

## ------------------------------------------------------------------------
smooth_x <- presmoothing(negd_x, smoothmethod = "loglinear")
smooth_y <- presmoothing(negd_y, smoothmethod = "loglinear")

## ----"ngedx_smooth", fig.cap="Smoothed (loglinear) plot of the common anchor scores against total scores on form X.", fig.pos = "!t"----
plot(smooth_x, xlab = "Total Scores Form X",
     ylab = "Common Anchor Scores Form X")

## ------------------------------------------------------------------------
negd_tucker <- equate(negd_x, negd_y,
                      type = "linear", method = "tucker")
negd_tucker$concordance

