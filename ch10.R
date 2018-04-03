## -------------------------------------------------------------------
## Chapter 10 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------
install.packages("equate")
library("equate")
library("hemp")

vignette("equatevignette")

# equating designs
hcre_data <- as.data.frame(table(hcre$score, hcre$form))
names(hcre_data) <- c("total", "form", "count")
hcre_x <- as.freqtab(hcre_data[hcre_data$form == "x",
                               c("total", "count")],
                     scales = 20:50)
hcre_y <- as.freqtab(hcre_data[hcre_data$form == "y",
                            c("total", "count")],
                     scales = 20:50)
rbind(form_x = summary(hcre_x), form_y = summary(hcre_y))
plot(hcre_x)
mean_yx <- equate(hcre_x, hcre_y, type = "mean")
mean_yx
head(mean_yx$concordance)
form_yx <- mean_yx$concordance
colnames(form_yx)[1] <- "total"
hcre_xy <- merge(hcre_x, form_yx)
head(hcre_xy)
linear_yx <- equate(hcre_x, hcre_y, type = "linear")
linear_yx

# nonlinear function ----
equi_yx <- equate(hcre_x, hcre_y, type = "equipercentile")
plot(equi_yx$concordance$yx ~ equi_yx$concordance$scale,
     type = "p", xlab = "Form X scores",
     ylab = "Adjusted X Scores on Form Y", ylim = c(20, 55))
points(linear_yx$concordance$yx ~ linear_yx$concordance$scale,
       pch = 4)

# nonequivalent group design ----
negd$total <- rowSums(negd[, 1:25])
negd$anchor <- rowSums(negd[, 26:35])
negd_x <- freqtab(negd[1:1000, c("total", "anchor")],
                  scales = list(0:25, 0:10))
negd_y <- freqtab(negd[1001:2000, c("total", "anchor")],
                  scales = list(0:25, 0:10))
plot(negd_x, xlab = "Total Scores Form X",
     ylab = "Common Anchor Scores Form X")

# presmoothing
smooth_x <- presmoothing(negd_x, smoothmethod = "loglinear")
smooth_y <- presmoothing(negd_y, smoothmethod = "loglinear")
plot(smooth_x, xlab = "Total Scores Form X",
     ylab = "Common Anchor Scores Form X")

# linear tucker equating ----
negd_tucker <- equate(negd_x, negd_y,
                      type = "linear", method = "tucker")
negd_tucker$concordance
