## ------------------------------------------------------------------------
library("hemp")

## ----eval = F------------------------------------------------------------
## onefacet_mod <- lmer(Score ~ (1 | Participants) + (1 | Items),
##                      data = efData)
## onefacet <- gstudy(onefacet_mod)
## onefacet

## ----eval = T, echo = F, background = "gray95"---------------------------
onefacet_mod <- lmer(Score ~ (1 | Participants) + (1 | Items),
	data = efData)
onefacet <- gstudy(onefacet_mod)
onefacet

## ----eval = F------------------------------------------------------------
## library("lattice")
## scores <- aggregate(efData$Score,
##                     by = list(efData$Participants), mean)
## histogram(~ x, scores, type = "count",
##           xlab = "Proportion of Items Correct")

## ----"hist_one", fig.cap="Histogram of the percent of items correct on the EF instrument.", echo = F----
library(lattice)
scores <- aggregate(efData$Score,
                    by = list(efData$Participants), mean)
histogram(~ x, scores, type = "count",
          xlab = "Proportion of Items Correct",
          col = "white")

## ----eval = F------------------------------------------------------------
## item_means <- aggregate(efData$Score,
##                         by = list(efData$Items), mean)
## colnames(item_means) <- c("Item", "Mean")
## item_means

## ----eval = T, echo = F, background = "gray95"---------------------------
item_means <- aggregate(efData$Score,
                        by = list(efData$Items), mean)
colnames(item_means) <- c("Item", "Mean")
item_means

## ----eval = F------------------------------------------------------------
## dstudy(onefacet, unit = "Participants", n = c("Items" = 1))

## ----eval = T, echo = F, background = "gray95"---------------------------
dstudy(onefacet, unit = "Participants", n = c("Items" = 1))

## ----eval = F------------------------------------------------------------
## onefacet_dstudy <- dstudy(onefacet,
##                      unit = "Participants",
##                      n = c("Items" = 10))
## onefacet_dstudy

## ----eval = T, echo = F, background = "gray95"---------------------------
onefacet_dstudy <- dstudy(onefacet,
                     unit = "Participants",
                     n = c("Items" = 10))
onefacet_dstudy

## ----eval = F------------------------------------------------------------
## onefacet_dstudy$relvar

## ----eval = T, echo = F, background = "gray95"---------------------------
onefacet_dstudy$relvar

## ----eval = F------------------------------------------------------------
## onefacet_dstudy$absvar

## ----eval = T, echo = F, background = "gray95"---------------------------
onefacet_dstudy$absvar

## ----eval = F------------------------------------------------------------
## dstudy_plot(onefacet, unit = "Participants",
##             facets = list(Items = c(10, 20, 30, 40, 50, 60)),
##             g_coef = FALSE)

## ----"onefacet", fig.cap="Plot of dependability coefficient against number of EF items for the one-facet random design.", echo = F----
dstudy_plot(onefacet, unit = "Participants",
            facets = list(Items = c(10, 20, 30, 40, 50, 60)),
            g_coef = F,
            bw = T)

## ------------------------------------------------------------------------
twofacet_mod <- lmer(scores ~ (1 | students) + (1  | prompts) +
                    (1 | raters) + (1 | students:prompts) +
                    (1 | students:raters) +
                    (1 | prompts:raters), 
                    data = writing)

## ----eval = F------------------------------------------------------------
## twofacet <- gstudy(twofacet_mod)
## twofacet

## ----eval = T, echo = F, background = "gray95"---------------------------
twofacet <- gstudy(twofacet_mod)
twofacet

## ----eval = F------------------------------------------------------------
## dstudy(twofacet, n = c("raters" = 2, "prompts" = 5),
##        unit = "students")

## ----eval = T, echo = F, background = "gray95"---------------------------
dstudy(twofacet, n = c("raters" = 2, "prompts" = 5),
       unit = "students")

## ----eval = F------------------------------------------------------------
## dstudy_plot(twofacet, unit = "students",
##             facets = list(prompts = 3:8,
##                           raters = 1:5))

## ----"twofacetx", fig.cap="Plot of generalizability coefficient against number of writing prompts by number of raters.", echo = F----
dstudy_plot(twofacet, unit = "students",
            facets = list(prompts = 3:8,
                          raters = 1:5), 
            bw = T)

## ------------------------------------------------------------------------
nested_model <- lmer(scores ~ (1 | students/raters) +
                    (1  | prompts) +
                    (1 | students:prompts),
                    data = writing2)
nested <- gstudy(nested_model)
nested

## ----eval = F------------------------------------------------------------
## dstudy(nested, n = c("raters" = 2, "prompts" = 5),
##        unit = "students")

## ----eval = T, echo = F, background = "gray95"---------------------------
dstudy(nested, n = c("raters" = 2, "prompts" = 5),
       unit = "students")

## ----eval = F------------------------------------------------------------
## twofacet_fixed <- gstudy(twofacet_mod, fixed = "prompts")
## twofacet_fixed

## ----eval = T, echo = F, background = "gray95"---------------------------
twofacet_fixed <- gstudy(twofacet_mod, fixed = "prompts")
twofacet_fixed

## ----eval = F------------------------------------------------------------
## dstudy(twofacet_fixed, n = c("raters" = 2, "prompts" = 5),
##        unit = "students")

## ----eval = T, echo = F, background = "gray95"---------------------------
dstudy(twofacet_fixed, n = c("raters" = 2, "prompts" = 5),
       unit = "students")

