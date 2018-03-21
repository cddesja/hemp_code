## -------------------------------------------------------------------
## Chapter 3 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------

library("hemp")

# one facet design ----
onefacet_mod <- lmer(Score ~ (1 | Participants) + (1 | Items),
    data = efData)
onefacet <- gstudy(onefacet_mod)
onefacet
library(lattice)
scores <- aggregate(efData$Score,
                    by = list(efData$Participants), mean)
histogram(~ x, scores, type = "count",
          xlab = "Proportion of Items Correct",
          col = "white")
item_means <- aggregate(efData$Score,
                        by = list(efData$Items), mean)
colnames(item_means) <- c("Item", "Mean")
item_means
dstudy(onefacet, unit = "Participants", n = c("Items" = 1))
onefacet_dstudy <- dstudy(onefacet,
                     unit = "Participants",
                     n = c("Items" = 10))
onefacet_dstudy
onefacet_dstudy$relvar
onefacet_dstudy$absvar
dstudy_plot(onefacet, unit = "Participants",
            facets = list(Items = c(10, 20, 30, 40, 50, 60)),
            g_coef = F,
            bw = T)

# two facet crossed design ----
twofacet_mod <- lmer(scores ~ (1 | students) + (1 | prompts) +
                    (1 | raters) + (1 | students:prompts) +
                    (1 | students:raters) +
                    (1 | prompts:raters),
                    data = writing)
twofacet <- gstudy(twofacet_mod)
twofacet
dstudy(twofacet, n = c("raters" = 2, "prompts" = 5),
       unit = "students")
dstudy_plot(twofacet, unit = "students",
            facets = list(prompts = 3:8,
                          raters = 1:5),
            bw = T)

# two facet nested design ----
nested_model <- lmer(scores ~ (1 | students / raters) +
                    (1  | prompts) +
                    (1 | students:prompts),
                    data = writing2)
nested <- gstudy(nested_model)
nested
dstudy(nested, n = c("raters" = 2, "prompts" = 5),
       unit = "students")

## two facet study with a fixed facet ----
twofacet_fixed <- gstudy(twofacet_mod, fixed = "prompts")
twofacet_fixed
dstudy(twofacet_fixed, n = c("raters" = 2, "prompts" = 5),
       unit = "students")
