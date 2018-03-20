## ----message = FALSE, verbose = FALSE------------------------------------
mc_long <- reshape(multiplechoice, 
                   varying = 1:27,
                   direction = "long",
                   timevar = "item",
                   idvar = "id",
                   v.names = "response")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## mc_long <- mc_long[order(mc_long$id, mc_long$item), ]
## head(mc_long)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
mc_long <- mc_long[order(mc_long$id, mc_long$item), ]
head(mc_long)

## ------------------------------------------------------------------------
library("hemp")
library("lme4")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## head(eirm, 10)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
head(eirm, 10)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## table(eirm$item, eirm$itemtype)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
table(eirm$item, eirm$itemtype)

## ----message = FALSE, verbose = FALSE------------------------------------
control <- glmerControl(optimizer = "bobyqa", 
                     optCtrl = list(maxfun = 100000))

## ----message = FALSE, verbose = FALSE------------------------------------
rasch_mod <- glmer(response ~ -1 + item + (1 | person), 
                     family = binomial, data = eirm, 
                     control = control)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## rasch_params <- data.frame(easiness = fixef(rasch_mod))
## rasch_params

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
rasch_params <- data.frame(easiness = fixef(rasch_mod))
rasch_params

## ------------------------------------------------------------------------
rasch_ability <- coef(rasch_mod)$person[, 1]

## ----raschlme4, message = FALSE, fig.cap="Item-person map for the Rasch model.", fig.align = "center", fig.height = 3.5, fig.pos = "!ht"----
difficulty <- rasch_params$easiness
names(difficulty) <- 1:10
itemperson_map(difficulty, rasch_ability, n = 50)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## lltm_mod <- glmer(response ~ -1 + itemtype + (1 | person),
##                     family = binomial, data = eirm,
##                     control = control)
## summary(lltm_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## lltme_mod <- glmer(response ~ -1 + itemtype + (1 | person) +
##                      (1 | item), family = binomial, data = eirm,
##                      control = control)
## summary(lltme_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## anova(rasch_mod, lltm_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## anova(rasch_mod, lltme_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## lrrm_mod <- glmer(response ~ -1 + item + gender + (1 | person),
##                     family = binomial, data = eirm,
##                     control = control)
## summary(lrrm_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## lrrme_mod <- glmer(response ~ -1 + item + gender +
##                      (gender | person), family = binomial,
##                      data = eirm, control = control)
## summary(lrrme_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## anova(rasch_mod, lrrm_mod)
## anova(lrrm_mod, lrrme_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## int_mod <- glmer(response ~ -1 + itemtype * gender +
##                   (1 | person) + (1 | item), family = binomial,
##                   data = eirm, control = control)
## summary(int_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## dif_mod <- glmer(response ~ -1 + item * gender +
##                  (1 | person), family = binomial,
##                  data = eirm, control = control)
## summary(dif_mod)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## dif10 <- with(eirm,
##               ifelse(gender == "M" & item == "10", 1, 0))
## dif10_mod <- glmer(response ~ -1 + item + gender +
##                     dif10 + (1 | person), family = binomial,
##                      data = eirm, control = control)
## anova(rasch_mod, dif10_mod)

