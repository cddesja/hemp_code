## -------------------------------------------------------------------
## Chapter 8 R code
## Handbook of Educational Measurement and Psychometrics Using R
## C. D. Desjardins & O. Bulut
## -------------------------------------------------------------------
library("hemp")
library("lme4")

# reshape data ----
mc_long <- reshape(multiplechoice,
                   varying = 1:27,
                   direction = "long",
                   timevar = "item",
                   idvar = "id",
                   v.names = "response")
mc_long <- mc_long[order(mc_long$id, mc_long$item), ]
head(mc_long)

# Rasch model ----
head(eirm, 10)
table(eirm$item, eirm$itemtype)
control <- glmerControl(optimizer = "bobyqa",
                     optCtrl = list(maxfun = 100000))
rasch_mod <- glmer(response ~ -1 + item + (1 | person),
                     family = binomial, data = eirm,
                     control = control)
rasch_params <- data.frame(easiness = fixef(rasch_mod))
rasch_params
rasch_ability <- coef(rasch_mod)$person[, 1]
difficulty <- rasch_params$easiness
names(difficulty) <- 1:10
itemperson_map(difficulty, rasch_ability, n = 50)

# linear logistic test model ----
lltm_mod <- glmer(response ~ -1 + itemtype + (1 | person),
                    family = binomial, data = eirm,
                    control = control)
summary(lltm_mod)

lltme_mod <- glmer(response ~ -1 + itemtype + (1 | person) + (1 | item),
                   family = binomial, data = eirm, control = control)
summary(lltme_mod)

anova(rasch_mod, lltm_mod)
anova(rasch_mod, lltme_mod)

# latent regression rasch model ----
lrrm_mod <- glmer(response ~ -1 + item + gender + (1 | person),
                  family = binomial, data = eirm,
                  control = control)
summary(lrrm_mod)
                  
lrrme_mod <- glmer(response ~ -1 + item + gender +
                     (gender | person), family = binomial,
                     data = eirm, control = control)
summary(lrrme_mod)
anova(rasch_mod, lrrm_mod)
anova(lrrm_mod, lrrme_mod)

# interaction models ----
int_mod <- glmer(response ~ -1 + itemtype * gender +
                  (1 | person) + (1 | item), family = binomial,
                  data = eirm, control = control)
summary(int_mod)

dif_mod <- glmer(response ~ -1 + item * gender +
                 (1 | person), family = binomial,
                 data = eirm, control = control)
summary(dif_mod)
dif10 <- with(eirm,
              ifelse(gender == "M" & item == "10", 1, 0))
dif10_mod <- glmer(response ~ -1 + item + gender +
                    dif10 + (1 | person), family = binomial,
                     data = eirm, control = control)
anova(rasch_mod, dif10_mod)
