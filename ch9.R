## ----eval = F------------------------------------------------------------
## install.packages("faoutlier")
## library("faoutlier")
## library("hemp")

## ----echo = F------------------------------------------------------------
library("faoutlier")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## cognitive <- interest[, c(4:9)]
## D2 <- robustMD(cognitive)
## plot(D2, ylab = "D2")

## ----"md_plot", fig.cap="Mahalanobis distance plotted against the observation identifier.", echo = F, fig.pos = "t!"----
cognitive <- interest[, c(4:9)]
D2 <- robustMD(cognitive)
plot(D2, main = " ", ylab = "D2", col = "black")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## D2$mah[which.max(D2$mah)]

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
D2$mah[which.max(D2$mah)]

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## cognitive[202,]

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
cognitive[202,]

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## apply(cognitive, 2, max)

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
apply(cognitive, 2, max)

## ------------------------------------------------------------------------
cog_mod <- "
verb =~ vocab + reading + sentcomp
math =~ mathmtcs + geometry + analyrea
"

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## gd <- gCD(interest, cog_mod)
## plot(gd, ylab = "Generalized Cook's D")

## ----"cooks_d", cache = T, fig.cap="Generalized Cook's distance plotted against observation ID.", echo = F, fig.pos = "t!"----
gd <- gCD(interest, cog_mod)
plot(gd, main = " ", col = "black", ylab = "Generalized Cook's D")

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## gd$gCD[which.max(gd$gCD)]

## ----message = FALSE, verbose = FALSE, echo = FALSE, background = "gray95"----
gd$gCD[which.max(gd$gCD)]

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## cog_resid <- obs.resid(interest, cog_mod)
## cogn_std <- data.frame(cog_resid$std_res)
## cogn_std_l <- reshape(cogn_std,
##                       direction = "long",
##                       varying = 1:6,
##                       v.names = "std_res",
##                       timevar = "var",
##                       times = names(cogn_std))
## qqmath(~ std_res | var, cogn_std_l,
##        xlab = "Normal theoretical quantiles",
##        ylab = "Standardized Residuals",
##        panel = function(x, ...){
##          panel.qqmathline(x, ...)
##          panel.qqmath(x, ...)
##          }, auto.key = T)

## ----"std_residual", cache=FALSE, fig.cap="Normal Q--Q plot comparing standardized residuals for the six manifest variables against normal theoretical quantiles.", echo = F, fig.pos = "t!"----
cognitive <- interest[, c(4:9)]
cog_resid <- obs.resid(cognitive, cog_mod)
cogn_std <- data.frame(cog_resid$std_res)
cogn_std_l <- reshape(cogn_std,
                      direction = "long",
                      varying = 1:6,
                      v.names = "std_res",
                      timevar = "var",
                      times = names(cogn_std))
qqmath(~ std_res | var, cogn_std_l, col = "black",
       xlab = "Normal theoretical quantiles",
       ylab = "Standardized Residuals",
       panel = function(x, ...){
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
         },
       par.settings = list(strip.background = list(col = "white")),
                           auto.key = T)

## ----<-------------------------------------------------------------------
cog_fit <- cfa(cog_mod, interest)
cog_pred <- data.frame(predict(cog_fit))

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## cognitive_l <- reshape(cognitive,
##                       direction = "long",
##                       varying = 1:6,
##                       v.names = "obs_score",
##                       timevar = "var",
##                       times = names(cognitive))
## cognitive_l$pred_fact <- c(rep(cog_pred$v, 3),
##                           rep(cog_pred$m, 3))
## xyplot(obs_score ~ pred_fact | var, cognitive_l,
##        xlab = "Factor Score",
##        ylab = "Manifest Variable",
##        auto.key = T)

## ----"resid_fact", cache=FALSE, fig.cap="Manifest variables plotted against the estimated factor scores.", echo = F, fig.pos = "t!"----
cognitive_l <- reshape(cognitive,
                      direction = "long",
                      varying = 1:6,
                      v.names = "obs_score",
                      timevar = "var",
                      times = names(cognitive))
cognitive_l$pred_fact <- c(rep(cog_pred$v, 3),
                          rep(cog_pred$m, 3))
xyplot(obs_score ~ pred_fact | var, cognitive_l, col = "black",
       par.settings = list(strip.background = list(col = "white")),
       xlab = "Factor Score",
       ylab = "Manifest Variable",
       auto.key = T)

## ----message = FALSE, verbose = FALSE, eval = FALSE----------------------
## plot(cog_resid, restype = "std_res")

## ----"std_residual_by_id", fig.cap="Standardized residuals against ids by the six manifest variables.", echo = F, fig.pos = "t!"----
plot(cog_resid, restype = "std_res", col = "black",
     par.settings = list(strip.background = list(col = "white")))

## ------------------------------------------------------------------------
outliers <- which(abs(cogn_std_l$std_res) > 3)
cogn_std_l[outliers, ]

## ----eval=FALSE----------------------------------------------------------
## install.packages("semPlot")
## library("semPlot")

## ----echo=FALSE----------------------------------------------------------
library("semPlot")

## ----"path1", cache = T, fig.cap="Path diagram for the two-factor cognitive model.", fig.pos = "t!"----
semPaths(cog_fit, what = "std", residuals = F,
         rotation = 4, color = "black",
         edge.color = "black",
         edge.label.cex = 1)

## ----eval = F------------------------------------------------------------
## configural <- cfa(cog_mod, interest, group = "gender")
## semPaths(configural, what = "est",
##          edge.label.cex = 1, panelGroups = T,
##          ask = FALSE, fade = FALSE, title = FALSE,
##          color = "black",  edge.color = "black")

## ----"multigroup", cache = T, fig.cap="Path diagram of the multi-group, configural model (female on the left; male on the right) for the two-factor cognitive model.", echo = F, fig.pos = "t!"----
configural <- cfa(cog_mod, interest, group = "gender")
semPaths(configural, what = "est", color = "black",  edge.color = "black",
         edge.label.cex = 1, panelGroups = T,
         ask = FALSE, fade = FALSE, title = FALSE)

## ----eval=F--------------------------------------------------------------
## # Load the required R packages
## library("shiny")
## library("lavaan")
## 
## # Shiny ui's can be drawn using a grid system
## # consisting of 12 columns wide. We make use of this below
## # by using the first 4 columns to contain the inputs (widgets)
## # and the remaining 8 columns to hold the plot.
## 
## ui <- fluidPage(
##   column(4,
##   # This creates the input for setting our factor loading
##   # for manifest variable one. The default value is set to
##   # 0.5 and if we click the arrows, it will increase/decrease
##   # by 0.5
##   numericInput("lam",
##     "Value for the factor loading for manifest variable 1.",
##     value = .5,
##     step = .5),
## 
##   # This creates the checkbox for a non-linear
##   # quadratic relationship. When the box is unchecked,
##   # the relationship is linear.
##   checkboxInput("nonlinear",
##     "Should manifest variable 1 have a non-linear
##       relationship with the factor?",
##     value = F)
##   ),
##   column(8,
##   # This will draw our diagnostic plot
##   plotOutput("diagPlot")
##   )
## )
## 
## # Below we define a server function
## # Every time we refresh the input random
## # data will be created. So, no two plots will
## # be identical!
## 
## server <- function(input, output) {
##   output$diagPlot <- renderPlot({
## 
##   # Create our factor as a standard random normal variable.
##   # rnorm() ensures every time the plots are different.
##   f1 <- rnorm(500)
## 
##   # If we specify the non-linear relationship
##   # then f1 will be squared else it won't be.
##   # We also add some specific variance/uniqueness
##   # (the rnorm part at the end).
##   if(input$nonlinear) {
##     m1 <- input$lam * f1 + 2 * f1^2 + rnorm(500)
##   } else {
##     m1 <- input$lam * f1 + rnorm(500)
##   }
## 
##   # Now do the same thing for manifest variables 2 - 4,
##   # except their relationship with the factor is always
##   # fixed.
##   # The .60, .75, and .9 are the factor loadings
##   # The rnorm() adds specific/unique variance
##   m2 <- .60 * f1 + rnorm(500)
##   m3 <- .75 * f1 + rnorm(500)
##   m4 <- .9 * f1 + rnorm(500)
##   dat <- data.frame(m1, m2, m3, m4)
## 
##   # Define the CFA model, run it, and extract the factor scores
##   mod <- '
##     fact =~ m1 + m2 + m3 + m4
##     '
##   fit <- cfa(mod, dat, std.lv = T)
##   dat$pred <- predict(fit)[,1]
## 
##   # Create a 2 by 2 plot (the par stuff) and
##   # add a blue LOWESS curve to each of the manifest
##   # variables against the est. factor scores sub plots.
##   par(mfrow = c(2,2))
##   for(i in 1:4){
##     plot(dat[,i] ~ pred, dat, xlab = "Est. Factor Score",
##          ylab = paste("Manifest Var", i))
##     lines(lowess(dat$pred, dat[,i]), col = "blue")
##   }
##   })
## }

## ----eval=FALSE----------------------------------------------------------
## shinyApp(ui = ui, server = server)

## ----eval = F------------------------------------------------------------
## fadiag_demo()

## ----eval = F------------------------------------------------------------
## # Load the shiny package
## library("shiny")
## 
## # Define the 3PL function
## threepl <- function(person, b, a, c) {
##   x <- c + (1 - c)*(exp(a * (person - b)) /
##                     (1 + exp(a * (person - b))))
##   return(x)
## }
## 
## # Define the range of abilities, difficulties,
## # discriminations, and guessing parameters allowed
## ability <- seq(-3, 3, by = .1)
## diff <- -3:3
## discr <- -2:2
## guess <- 0:10/10
## 
## # Create a data.frame with all the combinations
## # of the values permitted above
## parameter_setup <- expand.grid(person = ability,
##                                b = diff, a = discr, c = guess)
## # Run the threepl() function and save the probabilities
## # of getting an item correct as function of these parameters
## parameter_setup$prob <- threepl(person = parameter_setup$person,
##                          b = parameter_setup$b,
##                          a = parameter_setup$a,
##                          c = parameter_setup$c)
## 
## # Define the user-interface (ui) function
## # Will consists of three inputs
## #
## # - A slider to set item difficulty (b)
## # - A slider to set item discrimination (c)
## # - A slider to set guessing (a)
## #
## # and the plot (called threepl).
## #
## # Note: when a = 1 and c = 0, this the Rasch
## # and when c = 0, this is the 2PL
## #
## # Shiny ui's can be drawn using a grid system
## # consisting of 12 columns wide.We make use of this below
## # by using the first 4 columns to contain the widgets
## # and the remaining 8 columns to hold the plot.
## #
## ui <- fluidPage(
##   column(4,
##   sliderInput("b", label = "Item Difficulty",
##               min = -3, max = 3, value = 0, step = 1),
##   sliderInput("a", label = "Item Discrimination",
##               min = -2, max = 2, value = 1, step = 1),
##   sliderInput("c", label = "Guessing",
##               min = 0, max = 1, value = 0, step = .1)),
##   column(8,
##   plotOutput("threepl"))
## )
## 
## # Below we define a server function that
## # subset our data based on the values of the sliders
## # and then plot the IRF.
## server <- function(input, output){
##     output$threepl <- renderPlot({
##       plot.data <- subset(parameter_setup, b == input$b &
##                             a == input$a &
##                             c == input$c)
##       plot(prob ~ person, plot.data, type = "l",
##            xlab = expression(theta),
##            ylab = "Pr(Y = 1)",
##            ylim = c(0, 1))
##     })
## }

## ----eval=FALSE----------------------------------------------------------
## shinyApp(ui = ui, server = server)

## ----eval = F------------------------------------------------------------
## threepl_demo()

