####################################### Study 1 Pilot ###################################

library(tidyverse)
library(lmtest)
library(sandwich)

vc <- function(x) vcovHC(x, type = "HC4")

# Import Data
pilot <- rio::import("Data and Analyses (revised)/Study 1 - Pilot.sav") #added leo (setting link)

pilot$condition <- factor(pilot$condition, levels = c(0, 1 ,2),
                          labels = c("control","mindful-attention", "mindful-gratitude"))

#added leo start (demographics)
nrow(pilot) #N = 569
table(pilot$sex) #313/256
pilot$year <- as.numeric(pilot$year)
table(pilot$year)
pilot$year[pilot$year < 18] <- NA
pilot$year[pilot$year > 76] <- NA
table(pilot$year)
psych::describe(pilot$year)
hist(pilot$year)
table(pilot$condition)

#added leo end (demographics)

# Regressions 

# CN

mod1 <- lm(antiSe ~ condition, pilot)
mod2 <- lm(antiSe ~ (CNSC) * condition, pilot)
summary(mod1)

tab_model(mod1)
waldtest(mod2, vcov = vc) #add leo: this gives the bottom information (Table 1)
waldtest(mod1, mod2, vcov = vc) 

# IN
mod1a <- lm(antiSe ~ condition, pilot)
summary(mod1a)
mod2a <- lm(antiSe ~ (INs) * condition, pilot)
summary(mod2a)

waldtest(mod2a, vcov = vc) 
waldtest(mod1a, mod2a, vcov = vc) 

# ID

mod1b <- lm(antiSe ~ condition, pilot)
summary(mod1b)
mod2b <- lm(antiSe ~ (IGSAT) * condition, pilot)
summary(mod2b)

waldtest(mod2b, vcov = vc) 
waldtest(mod1a, mod2a, vcov = vc) 


# Regression tables

library(sjPlot)

labs1 = c("Collective Narcissism","Mindfulness - Attention","Mindfulness - Gratitude","Collective Narcissism X Attention", "Collective Narcissism X Gratitude")

# CN

sjPlot::tab_model(mod2,  pred.labels = c(labs1),
                  string.est = "b(SE)",
                  #string.std = "β",
                  strings = c(ci = "95%CI LL,UL", stat = "t"),
                  show.se = TRUE, collapse.se = T, 
                  ci.hyphen = "&comma;",
                  title = "",
                  show.intercept = F,
                  dv.labels = c("Model 1"),
                  #show.std = F,
                  show.est = T,
                  show.stat= F,
                  col.order = c("est", "se", "ci", "stat", "p"#, #"std.est", "std.se"
                  ),
                  emph.p = F
                  ,vcov.fun = "HC4"
                  
)
report::report(mod2) #add leo

# IN

labs2 = c("Individual Narcissism","Mindfulness - Attention","Mindfulness - Gratitude","Individual Narcissism X Attention", "Individual Narcissism X Gratitude")

sjPlot::tab_model(mod2a,  pred.labels = c(labs2),
          string.est = "b(SE)",
          #string.std = "β",
          strings = c(ci = "95%CI LL,UL", stat = "t"),
          show.se = TRUE, collapse.se = T, 
          ci.hyphen = "&comma;",
          title = "",
          show.intercept = F,
          dv.labels = c("Model 1"),
          #show.std = F,
          show.est = T,
          show.stat= F,
          col.order = c("est", "se", "ci", "stat", "p"#, #"std.est", "std.se"
          ),
          emph.p = F
          ,vcov.fun = "HC4"
          
)

# ID

labs3 = c("Ingroup identification","Mindfulness - Attention","Mindfulness - Gratitude","Ingroup identification X Attention", "Ingroup identification X Gratitude")

sjPlot::tab_model(mod2b,  pred.labels = c(labs3),
                  string.est = "b(SE)",
                  #string.std = "β",
                  strings = c(ci = "95%CI LL,UL", stat = "t"),
                  show.se = TRUE, collapse.se = T, 
                  ci.hyphen = "&comma;",
                  title = "",
                  show.intercept = F,
                  dv.labels = c("Model 1"),
                  #show.std = F,
                  show.est = T,
                  show.stat= F,
                  col.order = c("est", "se", "ci", "stat", "p"#, #"std.est", "std.se"
                  ),
                  emph.p = F
                  ,vcov.fun = "HC4"
                  
)

c("Ingroup identification","Mindfulness - Attention","Mindfulness - Gratitude","Ingroup identification X Attention", "Ingroup identification X Gratitude")

#add leo: mod3 doesnt seem to exist (maybe this was specified in a different script?)

trends.int <- interactions::sim_slopes(mod3, "CNSC", "condition", robust = "HC4", confint = T, digits = 3)
trends.int

trends.int <- interactions::sim_slopes(mod3, "IGSAT", "condition", robust = "HC4", confint = T, digits = 3)
trends.int


## simple slopes plot


plot <- sjPlot::plot_model(mod2, type = "pred", terms = c("CNSC","condition"), colors = c("black","blue", "red"),title = "", vcov.fun = "HC4") + labs(y = "Anti-Semitism", x = "Collective Narcissism", color = "Manipulation") + theme_classic() + theme(text = element_text(size = 12)) + coord_cartesian(ylim = c(1, 6)) + scale_colour_manual(values = c("black","blue", "red"), labels=c('Control', 'Mindful-Attention Practice', 'Mindful-Gratitude Practice')) + scale_fill_manual(values = c("black","blue", "red"))
plot 


##################################################

#add leo: reproduction Table 1 (start)
#this is the original code, just put in more readable order, except stated otherwise

##this partially reproduces model 1 in Table 1
summary(mod1) #estimates are equal, p-values and CIs differ slightly (not meaningful, but unclear why at all)

##this is to recreate the last line of model 1 (was not in code, self-written)
waldtest(mod1, vcov = vc) #add leo

##this reproduces the main model 2 in Table 1
sjPlot::tab_model(mod2,  pred.labels = c(labs1),
                  string.est = "b(SE)",
                  #string.std = "β",
                  strings = c(ci = "95%CI LL,UL", stat = "t"),
                  show.se = TRUE, collapse.se = T, 
                  ci.hyphen = "&comma;",
                  title = "",
                  show.intercept = F,
                  dv.labels = c("Model 1"),
                  #show.std = F,
                  show.est = T,
                  show.stat= F,
                  col.order = c("est", "se", "ci", "stat", "p"#, #"std.est", "std.se"
                  ),
                  emph.p = F
                  ,vcov.fun = "HC4"
                  
)

##this creates the information for second-lowest line of model 2 in table 1
waldtest(mod2, vcov = vc)

##open points:
#no idea where last line of model 2 comes from (sign. test on R2 change)
#I dont see any bootstrapping in the code

#remarks:
#this was surprisingly complicated!

#add leo: reproduction Table 1 (end)

##################################################

#add leo: reproduction Table 2 (start)

##I didnt find any code for this
##the definition of mod3 is missing in the code, maybe that was the respective model?

#add leo: reproduction Table 2 (end)

##################################################

#add leo: reproduction Figure 1 (start)

##this worked perfectly
plot <- sjPlot::plot_model(mod2, type = "pred", terms = c("CNSC","condition"), colors = c("black","blue", "red"),title = "", vcov.fun = "HC4") + labs(y = "Anti-Semitism", x = "Collective Narcissism", color = "Manipulation") + theme_classic() + theme(text = element_text(size = 12)) + coord_cartesian(ylim = c(1, 6)) + scale_colour_manual(values = c("black","blue", "red"), labels=c('Control', 'Mindful-Attention Practice', 'Mindful-Gratitude Practice')) + scale_fill_manual(values = c("black","blue", "red"))
plot 

#add leo: reproduction Figure 1 (end)

