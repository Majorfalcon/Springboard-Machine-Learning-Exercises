## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
eng.by.met <- subset(states.data, select = c("energy", "metro"))
summary(eng.by.met)
cor(eng.by.met, use = "pairwise")
plot(eng.by.met)
##   2. Print and interpret the model `summary'
model.0 <- lm(energy ~ metro, data = na.omit(states.data))
summary(model.0)
##   3. `plot' the model to look for deviations from modeling assumptions
plot(model.0)
confint(model.0)
##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

eng.by.met.tox.green <- subset(states.data, select = c("energy", "metro", "toxic", "green"))
summary(eng.by.met.tox.green)
cor(eng.by.met.tox.green, use = "pairwise")
plot(eng.by.met.tox.green)

model.1 <- lm(energy ~ metro + toxic + green, data = na.omit(states.data))
summary(model.1)
plot(model.1)
anova(model.0, model.1)



## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
model.2 <- lm(energy ~ metro + toxic + green + toxic*green, data = na.omit(states.data))
summary(model.2)

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
str(states.data$region)
states.data$region <- factor(states.data$region)

model.1.reg <- lm(energy ~ metro + toxic + green + region, data = na.omit(states.data))
summary(model.1.reg)
anova(model.1.reg)