
# Fishing Data Analysis for STAT 649 Activity 3 Client Report ------------------


# Insights into the Fishing Data -----------------------------------------------

## loading the data

library(readr)
fishing = read_csv("Docs/SPRING '25/STAT 649/HW3.pdf/fishing.csv")
fishing = as.data.frame(fishing)

## convert categorical variables to factors

fishing$livebait = as.factor(fishing$livebait)
fishing$camper = as.factor(fishing$camper)

## understanding the relationship between no. of fishes caught and the other variables

library(lattice)
library(gridExtra)

p1 = xyplot(count ~ livebait, data = fishing,
             xlab = "Live Bait Usage", ylab = "Number of Fish Caught",
             main = "Fish Count by Live Bait", 
             type = c("p", "smooth"), col = "magenta")

p2 = xyplot(count ~ camper, data = fishing,
             xlab = "Camping Status", ylab = "Number of Fish Caught",
             main = "Fish Count by Camping Status", 
             type = c("p", "smooth"), col = "lightgreen")

p3 = bwplot(count ~ factor(child), data = fishing,
             xlab = "Number of Children in Group", ylab = "Number of Fish Caught",
             main = "Fish Count by Children in Group", col = "red")

p4 = xyplot(count ~ persons, data = fishing,
             xlab = "Number of People in Group", ylab = "Number of Fish Caught",
             main = "Fish Count by Group Size",
             type = c("p", "smooth"), col = "darkblue")

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

## checking for possible feature correlation

corrplot::corrplot(cor(fishing[ , -5]), method = 'ellipse', order = 'AOE')

## summary (descriptive) statistics

library(dplyr)

summary(fishing$count) # summary of the fish count

# summarizing count by livebait
fishing %>%
  group_by(livebait) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count)
  )

# summarizing count by camper
fishing %>%
  group_by(camper) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count)
  )

# summarizing count by child
fishing %>%
  group_by(child) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count)
  )

# summarizing count by persons
fishing %>%
  group_by(persons) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count)
  )


# Overdispersion Check for count response --------------------------------------

hist(fishing$count, col = "darkgreen", main = "Number of Fish Caught",
     ylab = "count", xlab = "visitors", cex = 0.6, pch = 16, breaks = 100)

library(AER) # package for checking overdispersion

# fitting a Poisson GLM to the fishing data
poisson_model = glm(count ~ livebait + camper + persons + child, 
                    family = poisson, data = fishing)

# performing dispersion test
dispersion_test = dispersiontest(poisson_model)
print(dispersion_test)

# removing child as a covariate due to its high correlation with persons
poisson_model_child_removed = glm(count ~ livebait + camper + persons, 
                    family = poisson, data = fishing)

print(dispersiontest(poisson_model_child_removed))


# Different Model Fits ----------------------------------------------------

## Poisson GLM

poisson_GLM = glm(count ~ livebait + camper + persons + child, 
                    family = poisson, data = fishing)
summary(poisson_GLM)

## Negative Binomial GLM

# Load necessary library
library(MASS)

nb_model = glm.nb(count ~ livebait + camper + persons + child,
                  data = fishing)
summary(nb_model)

## zero-inflated poisson and negative binomial GLM

# load necessary library
library(pscl)

zip = zeroinfl(count ~ .,
                       dist = "poisson", link = "logit", data = fishing)
summary(zip)
AIC(zip)

zinb = zeroinfl(count ~ .,
                  dist = "negbin", link = "logit", data = fishing)
summary(zinb)
AIC(zinb)

## plotting the fit results

plot(1:dim(fishing)[1], fishing$count, type = "l", col = "black", ylab = "count", xlab = "visitors",
     main = "Fitted Values from the Competing Models")
lines(1:dim(fishing)[1], predict(poisson_GLM), lty = 2, col = "red", type = "b", cex = 0.5, pch = 16)
lines(1:dim(fishing)[1], predict(nb_model), lty = 2, lwd = 2, col = "magenta")
lines(1:dim(fishing)[1], predict(zip), lty = 3, col = "blue")
lines(1:dim(fishing)[1], predict(zinb), lty = 4, col = "green")
legend("topright", legend = c("true count", "Poisson", "NB", "ZIP", "ZINB"), 
       col = c("black", "red", "magenta", "blue", "green"), lty = c(1, 2, 2, 3, 4), 
       pch = c(NA, 16, NA, NA, NA), lwd = c(1, 1, 2, 1, 1))

## checking the residuals for ZINB

# Function to create residual plots
plot_residuals = function(model, model_name) {
  par(mfrow = c(1, 2)) # Set layout for two plots
  
  # Residuals vs Fitted values
  plot(fitted(model), residuals(model), 
       main = paste(model_name, "- Residuals vs Fitted"), 
       xlab = "Fitted Values", ylab = "Residuals", 
       col = "blue", pch = 20)
  abline(h = 0, col = "red", lty = 2)
  
  # QQ Plot for normality of residuals
  qqnorm(residuals(model), main = paste(model_name, "- Normal Q-Q Plot"), 
         col = "blue", pch = 20)
  qqline(residuals(model), col = "red", lty = 2)
}

plot_residuals(zinb, "ZINB Model")
