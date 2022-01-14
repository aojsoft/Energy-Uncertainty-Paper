library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting

# Modeling packages
library(earth)     # for fitting MARS models
library(caret)     # for automating the tuning process

# Model interpretability packages
library(vip)       # for variable importance
library(pdp)       # for variable relationships
###

head(mydata)

set.seed(18)
mydata = read.csv('uncert2.csv', header = T)
mydata = mydata[, -1]

head(mydata);tail(mydata)
#mydata_div = sort(sample(nrow(mydata), nrow(mydata)*0.7))
mydata_div = sample(1:nrow(mydata), nrow(mydata)*0.7)
mydata_trn = mydata[mydata_div,]
mydata_tst = mydata[-mydata_div,]


####

# Fit a basic MARS model
mars1 <- earth(
  KEROSENE ~ CIU2+MIU+EPU+GFI+VIX,  
  data = mydata_trn   
)

# Print model summary
print(mars1)

summary(mars1) %>% .$coefficients %>% head(10)
summary(mars1)


plot(mars1, which = 1)

# Fit a basic MARS model
mars2 <- earth(
 WTI ~ CIU1+MIU+EPU+GFI+VIX,  
  data = mydata_trn,
  degree = 3
)


# check out the first 10 coefficient terms
summary(mars2) %>% .$coefficients %>% head(10)
plot(mars2, which = 1)

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)



# Cross-validated model
set.seed(123)  # for reproducibility
cv_mars <- train(
  x = subset(mydata_trn, select = -BRENT),
  y = mydata_trn$BRENT,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# View results
cv_mars$bestTune


cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)

ggplot(cv_mars)


cv_mars$resample

# variable importance plots
p1 <- vip(cv_mars, num_features = 40, geom = "point", value = "gcv") + ggtitle("GCV")
p2 <- vip(cv_mars, num_features = 40, geom = "point", value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)

# extract coefficients, convert to tidy data frame, and
# filter for interaction terms
cv_mars$finalModel %>%
  coef() %>%  
  broom::tidy() %>%  
  filter(stringr::str_detect(names, "\\*")) 


# Construct partial dependence plots
p1 <- partial(cv_mars, pred.var = "EPU", grid.resolution = 10) %>% 
  autoplot()
p2 <- partial(cv_mars, pred.var = "CIU2", grid.resolution = 10) %>% 
  autoplot()
p3 <- partial(cv_mars, pred.var = c("EPU", "CIU2"), 
              grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, 
              screen = list(z = -20, x = -60))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)