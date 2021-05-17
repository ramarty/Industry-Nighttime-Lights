# Within Correlation: Regression Classification

# http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/
# https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/

library(tidyverse)
library(caret)
library(rpart)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

set.seed(42)

# Load/Prep Within Unit Correlation --------------------------------------------
cor_within <- readRDS(file.path(data_file_path, "Results", "correlation_within_unit.Rds"))
cor_within <- cor_within %>%
  dplyr::filter(!is.na(cor),
                #transform == "log",
                unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid"),
                ntl_var %in% c("dmspolsharmon_mean", "viirs_mean")) %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolsharmon_mean" ~ "DMSP-OLS",
                                    ntl_var %in% "viirs_mean"        ~ "VIIRS"),
                unit = unit %>% 
                  str_replace_all(" Grid", "") %>%
                  factor(levels = rev(c("5km", "10km", "25km", "50km", "100km"))),
                ntl_var_change = ntl_var_max - ntl_var_min,
                firm_var_change = firm_var_max - firm_var_min,
                cor_gp5 = cor > 0.5,
                cor_gp25 = cor > 0.25)

#cor_within$cor_gp5 <- cor_within$cor_gp5
cor_within$cor_gp5 <- cor_within$cor_gp5 %>% factor(levels = c("TRUE", "FALSE"))

# Regression Tree --------------------------------------------------------------
df <- cor_within %>%
  dplyr::filter(country %in% "Mexico",
                unit %in% "5km",
                ntl_var %in% "VIIRS",
                firm_var %in% "N_firms_sum_all")

# ntl_var_min + ntl_var_max + ntl_var_change + firm_var_change
model1 <- rpart(cor_gp5 ~ firm_var_min + firm_var_max + firm_var_change, 
                data = df)
model1
fancyRpartPlot(model1, caption = NULL)

#### 1. Prep Train/Test
# TODO: Split geographically
train_tf <- sample(x = c(T, F), 
                   prob = c(0.8, 0.2),
                   size = nrow(df),
                   replace = T) 

train.df <- df[train_tf,]
test.df <- df[!train_tf,]

#### 2. Train Model - Simple
model1 <- rpart(cor_gp5 ~ ntl_var_min + ntl_var_max + ntl_var_change + firm_var_change, 
                data = train.df, 
                method = "class",
                cp = 0.005)


par(xpd = NA) 
plot(model1)
text(model1, digits = 3)

#### 2. Train Model - CV for complexity parameter (cp)
model <- train(
  cor_gp5 ~ ntl_var_min + ntl_var_max + ntl_var_change + firm_var_change, 
  data = train.df, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model error vs different values of
# cp (complexity parameter)
plot(model)
# Print the best tuning parameter cp that
# minimize the model RMSE
model$bestTune


# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model$finalModel)
text(model$finalModel, digits = 3)











train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

model <- rpart(cor_gp5 ~ ntl_var_min + ntl_var_max + ntl_var_change + firm_var_change, data = df)





summary(model)

par(xpd = NA)
plot(model)
text(model, digits = 3)

library(party)
set.seed(123)
model <- train(
  cor_gp5 ~ ntl_var_min + ntl_var_max + ntl_var_change + firm_var_change, data = df, method = "ctree2",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.95 )
)
plot(model$finalModel)


