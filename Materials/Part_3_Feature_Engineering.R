###################################################################
## Code for Applied Machine Learning by Max Kuhn @ DC
## https://github.com/topepo/DC-ML-2018

library(AmesHousing)
library(tidymodels)
ames <- make_ames()

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

###################################################################
## slide 10

ggplot(ames_train, aes(x = Neighborhood)) + geom_bar() + coord_flip() + xlab("")

###################################################################
## slide 13

mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

###################################################################
## slide 15

mod_rec <- recipe(Sale_Price ~ Longitude + Latitude + Neighborhood, data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

###################################################################
## slide 16

mod_rec_trained <- prep(mod_rec, training = ames_train, retain = TRUE, verbose = TRUE)

###################################################################
## slide 17

mod_rec_trained

###################################################################
## slide 18

ames_test_dummies <- bake(mod_rec_trained,newdata = ames_test)
names(ames_test_dummies)

###################################################################
## slide 21

price_breaks <- (1:6)*(10^5)

ggplot(ames_train, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
    scale_x_log10() + 
  scale_y_continuous(breaks = price_breaks, trans = "log10" ) +
  geom_smooth(method = "loess")

###################################################################
## slide 22

library(MASS) # to get robust linear regression model

ggplot(ames_train, 
       aes(x = Year_Built, 
           y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  scale_y_continuous(breaks = price_breaks, 
                     trans = "log10") + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm")

###################################################################
## slide 23

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built: Central_Air, data = ames_train)
anova(mod1, mod2)

###################################################################
## slide 24

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train, retain = TRUE) %>%
  juice %>%
  # select a few rows with different values
  slice(153:157)

###################################################################
## slide 26

library(AmesHousing)
ames <- make_ames()

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(data_split)

set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price")

###################################################################
## slide 27

lin_coords <- recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
                      Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
                      Central_Air + Longitude + Latitude,
                    data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) 

coords <- lin_coords %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

###################################################################
## slide 28

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5)
  ) + 
  scale_y_log10()

###################################################################
## slide 29

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5)
  ) + 
  scale_y_log10()

###################################################################
## slide 30

cv_splits <- cv_splits %>% 
  mutate(coords = map(splits, prepper, recipe = coords, retain = TRUE))

###################################################################
## slide 31

lm_fit_rec <- function(rec_obj, ...) 
  lm(..., data = juice(rec_obj))

cv_splits <- cv_splits %>% 
  mutate(fits = map(coords, lm_fit_rec, Sale_Price ~ .))
glance(cv_splits$fits[[1]])

###################################################################
## slide 32

assess_predictions <- function(split_obj, rec_obj, mod_obj) {
  raw_data <- assessment(split_obj)
  proc_x <- bake(rec_obj, newdata = raw_data, all_predictors())
  # Now save _all_ of the columns and add predictions. 
  bake(rec_obj, newdata = raw_data, everything()) %>%
    mutate(
      .fitted = predict(mod_obj, newdata = proc_x),
      .resid = Sale_Price - .fitted,  # Sale_Price is already logged by the recipe
      # Save the original row number of the data
      .row = as.integer(split_obj, data = "assessment")
    )
}

###################################################################
## slide 33

cv_splits <- cv_splits %>%
  mutate(
    pred = 
      pmap(
        lst(split_obj = cv_splits$splits, rec_obj = cv_splits$coords, mod_obj = cv_splits$fits),
        assess_predictions 
      )
  )

###################################################################
## slide 34

# Compute the summary statistics
map_df(cv_splits$pred, metrics, truth = Sale_Price, estimate = .fitted) %>% 
  colMeans

###################################################################
## slide 35

assess_pred <- bind_rows(cv_splits$pred) %>%
  mutate(Sale_Price = 10^Sale_Price,
         .fitted = 10^.fitted) 

ggplot(assess_pred,
       aes(x = Sale_Price, y = .fitted)) + 
  geom_abline(lty = 2) + 
  geom_point(alpha = .4)  + 
  geom_smooth(se = FALSE, col = "red")

