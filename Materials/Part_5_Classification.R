###################################################################
## Code for Applied Machine Learning by Max Kuhn @ DC
## https://github.com/topepo/DC-ML-2018

###################################################################
## Slide 4

library(tidymodels)

###################################################################
## Slide 5

two_class_example %>% head(4)

###################################################################
## Slide 6

two_class_example %>% 
	conf_mat(truth = truth, estimate = predicted)

two_class_example %>% 
	accuracy(truth = truth, estimate = predicted)

###################################################################
## Slide 7

two_class_example %>% 
	conf_mat(truth = truth, estimate = predicted)

###################################################################
## Slide 10

library(pROC)
roc_obj <- roc(
	response = two_class_example$truth,
	predictor = two_class_example$Class1,
	# If the first level is the event of interest:
	levels = rev(levels(two_class_example$truth))
	)

auc(roc_obj)

plot(
	roc_obj,
	legacy.axes = TRUE,
	print.thres = c(.2, .5, .8), 
	print.thres.pattern = "cut = %.2f (Spec = %.2f, Sens = %.2f)",
	print.thres.cex = .8
)

###################################################################
## Slide 12

load("Data/okc.RData")
okc_train %>% dim()
okc_test %>% nrow()
table(okc_train$Class)

###################################################################
## Slide 17

library(caret)
ctrl <- trainControl(
	method = "cv",
	# Also predict the probabilities
	classProbs = TRUE,
	# Compute the ROC AUC as well as the sens and  
	# spec from the default 50% cutoff. The 
	# function `twoClassSummary` will produce those. 
	summaryFunction = twoClassSummary,
	savePredictions = "final",
	sampling = "down"
)

###################################################################
## Slide 23

set.seed(5515)
cart_mod <- train(
	x = okc_train[, names(okc_train) != "Class"], 
	y = okc_train$Class,
	method = "rpart2",
	metric = "ROC",
	tuneGrid = data.frame(maxdepth = 1:20),
	trControl = ctrl
)

###################################################################
## Slide 24

cart_mod

###################################################################
## Slide 25

ggplot(cart_mod)

###################################################################
## Slide 26

cart_mod$finalModel

###################################################################
## Slide 27

plot_roc <- function(x, ...) {
	averaged <- x %>%
		group_by(rowIndex, obs) %>%
		summarise(stem = mean(stem, na.rm = TRUE))
	roc_obj <- roc(
		averaged[["obs"]], 
		averaged[["stem"]], 
		levels = rev(levels(averaged$obs))
	)
	plot(roc_obj, ...)
}
plot_roc(cart_mod$pred)

###################################################################
## Slide 28

confusionMatrix(cart_mod)

###################################################################
## Slide 29

cart_imp <- varImp(cart_mod, scale = FALSE, 
                   surrogates = FALSE, 
                   competes = FALSE)
ggplot(cart_imp, top = 7) + xlab("")

###################################################################
## Slide 37

set.seed(5515)
cart_bag <- train(
	x = okc_train[, names(okc_train) != "Class"], 
	y = okc_train$Class,
	method = "treebag",
	metric = "ROC",
	trControl = ctrl
)

###################################################################
## Slide 38

cart_bag

###################################################################
## Slide 39

confusionMatrix(cart_bag)

###################################################################
## Slide 40

plot_roc(cart_mod$pred)
plot_roc(cart_bag$pred, col = "darkred", add = TRUE)

###################################################################
## Slide 41

bag_imp <- varImp(cart_bag, scale = FALSE)
ggplot(bag_imp, top = 30) + xlab("")


###################################################################
## Slide 55

is_two_levels <- function(x) length(unique(x)) == 2 & is.numeric(x)
probably_dummy <- vapply(okc_train, is_two_levels, logical(1))
dummies <- names(probably_dummy)[probably_dummy]

no_dummies <- recipe(Class ~ ., data = okc_train) %>%
	step_bin2factor(!!! dummies) %>%
	step_zv(all_predictors())

smoothing_grid <- expand.grid(usekernel = TRUE, fL = 0, adjust = seq(0.5, 3.5, by = 0.5))

###################################################################
## Slide 56

set.seed(5515)
nb_mod <- train(
	no_dummies,
	data = okc_train,
	method = "nb",
	metric = "ROC",
	tuneGrid = smoothing_grid,
	trControl = ctrl
)
###################################################################
## Slide 57

ggplot(nb_mod)

###################################################################
## Slide 58

plot_roc(cart_mod$pred)
plot_roc(cart_bag$pred, col = "red", add = TRUE)
plot_roc(nb_mod$pred, col = "blue", add = TRUE)

###################################################################
## Slide 60

rs <- resamples(
  list(CART = cart_mod, Bagged = cart_bag, Bayes = nb_mod)
)
library(tidyposterior)
roc_mod <- perf_mod(rs, seed = 2560, iter = 5000)

###################################################################
## Slide 61

roc_dist <- tidy(roc_mod)
summary(roc_dist)
differences <-
	contrast_models(
		roc_mod,
		list_1 = c("Bagged", "Bayes"),
		list_2 = c("CART", "Bagged"),
		seed = 650
	)

###################################################################
## Slide 62

summary(differences, size = 0.025)

###################################################################
## Slide 63

differences %>%
	mutate(contrast = paste(model_2, "vs", model_1)) %>%
	ggplot(aes(x = difference, col = contrast)) + 
	geom_line(stat = "density") + 
	geom_vline(xintercept = c(-0.025, 0.025), lty = 2)

###################################################################
## Slide 64

test_res <- okc_test %>%
	dplyr::select(Class) %>%
	mutate(
		prob = predict(nb_mod, okc_test, type = "prob")[, "stem"],
		pred = predict(nb_mod, okc_test)
	)
roc_curve <- roc(test_res$Class, test_res$prob, levels = c("other", "stem"))


###################################################################
## Slide 65

roc_curve
getTrainPerf(nb_mod)

plot(
	roc_curve, 
	print.thres = .5,
	print.thres.pattern = "cut = %.2f (Sp = %.3f, Sn = %.3f)",
	legacy.axes = TRUE
)

###################################################################
## Slide 66

ggplot(test_res, aes(x = prob)) + geom_histogram(binwidth = .04) + facet_wrap(~Class)
