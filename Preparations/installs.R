from_cran <- 
	c("AmesHousing", "caret", "doParallel", "e1071", "earth", 
		"glmnet", "ipred", "klaR", "pROC", "rpart", "sessioninfo", 
		"tidymodels")

install.packages(from_cran, repos = "http://cran.rstudio.com")

if(!interactive())
  q("no")
