## ----setup, include=FALSE------------------------------------------------
library(blkbox)

## ------------------------------------------------------------------------
# Example Data
my_data <- iris[1:100, 1:4]
head(my_data, 5)
# Example Labels
my_labels <- as.character(iris[1:100, 5])
unique(my_labels)


## ----eval = FALSE, message = FALSE---------------------------------------
#  # Partitioning Data
#  my_partition = Partition(data = my_data,
#                           labels = my_labels)
#  
#  # Creating a Training & Testing Model
#  model_1 <- blkbox(data = my_partition)
#  

## ----message = FALSE, eval = FALSE---------------------------------------
#  # Creating a Cross-fold Validation Model
#  model_2 <- blkboxCV(data = my_data,
#                      labels = my_labels)

## ----message = FALSE, eval = FALSE---------------------------------------
#  # Creating a Nested Cross-fold Validation Model
#  model_3 <- blkboxNCV(data = my_data,
#                       labels = my_labels,
#                       Method = "randomforest",
#                       AUC = 0.9)

## ---- eval = FALSE-------------------------------------------------------
#  # Calculate Performance
#  perf = Performance(model_1)
#  # Standard ROC curve
#  blkboxROC(perf_cv)

## ----eval=FALSE----------------------------------------------------------
#  # Standard ROC curve for Cross-fold Validation with 2 repeats
#  model_2r <- blkboxCV(data = my_data,
#                      labels = my_labels,
#                      repeats = 2)
#  perf_2r = Performance(model_2r)
#  blkboxROC(perf_2r)
#  
#  # Alternvatively to avoid Faceting
#  # perf_2r = Performance(model_2r, consensus = F)
#  # blkboxROC(perf_2r)
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Standard ROC curve for Cross-fold Validation
#  
#      # Need to adjust blkboxROC to accept NCV input (data is already in output)
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder cv.plot
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder ncv.plot
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # Example placeholder
#  

