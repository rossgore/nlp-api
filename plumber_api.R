source("descriptive-stats-api.r")



# plumber.
#* Echo back the input
#* @param msg The message to echo
#* @get /echo
echo <- function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot v histogram
#* @png
#* @get /plot
rand <- function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param v The first number to add
#* @param b The second number to add
#* @post /sum
sum <- function(v, b){
  as.numeric(v) + as.numeric(b)
}





#* Return the mean of v vector of numbers
#* @param v The vector of numbers
#* @post /mymean
mymean = function(v){
  to.return = getMean(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}



#* Return the mode of v dataset
#* @param v The dataset
#* @post /getmode
mode = function(v){
  to.return = getmode(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the mode of v dataset
#* @param v Enter the dataset
#* @post /getSD
SD = function(v){
  to.return = getSD(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the mode of v dataset
#* @param v Enter the dataset
#* @post /getMin
Min = function(v){
  to.return = getMin(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the mode of v dataset
#* @param v Enter the dataset
#* @post /getMax
Max = function(v){
  to.return = getMax(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the mode of v dataset
#* @param v Enter the dataset
#* @post /getMedian
Median = function(v){
  to.return = getMedian(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the mode of v dataset
#* @param v Enter the dataset
#* @post /getRange
range = function(v){
  to.return = getRange(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the mode of v dataset
#* @param v Enter the dataset
#* @post /getQuantile
quantile = function(v){
  to.return = getQuantile(v, io.json=TRUE)
  to.return = to.return %>% fromJSON()
}

#######################################################LINEAR REGRESSION###################################################

#* Return the Coeffecient of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getGLMCoeff
Coeff = function(ycolJ, xcolsJ){
  to.return = getCoeff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the Residual of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getGLMRes
Res = function(ycolJ, xcolsJ){
  to.return = getRes(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the Effects of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getGLMEff
Eff = function(ycolJ, xcolsJ){
  to.return = getEff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the Fit of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getGLMFit
Fit = function(ycolJ, xcolsJ){
  to.return = getFit(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the Rank of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getGLMRank
Rank = function(ycolJ, xcolsJ){
  to.return = getRank(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the Model of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getGLMModel
Model = function(ycolJ, xcolsJ){
  to.return = getModel(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}




###########################################################GLM############################################################

#* Return the Coeffecient of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getCoeff
Coeff = function(ycolJ, xcolsJ){
  to.return = getCoeff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the Residual of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getRes
Res = function(ycolJ, xcolsJ){
  to.return = getRes(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the Effects of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getEff
Eff = function(ycolJ, xcolsJ){
  to.return = getEff(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}


#* Return the Fit of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getFit
Fit = function(ycolJ, xcolsJ){
  to.return = getFit(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the Rank of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getRank
Rank = function(ycolJ, xcolsJ){
  to.return = getRank(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}

#* Return the Model of v dataset
#* @param v Enter the dataset y cols and x cols 
#* @post /getModel
Model = function(ycolJ, xcolsJ){
  to.return = getModel(datasetJ, ycolJ, xcolsJ, io.json = TRUE)
  to.return = to.return %>% fromJSON()
}



