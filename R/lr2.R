#' Load dataset from files of different formats
#' @description Read data from different file formats into a dataframe. Supported formats are csv, txt, fwf, tsv, psv, xls, xlsx, json, sav, sas7bdat, rds, dta.
#' @param loc file location
#'
#' @return dataframe containing data read from the file
#' @export
#' 
#' @examples
read_data <- function(loc){
  
  # get file extension
  ext = tools::file_ext(loc)
  
  # choose what function to use based on file extension
  if(ext == 'csv'| ext == 'txt' | ext == 'fwf' | ext == 'tsv' | ext == 'psv'){
    
    data = data.table::fread(loc)
    
  } else if(ext == 'xls' | ext == 'xlsx'){
    
    # read first row from excel file..
    # if first row is character then the file has headers (header = TRUE)
    # else header = FALSE 
    row1 = readxl::read_excel(loc,col_names = FALSE,n_max=1)
    header = is.character(row1[[1]])
    data = readxl::read_excel(loc,col_names = header)
    
  } else if(ext == 'json'){
    
    data = jsonlite::fromJSON(loc)
    data = jsonlite::flatten(data)
    
  } else if(ext == 'sav'){
    
    data = foreign::read.spss(loc)
    
  } else if(ext == 'sas7bdat'){
    
    data = haven::read_sas(loc)
    
  } else if(ext == 'rds'){
    
    data = base::readRDS(loc)
    
  } else if(ext == 'dta'){
    
    data = haven::read_dta(loc)
    
  } else {
    stop('Invalid File Format')
  }
  
  # convert data into dataframe, fix numeric types, and return it
  data = as.data.frame(data)
  data = fix_numeric(data)
  return(data)
  
}


#' Make sure all columns containing numbers are numeric
#' @description Fix numeric columns marked as non-numeric while reading data
#' @param data dataframe containing data read from the file
#'
#' @return dataframe after marking numeric columns correctly
#' @export
#' 
#' @examples
fix_numeric <- function(data){
  
  for(col in names(data)) {
    
    # if col is numeric, skip it
    if(is.numeric(data[[col]])){
      next
    }
    
    
    # get index of first non-NA element
    idx = min(which(!is.na(data[[col]])))
    
    
    # if the non-NA element can't be transformed into numeric, continue
    # if it can be transformed, transform whole column into numeric
    if(is.na(as.numeric(data[[col]][idx]))){
      next
    }
    else{
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  return(data)
  
}


#' Calculate total sum for x
#'
#' @param df this funtion takes a dataframe with 'x' column
#'
#' @return total_sum_of_independent_vector
#' @export
#'
#' @examples
Total_sum_x <- function(df){
  total_sum= 0
  for (i in df[["x"]]) {
    total_sum <- total_sum + i
  }
  return(total_sum)
}

#' Calculate total sum for y
#'
#' @param df this funtion takes a dataframe with 'y' column
#'
#' @return total_sum_of_dependent_vector
#' @export
#'
#' @examples
Total_sum_y <- function(df){
  total_sum= 0
  for (i in df[["y"]]) {
    total_sum <- total_sum + i
  }
  return(total_sum)
}

#' Get Dataframe Length
#'
#' @param df this function takes a dataframe with 'y' column
#'
#' @return length_of_dependent_vector
#' @export
#'
#' @examples
length_df <- function(df){
  length= 0
  for (i in df[["y"]]) {
    length <- length + 1
  }
  return(length)
}

#' Calculate Mean of X
#'
#' @param df this funtion takes a dataframe with 'x' column
#'
#' @return mean_of_independent_vector
#' @export
#'
#' @examples
mean_for_x <- function(df){
  n=length_df(df)
  sum_x= Total_sum_x(df)
  mean_x= sum_x/n
  return(mean_x)
}


#' Calculate Mean of Y
#'
#' @param df this funtion takes a dataframe with 'y' column
#'
#' @return mean_of_dependent_vector
#' @export
#'
#' @examples
mean_for_y <- function(df){
  n=length_df(df)
  sum_y= Total_sum_y(df)
  mean_y= sum_y/n
  return(mean_y)
}


#' Calculate Sxx
#'
#' @param df this funtion takes a dataframe with 'x' column
#'
#' @return sum_of_squared_deviations_of_the_independent_variable_from_its_mean
#' @export
#'
#' @examples
calculateSXX <- function(df){
  x= df[["x"]]
  x_mean= mean_for_x(df)
  squared_deviations= (x-x_mean)^2
  squared_deviations
  sxx= sum(squared_deviations)
  return(sxx)
}


#' Calculate Syy
#'
#' @param df this funtion takes a dataframe with 'y' column
#'
#' @return sum_of_squared_deviations_of_the_dependent_variable_from_its_mean
#' @export
#'
#' @examples
calculateSYY <- function(df){
  y= df[["y"]]
  y_mean= mean_for_y(df)
  squared_deviations= (y-y_mean)^2
  squared_deviations
  syy= sum(squared_deviations)
  return(syy)
}


#' Calculate Sxy
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#'
#' @return the_sum_of_the_product_of_the_differences_between_
#' x_values_and_its_mean_and_the_differences_between_
#' y_values_and_its_mean
#' @export
#'
#' @examples
calculateSXY <- function(df){
  x= df[["x"]]
  x_mean= mean_for_x(df)
  y= df[["y"]]
  y_mean= mean_for_y(df)
  deviations= (x-x_mean)*(y-y_mean)
  sxy= sum(deviations)
  sxy
  return(sxy)
}

#' Calculate B1
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param sxx,sxy (optional) values of Sxx and Sxy if known. Will be calculated from the data if unknown.
#'
#' @return the slope of the regression line
#' @export
#'
#' @examples
calculateB1 <- function(df, sxx=NULL, sxy=NULL){
  if(is.null(sxx)){
    sxx = calculateSXX(df)
  }
  if(is.null(sxy)){
    sxy = calculateSXY(df)
  }
  B1= sxy/sxx
  return(B1)
}

#' Calculate B0
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param B1 (optional) value of B1 if known.Will be calculated from the data if unknown.
#' @param sxx,sxy (optional) values of Sxx and Sxy if known. Will be calculated from the data if unknown.
#' sxx and sxy values are not needed if B1 is known.
#'
#' @return the intercept of the regression line
#' @export
#'
#' @examples
calculateB0 <- function(df, B1=NULL, sxx=NULL,sxy=NULL){
  x= df[["x"]]
  x_mean= mean_for_x(df)
  y= df[["y"]]
  y_mean= mean_for_y(df)
  
  if(is.null(B1)){
    B1= calculateB1(df,sxx,sxy)
  }
  
  B0= y_mean - (B1*x_mean)
  return(B0)
}

#' Calculate SRR
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param SXX,SXY (optional) values of Sxx and Sxy if known. Will be calculated from the data if unknown.
#' sxy value is not needed if B1 is known.
#' 
#' @return the percentage of data that is fitted by the regression model
#' the_squared_difference_between_the_predicted_values_and_the_mean_of_
#' the_actual_values
#' @export
#'
#' @examples
calculateSSR <- function(df,B1=NULL,SXX=NULL,SXY=NULL){
  if(is.null(SXX)){
    SXX = calculateSXX(df)
  }
  
  if(is.null(B1)){
    B1=calculateB1(df,SXX,SXY)
  }
  SSR=(B1^2)*SXX
  
  return(SSR)
}

#' Calculate SSE
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param SST,SSR (optional) values of SST and SSR if known. Will be calculated from the data if unknown.
#'
#' @return the_squared_difference_between_the_actual_values_and_
#' the_predicted_values
#' @export
#'
#' @examples
calculateSSE <- function(df,SST=NULL,SSR=NULL){
  if(is.null(SST)) {
    SST = calculateSYY(df)
  }
  if(is.null(SSR)) {
    SSR=calculateSSR(df)
  }
  
  SSE=SST-SSR
  
  return(SSE)
}

#' Calculate SST
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#'
#' @return the total variability of the dependent variable around its mean
#' @export
#'
#' @examples
calculateSST <- function(df){
  SST = calculateSYY(df)
  
  return(SST)
}


#' Calculate MSE
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param SSE (optional) values of SSE if known. Will be calculated from the data if unknown.
#'
#' @return estimation for sigma square
#' @export
#'
#' @examples
calculateMSE<-function(df,SSE=NULL){
  if(is.null(SSE)){
    SSE=calculateSSE(df)
  }
  DF_SST=length_df(df)-1
  DF_SSR=1
  DF_SSE=DF_SST-DF_SSR
  MSE=SSE/DF_SSE
  
  return(MSE)
}


#' Calculate MSR
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param SSR(optional) values of SSR if known. Will be calculated from the data if unknown.
#'
#' @return the mean square regression
#' @export
#'
#' @examples
calculateMSR<-function(df,SSR=NULL){
  if(is.null(SSR)){
    SSR=calculateSSR(df)
  }
  DF_SSR=1
  MSR=SSR/DF_SSR
  
  return(MSR)
}

#' Calculate R Squared
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param SST,SSR (optional) values of SST and SSR if known. Will be calculated from the data if unknown.
#' 
#' The following values are not needed if SSR is known.
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param SXX,SXY (optional) values of Sxx and Sxy if known. Will be calculated from the data if unknown.

#'
#' @return the coefficient of determination that is always +ve in range [0,1]
#' @export
#'
#' @examples
calculateRSQUARED<-function(df,SST=NULL,SSR=NULL,B1=NULL,SXX=NULL,SXY=NULL){
  if(is.null(SST)) {
    SST = calculateSYY(df)
  }
  if(is.null(SSR)) {
    SSR=calculateSSR(df,B1,SXX,SXY)
  }

  RSQUARED=SSR/SST
  return(RSQUARED)
}


#' Calculate r
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param RSQUARED (optional) value of RSQUARED if known. Will be calculated from the data if unknown.
#'
#' @return the_correlation_coefficient_that_determines_
#' the_strength_of_the_relationship_between_2_variables
#' @export
#'
#' @examples
calculateCORRELATIONCOEFF<-function(df,B1=NULL,RSQUARED=NULL){
  if(is.null(B1)){
    B1=calculateB1(df)
  }
  if(is.null(RSQUARED)){
    RSQUARED=calculateRSQUARED(df)
  }
  
  CORRELATIONCOEFF=RSQUARED^0.5
  if(B1>=0){
    return(CORRELATIONCOEFF)
  } else if(B1<0){
    return(-CORRELATIONCOEFF)
  }
}

#' Calculate Anova F-Value
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param MSR (optional) value of MSR if known. Will be calculated from the data if unknown.
#' @param MSE (optional) value of MSE if known. Will be calculated from the data if unknown.
#'
#' @return the division between MSR & MSE
#' @export
#'
#' @examples
calculateFVALUE<-function(df,MSR=NULL,MSE=NULL){
  if(is.null(MSR)){
    MSR=calculateMSR(df)
  }
  if(is.null(MSE)){
    MSE=calculateMSE(df)
  }
  FVALUE=MSR/MSE
  return(FVALUE)
}



#' Generate Anova Table
#'
#' @param df this funtion takes a dataframe with 'x' and 'y' columns
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param SXX (optional) value of Sxx if known. Will be calculated from the data if unknown.
#' @param SXY (optional) value of Sxy if known. Will be calculated from the data if unknown.
#' @param SYY (optional) value of Syy if known. Will be calculated from the data if unknown.
#'
#' @return the_analysis_of_variance_table_that_summarize_
#' the_results_of_an_ANOVA_test_displays_the_sources_of_variations_&_D.O.F_&_
#' sum_of_squares_&_mean_squares_&_F-statistics
#' @export
#'
#' @examples
calculateANOVATABLE<-function(df,B1=NULL,SXX=NULL,SXY=NULL,SYY=NULL){
  SST = SYY
  if(is.null(SST)){
    SST = calculateSYY(df)
  }
  SSR=calculateSSR(df,B1,SXX,SXY)
  SSE=calculateSSE(df,SST,SSR)
  DF_SST=length_df(df)-1
  DF_SSR=1
  DF_SSE=DF_SST-DF_SSR
  MSE=calculateMSE(df,SSE)
  MSR=calculateMSR(df,SSR)
  FVALUE=calculateFVALUE(df,MSR,MSE)
  
  tab <- matrix(c(SSR, DF_SSR, MSR, FVALUE, SSE, DF_SSE, MSE,' ', SST,DF_SST,' ',' '), ncol=4, byrow=TRUE)
  colnames(tab) <- c('SS','D.O.F','MS','F')
  rownames(tab) <- c('regression','residual','total')
  tab <- as.table(tab)
  
  
  return(tab)
}

#' calculateCI_B0
#'
#'@description This function calculate the confidence interval of the estimator B0 
#'in a linear regression model
#' @param data a dataset of x and y
#' @param known  a boolean parameter that takes (T/F) as value
#' @param sigma the variance of the dataset and it has  default value (NULL)
#' @param alpha the value of significance level
#' @param B0 (optional) value of B0 if known. Will be calculated from the data if unknown.
#' @param MSE (optional) value of MSE if known. Will be calculated from the data if unknown.
#' @param SXX (optional) value of Sxx if known. Will be calculated from the data if unknown.
#'
#' @return the confidence interval of B0
#' @export
#'
#' @examples 
calculateCI_B0=function(data,known=FALSE,sigma= NULL,alpha, B0=NULL,MSE=NULL,SXX=NULL){
  n=length_df(data)
  if(is.null(B0)){
    B0=calculateB0(data)
  }
  if(is.null(MSE)){
    MSE=calculateMSE(data)
  }
  if(is.null(SXX)){
    SXX=calculateSXX(data)
  }
  X_bar=mean_for_x(data)
  
  if(known){
    z_score <- qnorm(1 - alpha/2)
    Upper=B0+(z_score*(sigma*(1/n+X_bar^2/SXX))^.5)
    Lower=B0-(z_score*(sigma*(1/n+X_bar^2/SXX))^.5)
    return(paste("The", (1-alpha)*100, "% confidence interval for the intercept (B0) is [", Lower, ", ",Upper, "]."))
  }
  
  else{
    df=n-2
    t_score <- qt(1 - alpha/2, df)
    
    Upper=B0+(t_score*(MSE*(1/n+X_bar^2/SXX))^.5)
    Lower=B0-(t_score*(MSE*(1/n+X_bar^2/SXX))^.5)
    return(paste("The", (1-alpha)*100, "% confidence interval for the intercept (B0) is [", Lower, ", ",Upper, "]."))
  }
}  


#' calculateCI_B1
#'
#'@description This function calculate the confidence interval of the estimator B1 
#'in a linear regression model
#' @param data a dataset of x and y
#' @param known  a boolean parameter that takes (T/F) as value
#' @param sigma the variance of the dataset and it has  default value (NULL)
#' @param alpha the value of significance level
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param MSE (optional) value of MSE if known. Will be calculated from the data if unknown.
#' @param SXX (optional) value of Sxx if known. Will be calculated from the data if unknown.
#'
#' @return the confidence interval of B1
#' @export
#'
#' @examples 
calculateCI_B1=function(data,known=FALSE,sigma ,alpha, B1=NULL,MSE=NULL,SXX=NULL){
  n=length_df(data)
  if(is.null(B1)){
    B1=calculateB1(data)
  }
  if(is.null(MSE)){
    MSE=calculateMSE(data)
  }
  if(is.null(SXX)){
    SXX=calculateSXX(data)
  }
  X_bar=mean_for_x(data)
  
  
  if (known){
    z_score <- qnorm(1 - alpha/2)
    Upper=B1+(z_score*(sigma/SXX)^.5)
    Lower=B1-(z_score*(sigma/SXX)^.5)
    return(paste("The", (1-alpha)*100, "% confidence interval for the slope (B1) is [", Lower, ", ",Upper, "]."))
  }
  
  else{
    df=n-2
    t_score <- qt(1 - alpha/2, df)
    Upper=B1+(t_score*(MSE/SXX)^.5)
    Lower=B1-(t_score*(MSE/SXX)^.5)
    return(paste("The", (1-alpha)*100, "% confidence interval for the slope (B1) is [", Lower, ", ",Upper, "]."))
  }
  
}


#' caculateCI_for sigma square
#'
#'@description This function calculate the confidence interval of the estimator B1 
#'in a linear regression model
#' @param df a dataset of x and y
#' @param alpha the value of significance level
#' @param MSE (optional) value of MSE if known. Will be calculated from the data if unknown.
#'
#' @return the confidence interval of sigma squared
#' @export
#'
#' @examples 
calculateCI_SigmaSquare <- function(df, alpha, MSE = NULL) {
  n <- length_df(df)
  dof <- n - 2
  if(is.null(MSE)){
    MSE=calculateMSE(df)
  }
  lower <- dof * MSE / qchisq(1 - alpha/2, dof)
  upper <- dof * MSE / qchisq(alpha/2, dof)
  return(paste("The", (1-alpha)*100, "% confidence interval for sigma squared is [", lower, ", ",upper, "]."))
}


#' Calculating y_predicted
#'
#' @param df a dataset of x and y
#' @param xo  value of x0 to calculate the mean response at
#' @param Bo (optional) value of Bo if known. Will be calculated from the data if unknown.
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#'
#' @return y0
#' @export
#'
#' @examples 
calculateyo <- function(df, xo,Bo=NULL,B1=NULL){
  if(is.null(Bo)){
    Bo = calculateB0(df)
  }
  if(is.null(B1)){
    B1 = calculateB1(df)
  }
  
  yo = Bo + (B1*xo)
  return(yo)
}


#' Calculating CI for mean response
#'
#' @param df a dataset of x and y
#' @param a  significance level (alpha)
#' @param xo  value of xo to calculate the mean response at
#' @param known boolean to indicate if sigma squared is known or not
#' @param sigma2  value of sigma squared, if known
#' @param Bo (optional) value of Bo if known. Will be calculated from the data if unknown.
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param MSE (optional) value of MSE if known. Will be calculated from the data if unknown.
#' @param Sxx (optional) value of Sxx if known. Will be calculated from the data if unknown.
#'
#' @return confidence interval of mean response
#' @export
#'
#' @examples
calculateCI_mean_response <- function(df, a, xo, known=FALSE , sigma2 = NULL,Bo=NULL,B1=NULL,MSE=NULL,Sxx=NULL){
  
  yo = calculateyo(df,xo,Bo,B1)
  n = length_df(df)
  alpha = a
  dof = n - 2
  z_score = qnorm(1 - alpha/2)
  t_score = qt(1- alpha/2, dof)
  x_bar = mean_for_x(df)
  if(is.null(MSE)){
    MSE=calculateMSE(df)
  }
  if(is.null(Sxx)){
    Sxx = calculateSXX(df)
  }
  
  if(known){
    U = yo + (z_score*(sigma2*((1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
    L = yo - (z_score*(sigma2*((1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
  } else {
    U = yo + (t_score*(MSE*((1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
    L = yo - (t_score*(MSE*((1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
  }
  
  return(paste("The", (1-alpha)*100, "% confidence interval for the Mean response is [", L, ", ",U, "]."))
}

#' Calculating CI for new observation
#'
#' @param df a dataset of x and y
#' @param a  significance level (alpha)
#' @param xo  value of xo to calculate the mean response at
#' @param known boolean to indicate if sigma squared is known or not
#' @param sigma2  value of sigma squared, if known
#' @param Bo (optional) value of Bo if known. Will be calculated from the data if unknown.
#' @param B1 (optional) value of B1 if known. Will be calculated from the data if unknown.
#' @param MSE (optional) value of MSE if known. Will be calculated from the data if unknown.
#' @param Sxx (optional) value of Sxx if known. Will be calculated from the data if unknown.
#'
#' @return confidence interval of new observation
#' @export
#'
#' @examples
calculateCI_new_observation <- function(df, a, xo, known = FALSE, sigma2 = NULL,Bo=NULL,B1=NULL,MSE=NULL,Sxx=NULL){
  
  yo = calculateyo(df,xo,Bo,B1)
  n = length_df(df)
  alpha = a
  dof = n - 2
  z_score = qnorm(1 - alpha/2)
  t_score = qt(1 - alpha/2, dof)
  x_bar = mean_for_x(df)
  if(is.null(MSE)){
    MSE=calculateMSE(df)
  }
  if(is.null(Sxx)){
    Sxx = calculateSXX(df)
  }
  if(known){
    U = yo + (z_score*(sigma2*(1 + (1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
    L = yo - (z_score*(sigma2*(1 + (1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
  } else {
    U = yo + (t_score*(MSE*(1 + (1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
    L = yo - (t_score*(MSE*(1 + (1/n) + ((xo - x_bar)^2)/Sxx))^0.5)
  }
  
  return(paste("The", (1-alpha)*100, "% confidence interval for the New observation is [", L, ", ",U, "]."))
}

