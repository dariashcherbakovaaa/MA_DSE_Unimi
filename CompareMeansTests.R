library(devtools)

################################# data ####################################################################

d_1group_norm_25 <- data.frame(rnorm(sample(1:10, 25, replace=T), mean = 0, sd = 1))
names(d_1group_norm_25) <- c("One")
#d_2group_norm_25 <- data.frame(rnorm(sample(1:10, 25, replace=T), mean = 0, sd = 1),
#                               rnorm(sample(11:20, 25, replace=T), mean = 0, sd = 1))
#names(d_2group_norm_25) <- c("One", "Two")
#d_4group_norm_25 <- data.frame(rnorm(sample(1:10, 25, replace=T), mean = 0, sd = 1),
#                               rnorm(sample(11:20, 25, replace=T), mean = 0, sd = 1),
#                               rnorm(sample(21:30, 25, replace=T), mean = 0, sd = 1),
#                               rnorm(sample(31:40, 25, replace=T), mean = 0, sd = 1))
#names(d_4group_norm_25) <- c("One", "Two", "Three", "Four")
#
## normal distribution, big sample
#d_1group_norm_125 <- data.frame(rnorm(sample(1, 125, replace=T), mean = 0, sd = 1))
#names(d_1group_norm_125) <- c("One")
#d_2group_norm_125 <- data.frame(rnorm(sample(1:10, 125, replace=T), mean = 0, sd = 1),
#                                rnorm(sample(11:20, 125, replace=T), mean = 0, sd = 1))
#names(d_2group_norm_125) <- c("One", "Two")
#d_4group_norm_125 <- data.frame(rnorm(sample(1:10, 125, replace=T), mean = 0, sd = 1),
#                                rnorm(sample(11:20, 125, replace=T), mean = 0, sd = 1),
#                                rnorm(sample(21:30, 125, replace=T), mean = 0, sd = 1),
#                                rnorm(sample(31:40, 125, replace=T), mean = 0, sd = 1))
#names(d_4group_norm_125) <- c("One", "Two", "Three", "Four")
## not normal distribution, small sample
#d_1group_nenorm_25 <- data.frame(sample(1:10, 25, replace=T))
#names(d_1group_nenorm_25) <- c("One")
#d_2group_nenorm_25 <- data.frame(sample(1:10, 25, replace=T),
#                                 sample(11:20, 25, replace=T))
#names(d_2group_nenorm_25) <- c("One", "Two")
#d_4group_nenorm_25 <- data.frame(sample(1:10, 25, replace=T),
#                                 sample(11:20, 25, replace=T),
#                                 sample(21:30, 25, replace=T),
#                                 sample(31:40, 25, replace=T))
#names(d_4group_nenorm_25) <- c("One", "Two", "Three", "Four")
## not normal distribution, big sample
#d_1group_nenorm_125 <- data.frame(sample(1, 125, replace=T))
#names(d_1group_nenorm_125) <- c("One")
#d_2group_nenorm_125 <- data.frame(sample(1:10, 125, replace=T),
#                                  sample(11:20, 125, replace=T))
#names(d_2group_nenorm_125) <- c("One", "Two")
#d_4group_nenorm_125 <- data.frame(sample(1:10, 125, replace=T),
#                                  sample(11:20, 125, replace=T),
#                                  sample(21:30, 125, replace=T),
#                                  sample(31:40, 125, replace=T))
#names(d_4group_nenorm_125) <- c("One", "Two", "Tree", "Four")

################################# data ####################################################################

d_1group_norm_125 <- data.frame(rnorm(sample(1, 125, replace=T), mean = 0, sd = 1))
names(d_1group_norm_125) <- c("One")
x <- d_1group_norm_25 #assign data to x
################################# Check conditions to choose the test ####################################


#' Creating dataframe with information on data
#'
#' @return dataframe with information about data
#' @export
#'
#' @examples
create_dataframe <- function(){
  DV_Cat<-as.integer(readline(prompt="Enter the number of dependent categorical variables: "))
  IV_Cat<-as.integer(readline(prompt="Enter the number of independent categorical variables: "))
  DV_Con <- as.integer(readline(prompt="Enter the number of dependent continuous variables:"))
  IV_Con <- as.integer(readline(prompt="Enter the number of independet continuous variables:"))
  return(data.frame(DV_Categorical = c(DV_Cat), IV_Categorical = c(IV_Cat),
                    DV_Continuous=c(DV_Con), IV_Continuous=c(IV_Con)))
}
info_data <- create_dataframe()


#' Sample dependence function
#'
#' @return add the column to the dataset with information on Dependent variable and Independent variables
#' @export
#'
#' @examples
sample_dependence <- function(){
  answer <- readline(prompt = "Your DV and IV are in the same (sub)sample? (Yes/No): ")
  return(ifelse(answer == "Yes", 1, 0))
}
info_data$sample_dep <- sample_dependence()


#' Sample size function
#'
#' @param x
#'
#' @return add the column to the dataset with information on sample size
#' @export
#'
#' @examples
sample_size <- function(x){
  if (ncol(x) > 1){
    size <- length(x[ , (ncol(x))])
    return (size)
  } else {
    nrow(x)
  }
}

info_data$sample_size <- sample_size(x)


#' Calculate the distribution
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sig_p <- function(x){
  options(scipen=999)
  if (length(info_data$sample_size) > 50)
    print (shapiro.test(sapply(x, as.numeric)))
  else
    print (ks.test(sapply(x, as.numeric), "pnorm"))
}

sig_p(x)


#' Distribution check function
#'
#' @return The information on the distribution
#' @export
#'
#' @examples
p_val <- function(){
  a <- 0.05
  p <- readline(prompt = "please copy and past p-value: ")
  print(ifelse(ifelse(p > a, 1, 0) == 1, "Your data has the normal distribution",
               "Your data doesn't have the normal distribution"))
  return(ifelse(p > 0.05, 1, 0))
}
info_data$Normal_distribution <- p_val()

################################# Choose the test (function) ####################################

#' First step of compare means function, assuming the normal distribution
#'
#' @param x
#'
#' @return results of the compare means tests
#' @export
#'
test_performance <- function(x){
  if(info_data$Normal_distribution==0){
    if(info_data$sample_size < 30){
      print("Please consider non-parametric tests")}
    else
      if (ncol(x) == 1){chisq.test(x[[1]])}
    else if(ncol(x) == 2){chisq.test(x[[1]], x[[2]])}
  } else if (ncol(x) > 2) {
    chisq.test(x[[1]], x[[2]], x[[3]], x[[4]])
  }
  else if(info_data$Normal_distribution==1){
    if(info_data$sample_size > 30){
      if(ncol(x) == 1){
        t.test(x, mu = mean(x[[1]]), alternative = "two.sided")}
      else if(ncol(x) == 2){
        if(info_data$DV_Categorical == 1){
          if(info_data$IV_Categorical == 1){
            print("Use linear or multi-linear regression")}
          else{
            print("Use logistic regression")}}
        else if (info_data$IV_Categorical == 1){
          two.way <- aov(x[[1]] ~ x[[2]])
          summary(two.way)}
        else if(info_data$sample_dep == 1){
          t.test(x[[1]], x[[2]], paired = TRUE, alternative = "two.sided")
        } else if (ncol(x) > 2){t.test(x[[1]], x[[2]], paired = F, alternative = "two.sided")}
        } else if (ncol(x) > 2){
          two.way <- aov(x[[1]] ~ x[[2]] + x[[3]] + x[[4]])
          summary(two.way)}
          else {print("We cannot conduct the analysis")}
    }
    else print("We cannot conduct the analysis")}
}

test_performance(x)

#' Second step of compare means function, assuming the non-normal distribution
#'
#' @return results of the compare means tests
#' @export
#'
#' @examples
#non_par_tests <- function(){
#  if(info_data$Normal_distribution == 0){
#    if(info_data$sample_size < 30){
#      if(ncol(x) == 2){
#        if(info_data$sample_dep == 1){
#          pairwise.wilcox.test(x[[1]], x[[2]],p.adjust.method = "BH")
#        } else {wilcox.test(x[[1]]~droplevels(as.factor(x[[2]])))}
#    } else if(ncol(x) > 2){
#        kruskal.test(x[[1]] ~ droplevels(as.factor(x[[2]])))
#      }
#    }
#  }else{
#    print("Use tests for normal distribution")}
#}


