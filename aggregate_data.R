#install.packages("dplyr")
library(dplyr)
## The goal of this script is to produce a function that creates a data frame containing the relevant data for predicting churn. It offers two parameters that can be set by the
## user. These are the current year-week combination called "t" and the boolean "train". If retrain is TRUE, the function includes all churners
## from the entire data set. The third parameter "pad" is a boolean that operates if "train = TRUE" and allows the user to pad the
## minority class with all churners in the past. Please set "t" in the format of YYYYWW. For instance week 26 in year 2016
## translates to 201626

# Read in data from csv file and set the appropriate name for the period-column.
#dframe <- read.csv("MockSet.csv", header = TRUE, sep = ";")
#dframe <- read.csv("20170918_Lars_v2.csv", header = TRUE, sep = ";")

aggregate_data <- function(dframe, t, train=F, pad = F){
  
# Create lists of variables that need to be aggregated or that need to be assessed at period t itself.
to_aggregate <- c("gesloten_rek","Random_id", "Delta_volume" ,"trans_tot", "trans_neg", "trans_pos", "Login_tot", "App_login", "Web_login", "app_page", "web_page", "BROADCASTS", "OPENS", "CLICKS", "UNSUBSCRIBES", "BOUNCES")
no_aggregate <- c("Churn","Random_id","open_rek" ,"yearweek", "Total_volume", "cl_startdatum", "cl_last_mut", "Rel_duur", "LTV", "segment")
  
# If user does not wish to recalibrate the models, only the current period and the 3 periods before that need to be considered. Predictions will be made based on this data.
if (train == F){

# Select all rows that fall in the right k week period. Note that this needs adjustment when selecting around the begin/end of a year.    
dframe <- dframe[which(dframe$yearweek == t-3 | dframe$yearweek == t-2 | dframe$yearweek == t-1 | dframe$yearweek ==t),]

# Remove the people from the set that have already churned in this period or have churned in the past.
if (nrow(dframe[-which(dframe$Random_id %in% dframe[which(dframe$open_rek ==dframe$gesloten_rek),]$Random_id),])>0){
dframe <- dframe[-which(dframe$Random_id %in% dframe[which(dframe$open_rek ==dframe$gesloten_rek),]$Random_id),]
}

# Construct data file that is to be analysed. A left join is made from a data frame that contains all variables at period t
# that do not need to be aggregated and a data frame that aggregates the data from periods t-3, ... ,t
to_predict_df <- left_join(dframe[which(dframe$yearweek ==t),which(colnames(dframe) %in% no_aggregate)],
                           as.data.frame(dframe[,which(colnames(dframe) %in% to_aggregate)] %>% group_by(Random_id) %>% summarise_all(funs(sum)))
                           ,by = "Random_id")
}
  
# For trainig of the model we construct a dataframe using the observations up to week 29 of 2017 so that we can identify all
# customers that have churnt. Optional is the "pad" parameter, which adds all previous churners to the current period around t.
if (train == T){
  
# In case we pad the minority class, one enters this part of the program.
if (pad == T){

# In order to train a model, we need to now if our dependent variable is 0 or 1 and hence we need to know if the customers
# measured at time t have churned in the period between time t+3, ..., t+6
if (t > max(dframe$yearweek)-6){
  stop("Please enter a yearweek period that is at least six weeks before the last observed period.")
}
  
# Find all churners in data set.   
dframe$Churn <- 0
dframe$Churn[which(dframe$open_rek == dframe$gesloten_rek & dframe$open_rek>0)] <- 1

# Set the dates of all churners so that they fall in the period of interest, being t+3, ... , t+6
dframe$yearweek[which(dframe$Churn==1)] <- t
dframe$yearweek[which(dframe$Churn==1)-1] <- t-1
dframe$yearweek[which(dframe$Churn==1)-2] <- t-2
dframe$yearweek[which(dframe$Churn==1)-3] <- t-3

all_churners_df <-left_join(dframe[which(dframe$yearweek ==t & dframe$gesloten_rek!=0),which(colnames(dframe) %in% no_aggregate)],
                          as.data.frame(dframe[which(dframe$gesloten_rek !=0 & (dframe$yearweek==t-3|dframe$yearweek==t-2|dframe$yearweek==t-1|dframe$yearweek==t)),
                          which(colnames(dframe) %in% to_aggregate)] %>% group_by(Random_id) %>% summarise_all(funs(sum))),by="Random_id")

Churners <- dframe$Random_id[which(dframe$Churn==1)]

}

# In case we do not pad the minority class, one enters this part of the program.
if (pad == F){

# In order to train a model, we need to now if our dependent variable is 0 or 1 and hence we need to know if the customers
# measured at time t have churned in the period between time t+3, ..., t+6
if (t > max(dframe$yearweek)-6){
      stop("Please enter a yearweek period that is at least six weeks before the last observed period.")
    }
dframe <- dframe[which(dframe$yearweek == t-3 | dframe$yearweek == t-2 | dframe$yearweek == t-1 | dframe$yearweek == t |
                         dframe$yearweek == t+3 | dframe$yearweek == t+4 | dframe$yearweek == t+5 | dframe$yearweek == t+6 ),]

# Initialize churn at 0. Set churn equal to 1 if the number of open accounts equals the number of closed accounts that week 
# given that the number of open accounts was greater than 0 to begin with.
churners <- dframe$Random_id[which(dframe$open_rek==dframe$gesloten_rek & dframe$open_rek >0 & (dframe$yearweek==t+3 |
                            dframe$yearweek==t+4 | dframe$yearweek==t+5 | dframe$yearweek==t+6))]
# Remove the customers who churned in the period before t.
if (nrow(dframe[which(dframe$open_rek==dframe$gesloten_rek & (dframe$yearweek == t-3 | dframe$yearweek == t-2 | dframe$yearweek == t-1 | dframe$yearweek == t)),]) >0){
  dframe <- dframe[-which(dframe$Random_id %in% dframe[which(dframe$open_rek ==dframe$gesloten_rek & (dframe$yearweek == t-3 | dframe$yearweek == t-2 | dframe$yearweek == t-1 | dframe$yearweek == t)),]$Random_id),]
}

# Construct data file that is to be analysed. A left join is made from a data frame that contains all variables at period t
# that do not need to be aggregated and a data frame that aggregates the data from periods t-3, ... ,t
to_predict_df <-left_join(dframe[which(dframe$yearweek ==t),which(colnames(dframe) %in% no_aggregate)],
                               as.data.frame(dframe[which(dframe$yearweek==t-3|dframe$yearweek==t-2|dframe$yearweek==t-1|dframe$yearweek==t),
                              which(colnames(dframe) %in% to_aggregate)] %>% group_by(Random_id) %>% summarise_all(funs(sum))),by="Random_id")
to_predict_df$Churn <- 0
to_predict_df$Churn[which(to_predict_df$Random_id %in% churners)] <-1
}

  

  
  
  
  
  
  
  
  
}  

return(to_predict_df)
}

