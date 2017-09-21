## Script for calculating descriptive statistics.
library(dplyr)
load("scriptie_data.Rdata")

## Remove all rows where there are no open accounts as these will bias the descriptive statistics to 0
dframe <- dframe[-which(dframe$open_rek==0),]

## Construct a dummy variable for churn
dframe$Churn <- 0
dframe$Churn[which(dframe$open_rek==dframe$gesloten_rek)]<-1

## Compute population descriptives.
nr_clients <- length(unique(dframe$Random_id))
nr_churners <- sum(dframe$Churn)
  
pop_means <- data.frame(t(dframe %>% summarise(mean(open_rek), mean(Total_volume), mean(Rel_duur), mean(trans_tot), mean(App_login),
                                  mean(Web_login), mean(app_page), mean(web_page), mean(OPENS), mean(CLICKS))))
pop_mins <- data.frame(t(dframe %>% summarise(min(open_rek), min(Total_volume), min(Rel_duur), min(trans_tot), min(App_login),
                                    min(Web_login), min(app_page), min(web_page), min(OPENS), min(CLICKS))))
pop_max <- data.frame(t(dframe %>% summarise(max(open_rek), max(Total_volume), max(Rel_duur), max(trans_tot), max(App_login),
                                   max(Web_login), max(app_page), max(web_page), max(OPENS), max(CLICKS))))
pop_std_dev <- data.frame(t(dframe %>% summarise(sd(open_rek), sd(Total_volume), sd(Rel_duur), sd(trans_tot), sd(App_login),
                                                 sd(Web_login), sd(app_page), sd(web_page), sd(OPENS), sd(CLICKS))))

pop_descriptives <- cbind(pop_means,pop_mins,pop_max,pop_std_dev)
colnames(pop_descriptives)<-c("Mean", "Min", "Max", "Std_Dev")
rownames(pop_descriptives) <- c("Open_rek", "Total_volume", "Rel_duur", "Trans_tot", "App_login", "Web_login", "App_page",
                                "Web_page","Opens", "clicks")
pop_descriptives <- format(pop_descriptives, decimal.mark = ",")

## Write to csv for reporting purposes
write.csv(pop_descriptives,"C:/Users/l.vankempen/Desktop/Thesis Paper/Programming/Descriptive Statistics/population_descriptives.csv")
rm(pop_means,pop_mins,pop_max, pop_std_dev, pop_descriptives)


## Calculate weekly descriptives.
weekly_churners <- dframe %>% group_by(yearweek) %>% summarise(weekly_churn = sum(Churn))
avg_weekly_churners <- mean(weekly_churners$weekly_churn)
min_weekly_churners <- min(weekly_churners$weekly_churn)
max_weekly_churners <- max(weekly_churners$weekly_churn)

weekly_means <- dframe %>% group_by(yearweek) %>% summarise(mean(open_rek), mean(Total_volume), mean(Rel_duur), mean(trans_tot), mean(App_login),
                                               mean(Web_login), mean(app_page), mean(web_page), mean(OPENS), mean(CLICKS))
weekly_means <- format(weekly_means, decimal.mark = ",")
weekly_mins <- dframe %>% group_by(yearweek) %>% summarise(min(open_rek), min(Total_volume), min(Rel_duur), min(trans_tot), min(App_login),
                                                            min(Web_login), min(app_page), min(web_page), min(OPENS), min(CLICKS))
weekly_mins <- format(weekly_mins, decimal.mark = ",")
weekly_max <- dframe %>% group_by(yearweek) %>% summarise(max(open_rek), max(Total_volume), max(Rel_duur), max(trans_tot), max(App_login),
                                                           max(Web_login), max(app_page), max(web_page), max(OPENS), max(CLICKS))
weekly_max <- format(weekly_max, decimal.mark = ",")
weekly_std_devs <- dframe %>% group_by(yearweek) %>% summarise(sd(open_rek), sd(Total_volume), sd(Rel_duur), sd(trans_tot), sd(App_login),
                                                           sd(Web_login), sd(app_page), sd(web_page), sd(OPENS), sd(CLICKS))
weekly_std_devs <- format(weekly_std_devs, decimal.mark = ",")

write.csv(weekly_means, "C:/Users/l.vankempen/Desktop/Thesis Paper/Programming/Descriptive Statistics/weekly_means.csv")
write.csv(weekly_mins, "C:/Users/l.vankempen/Desktop/Thesis Paper/Programming/Descriptive Statistics/weekly_mins.csv")
write.csv(weekly_max,"C:/Users/l.vankempen/Desktop/Thesis Paper/Programming/Descriptive Statistics/weekly_max.csv")
write.csv(weekly_std_devs,"C:/Users/l.vankempen/Desktop/Thesis Paper/Programming/Descriptive Statistics/weekly_std_devs.csv")
rm(weekly_means, weekly_mins, weekly_max, weekly_std_devs)
