## Script for calculating descriptive statistics.
library(ggplot2)
library(zoo)
library(dplyr)
load("scriptie_data.Rdata")

## Remove all rows where there are no open accounts as these will bias the descriptive statistics to 0
dframe <- dframe[-which(dframe$open_rek==0),]
#dframe <- dframe[which(!is.na(dframe$Total_volume)),]
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

## Write to csv for reporting purposes
write.table(pop_descriptives,"C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/population_descriptives.csv", dec = ',')


## Calculate weekly descriptives.
weekly_churners <- dframe %>% group_by(yearweek) %>% summarise(weekly_churn = sum(Churn))
avg_weekly_churners <- mean(weekly_churners$weekly_churn)
min_weekly_churners <- min(weekly_churners$weekly_churn)
max_weekly_churners <- max(weekly_churners$weekly_churn)
write.csv(weekly_churners, "C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/weekly_churners.csv")
weekly_means <- dframe %>% group_by(yearweek) %>% summarise(mean(open_rek), mean(Total_volume), mean(Rel_duur), mean(trans_tot), mean(App_login),
                                               mean(Web_login), mean(app_page), mean(web_page), mean(OPENS), mean(CLICKS))
weekly_mins <- dframe %>% group_by(yearweek) %>% summarise(min(open_rek), min(Total_volume), min(Rel_duur), min(trans_tot), min(App_login),
                                                            min(Web_login), min(app_page), min(web_page), min(OPENS), min(CLICKS))
weekly_max <- dframe %>% group_by(yearweek) %>% summarise(max(open_rek), max(Total_volume), max(Rel_duur), max(trans_tot), max(App_login),
                                                           max(Web_login), max(app_page), max(web_page), max(OPENS), max(CLICKS))
weekly_sums <- dframe %>% group_by(yearweek) %>%summarise(sum(open_rek),sum(Total_volume),sum(trans_tot), sum(App_login),sum(Web_login), sum(app_page), sum(web_page), sum(OPENS), sum(CLICKS),
                                                          sum(BROADCASTS))
weekly_std_devs <- dframe %>% group_by(yearweek) %>% summarise(sd(open_rek), sd(Total_volume), sd(Rel_duur), sd(trans_tot), sd(App_login),
                                                           sd(Web_login), sd(app_page), sd(web_page), sd(OPENS), sd(CLICKS))

avg_sums <- data.frame(colMeans(weekly_sums))
write.table(weekly_means, "C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/weekly_means.csv", dec = ",", row.names = F)
write.table(weekly_mins, "C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/weekly_mins.csv",dec = ",", row.names = F)
write.table(weekly_max,"C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/weekly_max.csv", dec = ",", row.names = F)
write.table(weekly_sums,"C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/weekly_sums.csv", dec = ",", row.names = F)
write.table(weekly_std_devs,"C:/Users/l.vankempen/Documents/Thesis Paper/Programming/Descriptive Statistics/weekly_std_devs.csv", dec= ",", row.names = F)

## Construct some descriptive plots for in thesis paper

# Plot of churners per week
churnPlot <-ggplot(weekly_churners, aes(x=as.character(weekly_churners$yearweek), y=weekly_churners$weekly_churn)) +
geom_line(aes(group=1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Yearweek") +
ylab("Number of churners")+ theme(axis.text.y=element_text(size=10)) + ggtitle("Total number of churners per week") + theme(plot.title = element_text(hjust=0.5, size =22)) +
scale_x_discrete(breaks=weekly_churners$yearweek[seq(1,nrow(weekly_churners),by=2)]) + coord_fixed(ratio = 0.10) +
theme(axis.title=element_text(size =17))
ggsave(filename="churnPerWeek.eps",plot = churnPlot, width = 8, height = 5)
  


