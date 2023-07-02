install.packages('caret', dependencies = TRUE)
install.packages('ggplot2')
library(caret)
install.packages('rlang')

remove.packages('rlang')


install.packages("readxl")
library(readxl)

data_q1 <- read_xlsx('Assignment3HW3_Data.xlsx', sheet = 'Data 1')
data_q1

library(dplyr)
data_q1_new <- data_q1 %>%
  mutate(dif = `Month After` - `Month Before`)


didnot_join_diff <- data_q1_new[data_q1_new$`Joined?` == 0,]$dif
join_diff <- data_q1_new[data_q1_new$`Joined?` == 1,]$dif


#test equal variance
var.test(didnot_join_diff, join_diff, 
         ratio = 1, alternative = "two.sided")


### Variances are equal, so run Equal Variance t-test ###
t.test(didnot_join_diff, 
       join_diff, 
       alternative = "two.sided", 
       mu = 0, 
       paired = FALSE, 
       var.equal = TRUE)

mean_diff <- mean(join_diff) - mean(didnot_join_diff)
mean_diff





##Importing the data

mydata <-read_xlsx('Assignment3HW3_Data.xlsx', sheet = "Data 2")
View(mydata)


library(dplyr)
ret <- mydata %>% select(`Customer ID`, `Joined?`, `Customer Age with Firm at time of launching the online community`, `Churned at 3 months after launch of the online community`, `Average Spend Last 3 months of Life with the firm`) 


View(ret)
summary(ret)

#correlation matrix
cor_ret <- ret %>% select( `Churned at 3 months after launch of the online community`, `Joined?`,`Customer Age with Firm at time of launching the online community`, `Average Spend Last 3 months of Life with the firm`)
cor <- cor(cor_ret, method = "pearson", use = "complete.obs")
View(cor)


str(ret)

head(ret)

##Running t-test and test of equal variances
#test equal variance
var.test(didnot_join_diff, join_diff, 
         ratio = 1, alternative = "two.sided")


### Variances are equal, so run Equal Variance t-test ###
t.test(didnot_join_diff, 
       join_diff, 
       alternative = "two.sided", 
       mu = 0, 
       paired = FALSE, 
       var.equal = TRUE)

mean_diff <- mean(join_diff) - mean(didnot_join_diff)
mean_diff




##Running a logistic regression model
retmodel <- glm( `Churned at 3 months after launch of the online community` ~  `Joined?` + `Customer Age with Firm at time of launching the online community` + `Average Spend Last 3 months of Life with the firm` , data = ret ,family=binomial(link="logit"))
summary(retmodel)
AIC(retmodel)


#prediction of churn prob
retmodel_churn_prob <- predict.glm(retmodel, newdata=ret, type="response")
View(retmodel_churn_prob)

#classifying the 1's and 0's with benchmark of 0.5
pred_retmodel_churn <- ifelse(retmodel_churn_prob > 0.5, 1,0) #at what level should we say prob(default)=1
pred_retmodel_churn_comment <- ifelse(retmodel_churn_prob > 0.5, "Churned","Not Churned")

#final table
retention_prob <- 1- retmodel_churn_prob
final_table <- cbind(ret,retmodel_churn_prob,pred_retmodel_churn,pred_retmodel_churn_comment,retention_prob)
final_df <- data.frame(final_table)
View(final_df)


##CLV
##GM= 88*3
## CLV Calculation: Method  (Aggregate CLV)
#Formula used: CLV = (Gross Margin*Avg Spend*(1+Discount Rate))/(1+Discount Rate - Retention Rate)
totalspend<-final_df$Average.Spend.Last.3.months.of.Life.with.the.firm*3
margin<-totalspend*0.5
clv<-margin*(1/(1-retention_prob))
final_df<-cbind(final_df,totalspend,margin,clv)
View(final_df)

A1<-subset(final_df$Average.Spend.Last.3.months.of.Life.with.the.firm,final_df$Joined.==1)
B1<-mean(A1)

#mean(df[final_df$Joined. == '1', final_df$clv])
A2<-subset(final_df$Average.Spend.Last.3.months.of.Life.with.the.firm,final_df$Joined.==0)
B2<-mean(A2)

#mean(df[final_df$Joined. == '1', final_df$clv])
Aclv<-subset(final_df$clv ,final_df$Joined.==1)
Bclv<-mean(Aclv)

#mean(df[final_df$Joined. == '1', final_df$clv])
Aclv_not<-subset(final_df$clv ,final_df$Joined.==0)
Bclv_not<-mean(Aclv_not)

totalspendjoin<-final_df$Average.Spend.Last.3.months.of.Life.with.the.firm*3
marginjoin<-totalspend*0.5
clvjoin<-margin*(1/(1-retention_prob))
final_df<-cbind(final_df,totalspend,margin,clv)
View(final_df)








clvjoin<-ifelse(final_df$Joined.==1,mean(final_df$clv),0)
clvnotjoin<-


clv5_joined <- (gross_margin*avg_revenue_joined*(1+discount_rate))/(1+discount_rate-retention_rate_joined)
clv5_joined
clv5_not_joined <- (gross_margin*avg_revenue_not_joined*(1+discount_rate))/(1+discount_rate-retention_rate_not_joined)
clv5_not_joined



