rm(list = ls(all = TRUE))

setwd("C:/Users/amrit/OneDrive/Documents/SantaClara/R- OMIS2392")

Store_data = read.csv("store attributes.csv")

#Making store data a data frame
Sdf <- data.frame(Store_data)

Trans_Cust_dt = read.csv("Transactions and customer attributes.txt",sep="\t")

summary(Store_data)


#Dummy Variable  if the transaction was at a sister brand or not.  The sister brands did 
#not have a policy change and will serve as a reference group so we can isolate the effect 
#of changing the return policy.0= sister brand and 1= all other stores.
Trans_Cust_dt$X2 = ifelse(Trans_Cust_dt$Brand_number == "10"|Trans_Cust_dt$Brand_number == "8",1,0)

view(summary(Sdf))

# Data Subset based on online and physical store
Sdf$online <- ifelse(Sdf$store_number == "2" | Sdf$store_number == "6"| Sdf$store_number == "10", 1, 0)

OnlineStoreData <- subset(Sdf, Sdf$online == "1")

PhysicalStoreData <- subset(Sdf, Sdf$online == "0")

#Aggregating Store data by store number and month
PhysicalStoreData$monthly_Store_Sales <- aggregate(PhysicalStoreData$store_number, by = list(PhysicalStoreData$month, PhysicalStoreData$store_average_price), FUN = mean, na.rm = TRUE)

#Aggregating Transaction data by store number and month
Trans_Cust_dt$online <- ifelse(Trans_Cust_dt$store_number == "2" | Trans_Cust_dt$store_number == "6"| Trans_Cust_dt$store_number == "10", 1, 0)

OnlineTransactionData <- subset(Trans_Cust_dt, Trans_Cust_dt$online == "1")

PhysicalTransactionData <- subset(Trans_Cust_dt, Trans_Cust_dt$online == "0")

#Aggregating Store data by store number and month
PhysicalTransactionData$monthly_Store_Sales <- aggregate(PhysicalTransactionData$store_number, by = list(PhysicalTransactionData$month, PhysicalTransactionData$net_purchase_amount), FUN = mean, na.rm = TRUE)

#Merging the physical store and transactions data from the two datasets
Ps_Rt_dt <- merge(PhysicalStoreData, PhysicalTransactionData, by = c("store_number","month"))

#Finding Sales returns $ amount
Ps_Rt_dt$SalesReturn <- Ps_Rt_dt$net_purchase_amount*Ps_Rt_dt$return


#Dummy Variable to indicate if a purchase was before or after the policy change on Oct. 1. 
#0 = pre-change and 1= post-change.
Ps_Rt_dt$X1=ifelse(as.character.Date(Ps_Rt_dt$purchase_date) >=as.character.Date("01OCT2013:00:00:00"),1,0)


#Dummy Variable  if the transaction was at a sister brand or not.  The sister brands did 
#not have a policy change and will serve as a reference group so we can isolate the effect 
#of changing the return policy.0= sister brand and 1= all other stores.
Ps_Rt_dt$X2 = ifelse(Ps_Rt_dt$Brand_number.x == "10"|Ps_Rt_dt$Brand_number.x == "8",1,0)

print(summary(Ps_Rt_dt))
hist(Ps_Rt_dt$net_purchase_amount)

hist(log(1+Ps_Rt_dt$net_purchase_amount))

Ps_Rt_dt$month_num <- ifelse(Ps_Rt_dt$month=='JAN',1,ifelse(Ps_Rt_dt$month=='FEB',2,ifelse(Ps_Rt_dt$month=='MAR',3,ifelse(Ps_Rt_dt$month=='APR',4,ifelse(Ps_Rt_dt$month=='MAY',5,ifelse(Ps_Rt_dt$month=='JUN',6,ifelse(Ps_Rt_dt$month=='JUL',7,ifelse(Ps_Rt_dt$month=='AUG',8,ifelse(Ps_Rt_dt$month=='SEP',9,ifelse(Ps_Rt_dt$month=='OCT',10,ifelse(Ps_Rt_dt$month=='NOV',11,12))))))))))) # This command generates a new variable named "month" that indexes transaction month. We have a string variable named "month". Depending on the string value, we assign a number for the new variable month.

#Regresion Modelling
