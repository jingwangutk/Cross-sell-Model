rm(list=ls())
############ Data URLs ############
allsellR <- function(
  start_ind,
  end_ind,
  start_dep,
  end_dep,
  evaluate=TRUE){
  
  f <- "%d/%m/%Y"
  t1 <- as.Date(start_ind, f)
  t2 <- as.Date(end_ind, f)
  t3 <- as.Date(start_dep, f)
  t4 <- as.Date(end_dep, f)
  length_ind <- t2 - t1  
  
  if (!require("FNN")) {
    install.packages('randomForest',
                     repos="https://cran.rstudio.com/", 
                     quiet=TRUE) 
    require('FNN')
  }
  
  if (!require("AUC")) {
    install.packages('AUC',
                     repos="https://cran.rstudio.com/", 
                     quiet=TRUE) 
    require('AUC')
  }
  
  if (!require("lift")) {
    install.packages('lift',
                     repos="https://cran.rstudio.com/", 
                     quiet=TRUE) 
    require('lift')
  }
  
  readAndPrepareData <- function(train=TRUE,...){
    ## Input data
    cat("Reading in data:")
    time <- Sys.time()
r_url <- "http://kddata.co/data/chapter6/routes.csv"
p_url <-  "http://kddata.co/data/chapter6/products.csv"
c_url <- "http://kddata.co/data/chapter6/customers.csv"
vd_url <- "http://kddata.co/data/chapter6/visitdetails.csv"
v_url <- "http://kddata.co/data/chapter6/visits.csv"

############ Read in the data ############
# Reading in data with proper column classes
routes <- read.csv(r_url, header = T, colClasses = c("character", rep("factor", 3)))
products <- read.csv(p_url, header = T, colClasses = c("character", rep("factor", 2), "numeric"))
customers <- read.csv(c_url, header = T, colClasses = c(rep("character", 2), "factor", "integer", "factor"))
visitdetails <- read.csv(vd_url, header = T, colClasses = c(rep("character", 2), "integer", "character"))
visits <- read.csv(v_url, header = T, colClasses = c(rep("character", 3), "character", "numeric", rep("factor",2)))
#removing zeros
visits$VisitDate <- sub(".000000000","",visits$VisitDate)
#convert to date and time
visits$VisitDate <- as.POSIXlt(strptime(visits[,"VisitDate"], "%d-%b-%y %I.%M.%S %p"))

############# Build Base Table ###############
# Calculate number of VisitdetailIDs for each VisitID
vd_data <- as.data.frame(table(visitdetails$VisitID))
colnames(vd_data) <- c("VisitID", "N_Products")
vd_data$VisitID <- as.character(vd_data$VisitID)

# Merge this data
visitdetails <- merge(visitdetails, vd_data, by = "VisitID", all.x = T)

# Calculate the number of items that were purchased in each visit
vq_data <- aggregate(visitdetails$Quantity, by = list(visitdetails$VisitID), sum)
colnames(vq_data) <- c("VisitID", "N_Items")
vq_data$VisitID <- as.character(vq_data$VisitID)

# Merge this data
visitdetails <- merge(visitdetails, vq_data, by = "VisitID", all.x = T)

# Merge visits and visitdetails keeping ALL
visits_basetable <- merge(visits, visitdetails, by = "VisitID", all = T)

# Null the unneccessary columns
visits_basetable$Quantity <- visits_basetable$VisitDetailID <- NULL

# Re-order the table columns
visits_basetable <- visits_basetable[,c("VisitID", "CustomerID", "ProductID", "SalesRepresentativeID",
                                        "VisitDate", "Amount", "PaymentTerm", "Outcome", "N_Products",
                                        "N_Items")]

if (train==FALSE){
  dots <- list(...) # list(...) evaluates all arguments and
  # returns them in a named list
  t2 <- dots$end_ind
  t1 <- t2 - dots$length_ind
  rm(dots)
}
# Get the customers who actually made a purchase in the dependent period
customer_base_ind <- visits_basetable$CustomerID[which(visits_basetable$VisitDate < t2 & is.na(visits_basetable$ProductID)==F)]
if(train==TRUE){
  customer_base_dep <- visits_basetable$CustomerID[which(visits_basetable$VisitDate > t3 & is.na(visits_basetable$ProductID)==F)]
  visits_basetableall <- visits_basetable[which(visits_basetable$CustomerID %in% customer_base_dep & visits_basetable$CustomerID %in% customer_base_ind),]
  visits_basetableall <- merge(visits_basetableall, products[,c("ProductID", "Category", "Family")], by = "ProductID", all.x = T)
}
if(train==FALSE){
  visits_basetableall <- customer_base_ind
  visits_basetableall <- merge(visits_basetableall, products[,c("ProductID", "Category", "Family")], by = "ProductID", all.x = T)
}


#1st product in dependent period (dependent variable)
#most recent product independent
#2most recent product independent
#1st product bought
#2nd most recent product family
#1st product family
#most recent sales rep

customer_desc <- visits_basetableall[which(visits_basetableall$Outcome=="SALES"),]
customer_desc <- customer_desc[order(customer_desc$CustomerID,customer_desc$VisitDate),]
customer_descind <- customer_desc[which(customer_desc$VisitDate<=t2),]

if(train==TRUE){
customer_descdep <- customer_desc[which(customer_desc$VisitDate>=t3),]


customer_desc2 <- as.data.frame(matrix(ncol=9))
colnames(customer_desc2) <- c("CustomerID", "FirstProdDep", "MostRecentProdInd", "SecMostRecentProdInd", "FirstProdBought", "MostRecentProdFam", "SecRecentProdFam", "FirstProdFam", "MostRecentRep")

customer_descid <- unique(customer_desc$CustomerID)

#top 50 products
prodids50 <- head(sort(table(customer_desc$ProductID),decreasing = T),50)

#Creating table with variables for each customer regarding first, most recent, second most recent
for(i in 1:length(unique(customer_descid))){
  customer_desc2[i,1] <- customer_descid[i]
  customer_desc2[i,2] <- customer_descdep[head(which(customer_descind$CustomerID == customer_descid[i]),1),"ProductID"]
  customer_desc2[i,3] <- customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),1),"ProductID"]
  customer_desc2[i,4] <- customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),2)[1],"ProductID"]
  customer_desc2[i,5] <- customer_descind[head(which(customer_descind$CustomerID == customer_descid[i]),1),"ProductID"]
  customer_desc2[i,6] <- as.character(customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),1),"Family"])
  customer_desc2[i,7] <- as.character(customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),2)[1],"Family"])
  customer_desc2[i,8] <- as.character(customer_descind[head(which(customer_descind$CustomerID == customer_descid[i]),1),"Family"])
  customer_desc2[i,9] <- customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),1),"SalesRepresentativeID"]
}

customer_desc2$MostRecentProdFam <- as.factor(customer_desc2$MostRecentProdFam)
customer_desc2$SecRecentProdFam <- as.factor(customer_desc2$SecRecentProdFam)
customer_desc2$FirstProdFam <- as.factor(customer_desc2$FirstProdFam)

visits_basetableall <- merge(visits_basetableall, customer_desc2, by = "CustomerID", all.x = T)
}

if(train==FALSE){
  customer_desc2 <- as.data.frame(matrix(ncol=9))
  colnames(customer_desc2) <- c("CustomerID", "FirstProdDep", "MostRecentProdInd", "SecMostRecentProdInd", "FirstProdBought", "MostRecentProdFam", "SecRecentProdFam", "FirstProdFam", "MostRecentRep")
  
  customer_descid <- unique(customer_desc$CustomerID)
  
  #top 50 products
  prodids50 <- head(sort(table(customer_desc$ProductID),decreasing = T),50)
  
  #Creating table with variables for each customer regarding first, most recent, second most recent
  for(i in 1:length(unique(customer_descid))){
    customer_desc2[i,1] <- customer_descid[i]
    customer_desc2[i,2] <- customer_descind[head(which(customer_descind$CustomerID == customer_descid[i]),1),"ProductID"]
    customer_desc2[i,3] <- customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),1),"ProductID"]
    customer_desc2[i,4] <- customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),2)[1],"ProductID"]
    customer_desc2[i,5] <- customer_descind[head(which(customer_descind$CustomerID == customer_descid[i]),1),"ProductID"]
    customer_desc2[i,6] <- as.character(customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),1),"Family"])
    customer_desc2[i,7] <- as.character(customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),2)[1],"Family"])
    customer_desc2[i,8] <- as.character(customer_descind[head(which(customer_descind$CustomerID == customer_descid[i]),1),"Family"])
    customer_desc2[i,9] <- customer_descind[tail(which(customer_descind$CustomerID == customer_descid[i]),1),"SalesRepresentativeID"]
  }
  
  customer_desc2$MostRecentProdFam <- as.factor(customer_desc2$MostRecentProdFam)
  customer_desc2$SecRecentProdFam <- as.factor(customer_desc2$SecRecentProdFam)
  customer_desc2$FirstProdFam <- as.factor(customer_desc2$FirstProdFam)
  
  visits_basetableall <- merge(visits_basetableall, customer_desc2, by = "CustomerID", all.x = T)
  
}

if(train==TRUE){
  # Create Dummy Variables for ProductID -> Response Variables
  #Generate response for dependent period
  exampleDEP <- as.data.frame(customer_descdep$ProductID)
  names(exampleDEP) <- "ProductID"
  exampleDEP <- data.frame(CustomerID=customer_descdep$CustomerID, exampleDEP)
  exampleDEP$CustomerID <- as.character(exampleDEP$CustomerID)
  
  for(level in unique(products$ProductID)){
    exampleDEP[paste(level, sep = "_")] <- ifelse(exampleDEP$ProductID == level, 1, 0)
  }
  exampleDEP$ProductID <- NULL
  
  #aggregate by CustomerID 
  exampleDEP <- aggregate(as.matrix(exampleDEP[,2:318]) ~ CustomerID, FUN=max, na.rm=TRUE, data=exampleDEP)
  yDEP <- exampleDEP
  
}

#Generate response for independent period
exampleIND <- as.data.frame(customer_descind$ProductID)
names(exampleIND) <- "ProductID"
exampleIND <- data.frame(CustomerID=customer_descind$CustomerID, exampleIND)
exampleIND$CustomerID <- as.character(exampleIND$CustomerID)

for(level in unique(products$ProductID)){
  exampleIND[paste(level, sep = "_")] <- ifelse(exampleIND$ProductID == level, 1, 0)
}
exampleIND$ProductID <- NULL

#aggregate by CustomerID 
exampleIND <- aggregate(as.matrix(exampleIND[,2:318]) ~ CustomerID, FUN=max, na.rm=TRUE, data=exampleIND)
yIND <- exampleIND

# most frequent family
v.cust <- unique(visits_basetableall$CustomerID)
visits_basetableall$MostFreqFamily <- as.character("0")

for (i in 1:length(v.cust)){
  mff <- head(names(sort(table(customer_descind[which(customer_descind$CustomerID == v.cust[i]),"Family"]),decreasing=T)),1)
  visits_basetableall[which(visits_basetableall$CustomerID==v.cust[i]),"MostFreqFamily"] <- mff
}

visits_basetableall$MostFreqFamily <- as.factor(visits_basetableall$MostFreqFamily)

# most frequent product
visits_basetableall$MostFreqProduct <- as.character("0")

for (i in 1:length(v.cust)){
  mfp <- head(names(sort(table(customer_descind[which(customer_descind$CustomerID == v.cust[i]),"ProductID"]),decreasing=T)),1)
  visits_basetableall[which(visits_basetableall$CustomerID==v.cust[i]),"MostFreqProduct"] <- mfp
}

# most frequent sales rep
visits_basetableall$MostFreqSalesRep <- as.character("0")

for (i in 1:length(v.cust)){
  mfsr <- head(names(sort(table(customer_descind[which(customer_descind$CustomerID == v.cust[i]),"SalesRepresentativeID"]),decreasing=T)),1)
  visits_basetableall[which(visits_basetableall$CustomerID==v.cust[i]),"MostFreqSalesRep"] <- mfsr
}

# most frequent payment term
visits_basetableall$MostFreqPayTerm <- as.character("0")

for (i in 1:length(v.cust)){
  mfpt <- head(names(sort(table(customer_descind[which(customer_descind$CustomerID == v.cust[i]),"PaymentTerm"]),decreasing=T)),1)
  visits_basetableall[which(visits_basetableall$CustomerID==v.cust[i]),"MostFreqPayTerm"] <- mfpt
}
visits_basetableall$MostFreqPayTerm <- as.factor(visits_basetableall$MostFreqPayTerm)

#total number of visits,count visitID
customer_descind_numVisits <-  aggregate(customer_descind$VisitID,by=list(customer_descind$CustomerID),function(x)length(unique(x)))
colnames(customer_descind_numVisits) <- c("CustomerID","numVisits")
str(customer_descind_numVisits)

#Total amount spent, sum of total purchase
customer_descind_total <-  aggregate(list(customer_descind$Amount,customer_descind$N_Items),by=list(customer_descind$CustomerID),sum)
colnames(customer_descind_total) <- c("CustomerID","TotalAmount","TotalItems")
str(customer_descind_total)

#average spent per item by visit ID
customer_descind_merge <- merge(customer_descind_numVisits,customer_descind_total,by="CustomerID")
customer_descind_merge[,"AvergeSpent"] <- round(customer_descind_merge$TotalAmount/customer_descind_merge$TotalItems,1)

#total number of unique products of most recent purchase
a <- aggregate(customer_descind$VisitDate,by=list(customer_descind$CustomerID),max)
customer_descind$VisitDate <- as.character(customer_descind$VisitDate)
a$x <- as.character(a$x)
RecentVisitID <- customer_descind[customer_descind$VisitDate%in%a$x,]$VisitID
Recentpurchase <- customer_descind[customer_descind$VisitID%in%RecentVisitID,]
RecentNumproduct <- aggregate(Recentpurchase$ProductID,by=list(CustomerID=Recentpurchase$CustomerID),function(x)length(unique(x)))
colnames(RecentNumproduct)[2] <- "RecentNumproducts"

### final merged table includes total number of visits,Total amount spent,average spent per item,total number of prouduts of most recent purchase
customer_descind_3 <- merge(customer_descind_merge,RecentNumproduct,by="CustomerID")

visits_basetableall <- merge(visits_basetableall, customer_descind_3, by = "CustomerID", all.x = T)

customers <- merge(customers, routes, by = "RouteID", all.x = T)
visits_basetableall <- merge(visits_basetableall, customers, by = "CustomerID", all.x = T)

visits_basetableall$ProductID <- visits_basetableall$VisitID <- visits_basetableall$SalesRepresentativeID <- visits_basetableall$VisitDate <- NULL
visits_basetableall$Amount <- visits_basetableall$PaymentTerm <- visits_basetableall$Outcome <- visits_basetableall$N_Products <- visits_basetableall$N_Items <- NULL
visits_basetableall$Category <- visits_basetableall$Family <- NULL

visits_basetableall <- visits_basetableall[,-c(2,3,4,5,31,32)]
visits_basetableall <- unique(visits_basetableall)
visits_basetableall <- merge(visits_basetableall,exampleIND,by="CustomerID", all.x = T)
for(i in c(4,8,7,15)){
  visits_basetableall[,i] <- as.factor(visits_basetableall[,i])
}
  }
  
visits_basetableall <- readAndPrepareData()
  ## Modeling 
  if (evaluate==TRUE){
    cat("Evaluating model:")

# randomize order of indicators
allind <- sample(x=1:nrow(visits_basetableall),
                 size=nrow(visits_basetableall))

# split allind into three parts
trainind <- allind[1:round(length(allind)/3)]
valind <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))]
testind <- allind[(round(length(allind)*(2/3))+1):length(allind)]
allcustomerID <- c(visits_basetableall$CustomerID[trainind],visits_basetableall$CustomerID[valind],
                   visits_basetableall$CustomerID[testind])

trainKNN <- data.frame(sapply(visits_basetableall[trainind,c(2:338)], function(x) as.numeric(x)))
trainKNN <- na.omit(trainKNN)
valKNN <- data.frame(sapply(visits_basetableall[valind,c(2:338)], function(x) as.numeric(x)))
valKNN <- na.omit(valKNN)
testKNN <- data.frame(sapply(visits_basetableall[testind,c(2:338)], function(x) as.numeric(x)))
testKNN <- na.omit(testKNN)
trainbigKNN <- rbind(trainKNN,valKNN)



stdev <- sapply(trainKNN,sd)
means <- sapply(trainKNN,mean)
trainKNN <- data.frame(t((t(trainKNN)-means)/stdev))
valKNN <- data.frame(t((t(valKNN)-means)/stdev))
testKNN <- data.frame(t((t(testKNN)-means)/stdev))
trainbigKNN <- data.frame(t((t(trainbigKNN)-means)/stdev))
colnames(trainKNN)[21:337] <- colnames(valKNN)[21:337] <- colnames(testKNN)[21:337] <-colnames(trainbigKNN)[21:337] <-  colnames(yIND)[2:318]

trainKNN <- trainKNN[ , apply(trainKNN, 2, function(x) !any(is.na(x)))]
valKNN <- valKNN[ , apply(valKNN, 2, function(x) !any(is.na(x)))]
testKNN <- testKNN[ , apply(testKNN, 2, function(x) !any(is.na(x)))]
trainKNNbig<- trainbigKNN[ , apply(trainbigKNN, 2, function(x) !any(is.na(x)))]

ytrain <- yDEP[row.names(trainKNN),][,2:318]
yval <- yDEP[row.names(valKNN),][,2:318]
ytest <- yDEP[row.names(testKNN),][,2:318]
ytrain <- ytrain[,colnames(ytrain)%in%(colnames(trainKNN)[21:282])]
yval <- yval[,colnames(yval)%in%(colnames(valKNN)[21:282])]
ytrainbig <- rbind(ytrain,yval)
ytest <- ytest[,colnames(ytest)%in%(colnames(valKNN)[21:282])]
rmse <- numeric()

for (k in 10:20) {
  # Retreive the indicators of the nearest neighbors
  indicatorsKNN <- as.integer(knnx.index(data=trainKNN,
                                         query=valKNN,
                                         k=k))
  
  yval_pred <- data.frame(matrix(NA,nrow = nrow(yval),ncol = ncol(yval)))
  yval_pred[1,] <- apply(ytrain[indicatorsKNN[1:k],],2,mean)
  for (i in 2:nrow(valKNN)){
    yval_pred[i,] <- apply(ytrain[indicatorsKNN[((i-1)*k+1):((i-1)*k+k)],],2,mean)
  }
  
  yval_substra <- data.frame(matrix(NA,nrow = nrow(yval),ncol = ncol(yval)))
  for(i in 1:nrow(yval)){
    yval_substra[i,] <- abs(yval[i,] - yval_pred[i,])
  }
  colnames(yval_substra) <- colnames(yval)
  # sum(is.na(yval_substra))
  yval_substra <- apply(yval_substra,2,function(x)(as.numeric(x)))
  yval_substra_mean <- c()
  for(i in 1:nrow(yval_substra)){
    yval_substra_mean[i]<- summary(yval_substra[i,])[[4]]
  }
  
  rmse[k] <- sqrt(mean(yval_substra_mean^2))
  print(k)
}
plot(10:40,rmse[10:40],type="l", xlab="k")

(k <- which.min(rmse))

indicatorsKNN <- as.integer(knnx.index(data=trainKNNbig,
                                       query=testKNN,
                                       k=k))

ytest_pred <- data.frame(matrix(NA,nrow = nrow(ytest),ncol = ncol(ytest)))
ytest_pred[1,] <- apply(ytrainbig[indicatorsKNN[1:k],],2,mean)
for (i in 2:nrow(testKNN)){
  ytest_pred[i,] <- apply(ytrainbig[indicatorsKNN[((i-1)*k+1):((i-1)*k+k)],],2,mean)
}
ytest_pred <- ifelse(ytest_pred>=0.1,1,0)
ytest_pred <- as.data.frame(ytest_pred)
colnames(ytest_pred) <- colnames(ytest)

sum(ytest_pred==ytest)/(nrow(ytest)*ncol(ytest))*100
truepostive <- 0
falsepostive <- 0
truenagetive <- 0
falsenegatvie <- 0
for(i in 1:nrow(ytest)){
  for(j in 1:ncol(ytest)){
    if(ytest[i,j]==1&ytest_pred[i,j]==1){
      truepostive <- truepostive+1
    }else if(ytest[i,j]==0&ytest_pred[i,j]==1){
      falsepostive <- falsepostive+1
    }else if(ytest[i,j]==0&ytest_pred[i,j]==0){
      truenagetive <- truenagetive+1
    }else{
      falsenegatvie <- falsenegatvie+1
    }
  }
  print(i)
}
########### correctly predicting 1, accuracy 0.42
truepostive/sum(ytest==1)

########### correctly predicting 0, accuracy 0.92
truenagetive/sum(ytest==0)

cat("Percentage of times correctly predicting 1:",
    truepostive/sum(ytest==1),"\n")
cat("Percentage of times correctly predicting 0:",
    truenagetive/sum(ytest==0))

cat("TopDecileLift",TopDecileLift(ytest_pred,ytest))

(duration <- Sys.time() - start)
  }
cat("Creating model:")
#Build model on all data, ask for new train and test
indicatorsKNN <- as.integer(knnx.index(data=TrainKNN,
                                       query=TESTKNN,
                                       k=k))

ytest_pred <- data.frame(matrix(NA,nrow = nrow(ytest),ncol = ncol(ytest)))
ytest_pred[1,] <- apply(ytrainbig[indicatorsKNN[1:k],],2,mean)
for (i in 2:nrow(testKNN)){
  ytest_pred[i,] <- apply(ytrainbig[indicatorsKNN[((i-1)*k+1):((i-1)*k+k)],],2,mean)
}
ytest_pred <- ifelse(ytest_pred>=0.1,1,0)
ytest_pred <- as.data.frame(ytest_pred)
colnames(ytest_pred) <- colnames(ytest)
cat("Creating model:")

#Build model on all data
NEWTRAIN <- rbind(trainKNN,valKNN,testKNN)
indicatorsKNN <- as.integer(knnx.index(data=NEWTRAIN,
                                       query=NEWTRAIN,
                                       k=k))
yTRAIN <- rbind(ytrainbig,ytest)

ytest_pred <- data.frame(matrix(NA,nrow = nrow(yTRAIN),ncol = ncol(yTRAIN)))
ytest_pred[1,] <- apply(yTRAIN[indicatorsKNN[1:k],],2,mean)
for (i in 2:nrow(NEWTRAIN)){
  ytest_pred[i,] <- apply(yTRAIN[indicatorsKNN[((i-1)*k+1):((i-1)*k+k)],],2,mean)
}
ytest_pred <- ifelse(ytest_pred>=0.1,1,0)
ytest_pred <- as.data.frame(ytest_pred)
colnames(ytest_pred) <- colnames(ytest)
results <- cbind(allcustomerID,ytest_pred)

l <- list(indicatorsKNN = indicatorsKNN,
          readAndPrepareData = readAndPrepareData,
          f = f,
          length_ind = length_ind)
class(l) <- "allsellR"
return(l)

}

allsellModel <- allsellR(
  "08/01/2007",
  "31/12/2007",
  "01/01/2008",
  "31/12/2008",
  evaluate=TRUE)


predict.allsellR <- function(object, dumpDate) {
  #Load all required packages
  for (i in c("AUC","FNN")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i,
                       repos='http://cran.rstudio.com',quiet=TRUE)
      require(i,
              character.only=TRUE,
              quietly=TRUE)
    }
  }
  
  environment(object$readAndPrepareData) <- environment()
  #browser()
  basetable <- object$readAndPrepareData(train=FALSE,
                                         end_ind= as.Date(dumpDate, object$f),
                                         length_ind=object$length_ind)
  cat("Predicting: ")
  time <- Sys.time()
  ans <- data.frame(CustomerID=results$customerID,
                    Score=predict(object=object$indicatorsKNN,
                                  newdata=NEWTRAIN)
                                  )
 ans
}




