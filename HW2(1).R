library(data.table)
#Register,Trannum,Saledate

data<-fread("/Users/jhj930/Desktop/trnsact.csv",select=c(1,2,3,4,6))
colnames(data)<-c("SKU","Store","Register","Trannum","Saledate")
#select 20% of data

df<-data[which(data$Store>=7500),]##subset(data,data$Store==2600)


#coerce data into transaction data

install.packages(arules)
library("arules")



df$SKU<-as.factor(df$SKU)

df$Store<-as.numeric(df$Store)

df$Register<-as.numeric(df$Register)

df$Trannum<-as.numeric(df$Trannum)

df$Saledate<-as.Date(df$Saledate)
df$BasketID<-paste(df$Store,df$Register,df$Trannum,df$Saledate,collapse=NULL,sep=",")
df$BasketID<-as.factor(df$BasketID)

tran<-data.frame(df$BasketID,df$SKU)


#transaction<-as(tran,"transactions")
transaction<-write.csv(tran,file="transactf.csv",row.names=FALSE)
transaction<-read.transactions("transactf.csv",cols=c(1,2),format="single",rm.duplicates=TRUE)
rules<-as.data.frame()

#100 Potential Items
rules<-apriori(transaction,parameter=list(supp=0.00009,conf=0.25,minlen=2))

#Final 20 moves
#rules<-apriori(transaction,parameter=list(supp=0.0001,conf=0.2,minlen=2))
rules<-sort(rules, by="lift")
summary(rules)
inspect(rules)

rules.list<-as.data.frame(inspect(rules))
#pruning trees

#check if one rule is subset of other
subset.matrix<-is.subset(rules,rules)
#Have lower triangle along the diagonal as NA
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA;
#it returns colum that contains redundant 
redundant<-colSums(subset.matrix,na.rm=T)>=1
which(redundant)

#Prune redundant rules
rules.pruned<-rules[!redundant]

rules.pruned<-sort(rules, by="lift")

inspect(rules.pruned)

#Visualize Rules 

install.packages(arulesViz)
library(arulesViz)
plot(rules.pruned)
plot(rules,method="graph",control=list(type="items"))






#a<-as.factor(unique(df1$Store))
#b<-as.factor(unique(df1$Register))
#c<-as.factor(unique(df1$Trannum))
#d<-as.factor(unique(df1$Saledate))

#testdf<-aggregate(df1$SKU~df1$Store+df1$Register+df1$Trannum+df1$Saledate,data=df1,paste,sep=",")





##as(split(df[,"SKU"],df[,"Store"],df[,"Register"],df[,"Trannum"],df[,"Saledate"]),"transaction")

##df1<-as(as.matrix(df),"transactions")
##trans<-as(df,"transactions")

##df
##trans=read.transactions(df,format="basket",sep=",")


