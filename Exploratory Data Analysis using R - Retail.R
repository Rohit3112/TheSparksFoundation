#Importing Data into R 
Retail <- read.csv(file.choose())
Retail

#Checking summary of the Data 
head(Retail)
tail(Retail)
str(Retail)
summary(Retail)

#Making changes in rows and columns to get a clearer picture of the Data given 
#We can see that a most of the countries listed in the 'Country' column is in fact 'United States', so let's check if there is any other country in the columns using the 'stats' function

Retail[!Retail$Country == "United States",] #OR
Retail[!which(Retail$Country == "United States"),]

#We can see that all rows have 'United States' in the 'Country' column. Since the country for all the rows is the same, the column is not of any use and it only occupies more space in the console. Hence we shall delete it.

#Deleting column 'Country' & Postal.Code & Region
Retail$Country = NULL
Retail$Postal.Code = NULL
head(Retail)

#Assuming Sales column shows the total sale, Sales per Quantity is calculated and shown in a different column 
Retail$SalesPerQuant <- Retail$Sales/Retail$Quantity
Retail$SalesPerQuant
head(Retail)

str(Retail)

#Shifting Numeration of SalesPerQuant 
Retail <- Retail[c(1,2,3,4,5,6,7,8,9,13,10,11,12)]
head(Retail)

#Checking for missing data in the file 
Retail[!complete.cases(Retail),] #Result shows that there is no missing data 

#Expressing Discount as % 
Retail$Discount <- Retail$Discount * 100 
head(Retail)

#Renaming columns 
colnames(Retail)[names(Retail) == "Sales"] <- "Sales_Dollars"
colnames(Retail)[names(Retail) == "SalesPerQuant"] <- "SalesPerQuant_Dollars"
colnames(Retail)[names(Retail) == "Discount"] <- "DiscountPerc"
colnames(Retail)[names(Retail) == "Profit"] <- "Profit_Dollars"
head(Retail)

#Rounding off the Numeric Data for columns having $
Retail[,"Sales_Dollars"] <- round(Retail[,"Sales_Dollars"], 2)
Retail[,"SalesPerQuant_Dollars"] <- round(Retail[,"SalesPerQuant_Dollars"], 2)
Retail[,"Profit_Dollars"] <- round(Retail[,"Profit_Dollars"], 2)
head(Retail)

#GGplot2
library(ggplot2)
u <- ggplot(data=Retail, aes(x=Profit_Dollars, y=SalesPerQuant_Dollars, colour=Category, size=Quantity))
u + geom_point(alpha=0.4) + facet_grid(Category~Sub.Category)

y <- ggplot(data=Retail, aes(x=Category, y=Profit_Dollars, size=Quantity))
y + geom_point(colour="Blue", alpha=0.5)

v <- ggplot(data=Retail, aes(x=Sub.Category, y=SalesPerQuant_Dollars, size=Quantity, colour=Category))
v + geom_point(alpha=0.4)
                              
w <- ggplot(data=Retail, aes(x=Sub.Category, y=DiscountPerc, colour=Category))
w + geom_line(size=3)
                              
w <- ggplot(data=Retail, aes(x=Sub.Category, y=DiscountPerc, colour=Category, size=Quantity))
w + geom_point()

#Discount vs Profit
#
library(tidyverse)
DiscountRetail <- ggplot(data=Retail, aes(DiscountPerc))
DiscountRetail <- DiscountRetail + geom_bar(fill="lightslateblue", colour="Black") + ggtitle("Maximum Discount Range")

Discountvsprofit <- ggplot(data=Retail, aes(x=DiscountPerc, y=Profit_Dollars, colour=Category, size=Quantity))
Discountvsprofit <- Discountvsprofit + geom_point(alpha=0.5) + xlim(0,100) + ylim(-4000,0) + ggtitle("Discount vs Profit (Category)")

DiscountRetailLoss <- ggplot(data=Retail_loss, aes(DiscountPerc))
DiscountRetailLoss <- DiscountRetailLoss + geom_bar(fill="lightslateblue", colour="Black") + ggtitle("Maximum Discount Range for Profit < 0")

DiscountvsprofitLoss <- ggplot(data=Retail_loss, aes(x=DiscountPerc, y=Profit_Dollars, colour=Category, size=Quantity))
DiscountvsprofitLoss <- DiscountvsprofitLoss + geom_point(alpha=0.5) + xlim(0,100) + ylim(-4000,0) + ggtitle("Discount vs Profit (Category) for Profit<0")

DiscountOverview<- ggarrange(DiscountRetail, Discountvsprofit, DiscountRetailLoss, DiscountvsprofitLoss, ncol = 2, nrow = 2) 
DiscountOverview

#Discount for whole retail
RetailFurniture <- Retail[Retail$Category=="Furniture",]
RetailFurniture

RetailOfficeSupplies <- Retail[Retail$Category=="Office Supplies",]
RetailOfficeSupplies

RetailTechnology <- Retail[Retail$Category=="Technology",]
RetailTechnology

DiscountTechSG <- ggplot(data=RetailTechnology, aes(x=DiscountPerc))
DiscountTechSG <- DiscountTechSG + geom_histogram(binwidth=10, aes(fill=Sub.Category), colour="Black") + ggtitle("Discount(Technology)")

DiscountFurSG <- ggplot(data=RetailFurniture, aes(x=DiscountPerc))
DiscountFurSG <- DiscountFurSG + geom_histogram(binwidth=10, aes(fill=Sub.Category), colour="Black") + ggtitle("Discount(Furniture)")

DiscountOfficeSG <- ggplot(data=RetailOfficeSupplies, aes(x=DiscountPerc))
DiscountOfficeSG <- DiscountOfficeSG + geom_histogram(binwidth=10, aes(fill=Sub.Category), colour="Black") + ggtitle("Discount(OfficeSupplies)")

DiscountvsQuantTech <- ggplot(data=RetailTechnology, aes(x=DiscountPerc, y=Quantity, colour=Sub.Category))
DiscountvsQuantTech <- DiscountvsQuantTech + geom_jitter(size=0.9) + geom_boxplot(size=1.2, alpha=0.5) + ggtitle("Discount vs Quantity in Technology")

DiscountvsQuantOff <- ggplot(data=RetailOfficeSupplies, aes(x=DiscountPerc, y=Quantity, colour=Sub.Category))
DiscountvsQuantOff <- DiscountvsQuantOff + geom_jitter(size=0.9) + geom_boxplot(size=1.2, alpha=0.5) + ggtitle("Discount vs Quantity in OfficeSupplies")

DiscountvsQuantFur <- ggplot(data=RetailFurniture, aes(x=DiscountPerc, y=Quantity, colour=Sub.Category))
DiscountvsQuantFur <- DiscountvsQuantFur + geom_jitter(size=0.9) + geom_boxplot(size=1.2, alpha=0.5) + ggtitle("Discount vs Quantity in Furniture")

DiscountSubQuant <- ggarrange(DiscountTechSG, DiscountFurSG, DiscountOfficeSG, DiscountvsQuantTech, DiscountvsQuantFur, DiscountvsQuantOff, ncol = 3, nrow = 2) 
DiscountSubQuant

#Give me onlt the rows for which Profit <= 0 
Retail_loss <- Retail[Retail$Profit_Dollars<0,]
Retail_loss

Retail_Furniture <- Retail_loss[Retail_loss$Category=="Furniture",]
Retail_Furniture

Retail_OfficeSupplies <- Retail_loss[Retail_loss$Category=="Office Supplies",]
Retail_OfficeSupplies

Retail_Technology <- Retail_loss[Retail_loss$Category=="Technology",]
Retail_Technology

Retail_Technology[Retail_Technology$Discount>=40,]
Retail_Furniture[Retail_Furniture$Discount>=40,]
Retail_OfficeSupplies[Retail_OfficeSupplies$Discount>=40,]


#

DiscountLossTechSG <- ggplot(data=Retail_Technology, aes(x=DiscountPerc))
DiscountLossTechSG <- DiscountLossTechSG + geom_histogram(binwidth=10, aes(fill=Sub.Category), colour="Black") + coord_cartesian(ylim=c(0,150)) + ggtitle("Discount(Technology) profit<0")

DiscountLossFurSG <- ggplot(data=Retail_Furniture, aes(x=DiscountPerc))
DiscountLossFurSG <- DiscountLossFurSG + geom_histogram(binwidth=10, aes(fill=Sub.Category), colour="Black") + coord_cartesian(ylim=c(0,250)) + ggtitle("Discount(Furniture) profit<0")

DiscountLossOfficeSG <- ggplot(data=Retail_OfficeSupplies, aes(x=DiscountPerc))
DiscountLossOfficeSG <- DiscountLossOfficeSG + geom_histogram(binwidth=10, aes(fill=Sub.Category), colour="Black") + ggtitle("Discount(OfficeSupplies) profit<0")

DiscountvsQuantLossTech <- ggplot(data=Retail_Technology, aes(x=DiscountPerc, y=Quantity, colour=Sub.Category))
DiscountvsQuantLossTech <- DiscountvsQuantLossTech + geom_jitter(size=0.9) + geom_boxplot(size=1.2, alpha=0.5) + ggtitle("DiscountvsQuantity (profit<0 in Technology)")

DiscountvsQuantLossOff <- ggplot(data=Retail_OfficeSupplies, aes(x=DiscountPerc, y=Quantity, colour=Sub.Category))
DiscountvsQuantLossOff <- DiscountvsQuantLossOff + geom_jitter(size=0.9) + geom_boxplot(size=1.2, alpha=0.5) + ggtitle("DiscountvsQuantity (profit<0 in OfficeSupplies)")

DiscountvsQuantLossFur <- ggplot(data=Retail_Furniture, aes(x=DiscountPerc, y=Quantity, colour=Sub.Category))
DiscountvsQuantLossFur <- DiscountvsQuantLossFur + geom_jitter(size=0.9) + geom_boxplot(size=1.2, alpha=0.5) + ggtitle("DiscountvsQuantity (profit<0 in Furniture)")

DiscountSubQuantLoss <- ggarrange(DiscountLossTechSG, DiscountLossFurSG, DiscountLossOfficeSG, DiscountvsQuantLossTech, DiscountvsQuantLossOff, DiscountvsQuantLossFur, ncol = 3, nrow = 2) 
DiscountSubQuantLoss

#Shipment mode 
head(Retail)

m <- ggplot(data=Retail, aes(x=Ship.Mode, y=Profit_Dollars))
m + geom_bar(aes(fill=Ship.Mode), stat="identity",position="dodge")
  
n <- ggplot(data=Retail, aes(x=Ship.Mode, y=SalesPerQuant_Dollars))
n + geom_bar(aes(fill=Ship.Mode), stat="identity",position="dodge")
  
c <- ggplot(data=Retail, aes(x=SalesPerQuant_Dollars, y=Profit_Dollars, colour=Ship.Mode))
c + geom_point(size=4, alpha=0.9)

d <- ggplot(data=Retail, aes(x=Quantity, y=Profit_Dollars, colour=Ship.Mode))
d + geom_point(size=4, alpha=0.8)

e <- ggplot(data=Retail, aes(x=DiscountPerc, y=Profit_Dollars, colour=Ship.Mode))
e + geom_point(size=4, alpha=0.8)

f <- ggplot(data=Retail, aes(Ship.Mode, fill=Ship.Mode))
f + geom_bar()

as.data.frame(table(Retail$Ship.Mode))

#Categories
g <- ggplot(data=Retail, aes(Category, fill=Category))
g + geom_bar()
as.data.frame(table(Retail$Category))

h <- ggplot(data=Retail, aes(x=SalesPerQuant_Dollars, y=Profit_Dollars, colour=Category))
h + geom_point(size=4, alpha=0.9)

i <- ggplot(data=Retail, aes(x=Quantity, y=Profit_Dollars, colour=Category))
i + geom_point(size=4, alpha=0.8)

j <- ggplot(data=Retail, aes(x=DiscountPerc, y=Profit_Dollars, colour=Category))
j + geom_point(size=4, alpha=0.8)

head(Retail)

#Sub Categories
k <- ggplot(data=Retail, aes(Sub.Category, fill=Sub.Category))
k + geom_bar()

FreqSubCategory <- as.data.frame(table(Retail$Sub.Category))
FreqSubCategory
FreqSubCategory[order(FreqSubCategory$Freq, decreasing = T),]


#State
l <- ggplot(data=Retail, aes(y=reorder(State ,State, function(y)+length(y))))

l + geom_bar(fill="lightslateblue", colour="Black") + ylab("State")

FreqState <- as.data.frame(table(Retail$State))
FreqState
FreqState[order(FreqState$Freq, decreasing = T),]

#Quantity
Quantity <- ggplot(data=Retail, aes(Quantity))
Quantity + geom_bar(fill="lightslateblue", colour="Black") + ggtitle("Maximum Quantity Range")

#Region
library(scales)
Regionbar<- ggplot(Retail, aes(x="", y="", fill=Region)) + geom_bar(width = 1, stat = "identity")
#Regionpie <- Regionbar + coord_polar("y", start=0)
#Regionpie

#ProfitSales
#vs Segment
library(ggpubr)
ProfitSeg <- ggplot(data=Retail, aes(x=Segment, y=Profit_Dollars, fill=Segment))
ProfitSegPlot <- ProfitSeg + geom_bar(stat="identity",position="dodge")

SalesSeg <- ggplot(data=Retail, aes(x=Segment, y=SalesPerQuant_Dollars, fill=Segment))
SalesSegPlot <- SalesSeg + geom_bar(stat="identity",position="dodge") + coord_cartesian(ylim=c(0,4000))

ProfitSalesvsSeg <- ggarrange(ProfitSegPlot, SalesSegPlot + rremove("x.text"), ncol = 2, nrow = 1)
ProfitSalesvsSeg


#Vs Region
ProfitReg <- ggplot(data=Retail, aes(x=Region, y=Profit_Dollars, fill=Region))
ProfitRegPlot <- ProfitReg + geom_bar(stat="identity",position="dodge")

SalesReg <- ggplot(data=Retail, aes(x=Region, y=SalesPerQuant_Dollars, fill=Region))
SalesRegPlot <- SalesReg + geom_bar(stat="identity",position="dodge") + coord_cartesian(ylim=c(0,4000))

ProfitSalesvsReg <- ggarrange(ProfitRegPlot, SalesRegPlot + rremove("x.text"))
ProfitSalesvsReg

#Vs State
library(tidyverse)

#df <- Retail[order(Retail$Profit_Dollars ,decreasing = TRUE),]
#barplot(df$num,names.arg = df$cat)

ProfitEachStateMax <- aggregate(Profit_Dollars ~ State, data=Retail, max)

ProfitEachStateMin <- aggregate(Profit_Dollars ~ State, data=Retail, min)

ProfitEachState <- merge(ProfitEachStateMax, ProfitEachStateMin, by="State")
ProfitEachState$profit <- ProfitEachState[-1][cbind(1:nrow(ProfitEachState), max.col(abs(ProfitEachState[-1])))]
ProfitEachState$Profit_Dollars.x <- NULL
ProfitEachState$Profit_Dollars.y <- NULL

ProfitEachState <- ProfitEachState[order(ProfitEachState$profit, decreasing = T),]
ProfitEachState

ProfitState <- ggplot(data=ProfitEachState, aes(x=reorder(State, profit), y=profit))
ProfitStatePlot <- ProfitState + geom_bar(stat="identity", position="dodge", fill="DarkGreen")
ProfitStatePlot <- ProfitStatePlot + xlab("State") + ylab("Profit") + coord_flip()


SalesEachStateMax <- aggregate(SalesPerQuant_Dollars ~ State, data=Retail, max)
SalesEachState <- SalesEachStateMax[order(SalesEachStateMax$SalesPerQuant_Dollars, decreasing = T),]
SalesEachState

SalesState <- ggplot(data=SalesEachState, aes(x=reorder(State, SalesPerQuant_Dollars), y=SalesPerQuant_Dollars))
SalesStatePlot <- SalesState + geom_bar(stat="identity", position="dodge", fill="Red")
SalesStatePlot <- SalesStatePlot + xlab("State") + ylab("Unit Sales") + coord_flip()

ProfitSalesvsState <- ggarrange(ProfitStatePlot, SalesStatePlot, ncol = 2, nrow = 1)
ProfitSalesvsState 

#Vs Category
ProfitCat <- ggplot(data=Retail, aes(x=Category, y=Profit_Dollars, fill=Category))
ProfitCatPlot <- ProfitCat + geom_bar(stat="identity",position="dodge") + ylab("Profit") + ggtitle("Profit vs Category")
ProfitCatPlot

SalesCat <- ggplot(data=Retail, aes(x=Category, y=SalesPerQuant_Dollars, fill=Category))
SalesCatPlot <- SalesCat + geom_bar(stat="identity",position="dodge") + ylab("Sales") + coord_cartesian(ylim=c(0,4000)) + ggtitle("Sales vs Category")
SalesCatPlot

ProfitSalesvsCat <- ggarrange(ProfitCatPlot, SalesCatPlot, common.legend=TRUE, legend="bottom", ncol = 2, nrow = 1) 
ProfitSalesvsCat

#Vs Subcategory
ProfitSCat <- ggplot(data=Retail, aes(x=Sub.Category, y=Profit_Dollars, fill=Category))
ProfitSCatPlot <- ProfitSCat + geom_bar(stat="identity",position="dodge") + ylab("Profit") + xlab("Sub Category") + ggtitle("Profit vs Sub Category") 

SalesSCat <- ggplot(data=Retail, aes(x=Sub.Category, y=SalesPerQuant_Dollars, fill=Category))
SalesSCatPlot <- SalesSCat + geom_bar(stat="identity",position="dodge") + ylab("Sales") + xlab("Sub Category") + coord_cartesian(ylim=c(0,4000)) + ggtitle("Sales vs Sub Category") 

ProfitSalesvsSCat <- ggarrange(ProfitSCatPlot, SalesSCatPlot, common.legend=TRUE, legend="bottom", ncol = 1, nrow = 2) 
ProfitSalesvsSCat






         
 