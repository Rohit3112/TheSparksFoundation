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

#Deleting column 'Country' & Postal.Code
Retail$Country = NULL
Retail$Postal.Code = NULL
head(Retail)

#Assuming Sales column shows the total sale, Sales per Quantity (Unit Quantity) is calculated and shown in a different column 
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

#Shipment mode 
head(Retail)

ShipModeCount <- ggplot(data=Retail, aes(Ship.Mode, fill=Ship.Mode))
ShipModeCountP <- ShipModeCount + geom_bar() + ggtitle("Total Ship Mode Count")

ProfitvsShipMode <- ggplot(data=Retail, aes(x=Ship.Mode, y=Profit_Dollars))
ProfitvsShipModeP <- ProfitvsShipMode + geom_bar(aes(fill=Ship.Mode), stat="identity",position="dodge") + ggtitle("Profit vs Ship Mode")
  
SalesvsShipMode <- ggplot(data=Retail, aes(x=Ship.Mode, y=SalesPerQuant_Dollars))
SalesvsShipModeP <- SalesvsShipMode + geom_bar(aes(fill=Ship.Mode), stat="identity",position="dodge") + ggtitle("Sales vs Ship Mode")
  
ProfitvsSalesShip <- ggplot(data=Retail, aes(x=SalesPerQuant_Dollars, y=Profit_Dollars, colour=Ship.Mode))
ProfitvsSalesShipP <- ProfitvsSalesShip + geom_point(size=4, alpha=0.9) + ggtitle("Profit vs Sales (Ship.Mode)")

ProfitvsQuantShip <- ggplot(data=Retail, aes(x=Quantity, y=Profit_Dollars, colour=Ship.Mode))
ProfitvsQuantShipP <- ProfitvsQuantShip + geom_point(size=4, alpha=0.8) + ggtitle("Profit vs Quantity (Ship.Mode)")

ProfitvsDiscShip <- ggplot(data=Retail, aes(x=DiscountPerc, y=Profit_Dollars, colour=Ship.Mode))
ProfitvsDiscShipP <- ProfitvsDiscShip + geom_point(size=4, alpha=0.8) + ggtitle("Profit vs Discount (Ship.Mode)")

SalesvsProfitShip <- ggplot(data=Retail, aes(x=Profit_Dollars, y=SalesPerQuant_Dollars, colour=Ship.Mode))
SalesvsProfitShipP <- SalesvsProfitShip + geom_point(size=4, alpha=0.9) + ggtitle("Sales vs Profit (Ship.Mode)")

SalesvsQuantShip <- ggplot(data=Retail, aes(x=Quantity, y=SalesPerQuant_Dollars, colour=Ship.Mode))
SalesvsQuantShipP <- SalesvsQuantShip + geom_point(size=4, alpha=0.8) + ggtitle("Sales vs Quantity (Ship.Mode)")

SalesvsDiscShip <- ggplot(data=Retail, aes(x=DiscountPerc, y=SalesPerQuant_Dollars, colour=Ship.Mode))
SalesvsDiscShipP <- SalesvsDiscShip + geom_point(size=4, alpha=0.8) + ggtitle("Sales vs Discount (Ship.Mode)")

ShipmentMode <- ggarrange(ShipModeCountP, ProfitvsShipModeP, SalesvsShipModeP, ProfitvsSalesShipP, ProfitvsQuantShipP, ProfitvsDiscShipP, SalesvsProfitShipP, SalesvsQuantShipP, SalesvsDiscShipP, common.legend=TRUE, legend="bottom", ncol = 3, nrow = 3) 
ShipmentMode
#Maximum Mode of shipment is 'Standard' and Profit & Unit Sales is Maximum for Standard and First Class Shipment

as.data.frame(table(Retail$Ship.Mode))
#


#Categories
CategoryCount <- ggplot(data=Retail, aes(Category, fill=Category))
CategoryCount <- CategoryCount + geom_bar() + ggtitle("Total Category Count")
CategoryCount
#
as.data.frame(table(Retail$Category))
#
ProfitvsCat <- ggplot(data=Retail, aes(x=Category, y=Profit_Dollars))
ProfitvsCatP <- ProfitvsCat + geom_bar(aes(fill=Category), stat="identity",position="dodge") + ggtitle("Profit vs Category")

SalesvsCat <- ggplot(data=Retail, aes(x=Category, y=SalesPerQuant_Dollars))
SalesvsCatP <- SalesvsCat + geom_bar(aes(fill=Category), stat="identity",position="dodge") + ggtitle("Sales vs Category")

ProfitvsSalesCat <- ggplot(data=Retail, aes(x=SalesPerQuant_Dollars, y=Profit_Dollars, colour=Category))
ProfitvsSalesCatP <- ProfitvsSalesCat + geom_point(size=4, alpha=0.9) + ggtitle("Profit vs Sales (Category)")

ProfitvsQuantCat <- ggplot(data=Retail, aes(x=Quantity, y=Profit_Dollars, colour=Category))
ProfitvsQuantCatP <- ProfitvsQuantCat + geom_point(size=4, alpha=0.8) + ggtitle("Profit vs Quantity (Category)")

ProfitvsDiscCat <- ggplot(data=Retail, aes(x=DiscountPerc, y=Profit_Dollars, colour=Category))
ProfitvsDiscCatP <- ProfitvsDiscCat + geom_point(size=4, alpha=0.8) + ggtitle("Profit vs Discount (Category)")

SalesvsProfitCat <- ggplot(data=Retail, aes(x=Profit_Dollars, y=SalesPerQuant_Dollars, colour=Category))
SalesvsProfitCatP <- SalesvsProfitCat + geom_point(size=4, alpha=0.9) + ggtitle("Sales vs Profit (Category)")

SalesvsQuantCat <- ggplot(data=Retail, aes(x=Quantity, y=SalesPerQuant_Dollars, colour=Category))
SalesvsQuantCatP <- SalesvsQuantCat + geom_point(size=4, alpha=0.8) + ggtitle("Sales vs Quantity (Category)")

SalesvsDiscCat <- ggplot(data=Retail, aes(x=DiscountPerc, y=SalesPerQuant_Dollars, colour=Category))
SalesvsDiscCatP <- SalesvsDiscCat + geom_point(size=4, alpha=0.8) + ggtitle("Sales vs Discount (Category)")

CategoryPlot <- ggarrange(CategoryCount, ProfitvsCatP, SalesvsCatP, ProfitvsSalesCatP, ProfitvsQuantCatP, ProfitvsDiscCatP, SalesvsProfitCatP, SalesvsQuantCatP, SalesvsDiscCatP, common.legend=TRUE, legend="bottom", ncol = 3, nrow = 3) 
CategoryPlot
#Maximum no. is from Office Supplies, Profit and Unit Sales is Maximum in Technology 

#Sub Categories
SubCatCount <- ggplot(data=Retail, aes(Sub.Category))
SubCatCount <- SubCatCount + geom_bar(fill="IndianRed3") + guides(fill=FALSE) + ggtitle("Total Sub Category Count")
SubCatCount

SubCatPro <- ggplot(data=Retail, aes(x=Sub.Category, y=Quantity))
SubCatProP <- SubCatPro + geom_bar(fill="Skyblue3", stat="identity",position="dodge") + ggtitle("Quantity for Sub Category")

SubCatSale <- ggplot(data=Retail, aes(x=Sub.Category, y=DiscountPerc))
SubCatSaleP <- SubCatSale + geom_bar(fill="Violetred2", stat="identity",position="dodge") + ggtitle("Discount for Sub Category")

SubCategoryP <- ggarrange(SubCatCount, SubCatProP, SubCatSaleP, common.legend=TRUE, legend="bottom", ncol = 1, nrow = 3) 
SubCategoryP
#
FreqSubCategory <- as.data.frame(table(Retail$Sub.Category))

FreqSubCategory[order(FreqSubCategory$Freq, decreasing = T),]
#--------Maximum are from Binders, Paper, furnishings, Phones, storage, art, accessories and minimum from copiers, machines, suppliers

#State
l <- ggplot(data=Retail, aes(y=reorder(State ,State, function(y)+length(y))))

l + geom_bar(fill="lightslateblue", colour="Black") + ylab("State")

FreqState <- as.data.frame(table(Retail$State))
FreqState
FreqState[order(FreqState$Freq, decreasing = T),]
#--------Highest number of buyers are from California and New York

#Quantity
Quantity <- ggplot(data=Retail, aes(Quantity))
Quantity + geom_bar(fill="Chocolate", colour="Black") + ggtitle("Maximum Quantity Range")

#Region
library(scales)
Regionbar<- ggplot(Retail, aes(x="", y="", fill=Region)) + geom_bar(width = 1, stat = "identity")
Regionpie <- Regionbar + coord_polar("y", start=0)
Regionpie

FreqRegion <- as.data.frame(table(Retail$Region))
FreqRegion[order(FreqRegion$Freq, decreasing = T),]

#ProfitSales
#vs Segment
library(ggpubr)

#
FreqSegment <- as.data.frame(table(Retail$Segment))

#
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

ProfitEachStateMax <- aggregate(Profit_Dollars ~ State, data=Retail, max)

ProfitEachStateMin <- aggregate(Profit_Dollars ~ State, data=Retail, min)

ProfitEachState <- merge(ProfitEachStateMax, ProfitEachStateMin, by="State")
ProfitEachState$profit <- ProfitEachState[-1][cbind(1:nrow(ProfitEachState), max.col(abs(ProfitEachState[-1])))]
ProfitEachState$Profit_Dollars.x <- NULL
ProfitEachState$Profit_Dollars.y <- NULL

ProfitEachState <- ProfitEachState[order(ProfitEachState$profit, decreasing = T),]
ProfitEachState

#
ProfitState <- ggplot(data=ProfitEachState, aes(x=reorder(State, profit), y=profit))
ProfitStatePlot <- ProfitState + geom_bar(stat="identity", position="dodge", fill="DarkGreen")
ProfitStatePlot <- ProfitStatePlot + xlab("State") + ylab("Profit") + coord_flip()

SalesEachStateMax <- aggregate(SalesPerQuant_Dollars ~ State, data=Retail, max)
SalesEachState <- SalesEachStateMax[order(SalesEachStateMax$SalesPerQuant_Dollars, decreasing = T),]
SalesEachState
#
SalesState <- ggplot(data=SalesEachState, aes(x=reorder(State, SalesPerQuant_Dollars), y=SalesPerQuant_Dollars))
SalesStatePlot <- SalesState + geom_bar(stat="identity", position="dodge", fill="Red")
SalesStatePlot <- SalesStatePlot + xlab("State") + ylab("Unit Sales") + coord_flip()

ProfitSalesvsState <- ggarrange(ProfitStatePlot, SalesStatePlot, ncol = 2, nrow = 1)
ProfitSalesvsState 

#Vs Category
ProfitCat <- ggplot(data=Retail, aes(x=Category, y=Profit_Dollars, fill=Category))
ProfitCatPlot <- ProfitCat + geom_bar(stat="identity",position="dodge") + ylab("Profit") + ggtitle("Profit vs Category")

SalesCat <- ggplot(data=Retail, aes(x=Category, y=SalesPerQuant_Dollars, fill=Category))
SalesCatPlot <- SalesCat + geom_bar(stat="identity",position="dodge") + ylab("Sales") + coord_cartesian(ylim=c(0,4000)) + ggtitle("Sales vs Category")

ProfitvsCatQuant <- ggplot(data=Retail, aes(x=Category, y=Profit_Dollars, size=Quantity))
ProfitvsCatQuant <- ProfitvsCatQuant + geom_point(colour="LightSlateBlue", alpha=0.5) + ggtitle("Profit vs Category(Quantity)")

SalesvsCatQuant <- ggplot(data=Retail, aes(x=Category, y=Sales_Dollars, size=Quantity))
SalesvsCatQuant <- SalesvsCatQuant + geom_point(colour="LightSlateBlue", alpha=0.5) + ggtitle("Sales vs Category(Quantity)")

ProfitSalesvsCat <- ggarrange(ProfitCatPlot, SalesCatPlot, ProfitvsCatQuant, SalesvsCatQuant, legend="bottom", ncol = 2, nrow = 2) 
ProfitSalesvsCat


#
#Vs Subcategory
ProfitSCat <- ggplot(data=Retail, aes(x=Sub.Category, y=Profit_Dollars, fill=Category))
ProfitSCatPlot <- ProfitSCat + geom_bar(stat="identity",position="dodge") + ylab("Profit") + xlab("Sub Category") + ggtitle("Profit vs Sub Category") 

SalesSCat <- ggplot(data=Retail, aes(x=Sub.Category, y=SalesPerQuant_Dollars, fill=Category))
SalesSCatPlot <- SalesSCat + geom_bar(stat="identity",position="dodge") + ylab("Sales") + xlab("Sub Category") + coord_cartesian(ylim=c(0,4000)) + ggtitle("Sales vs Sub Category") 

ProfitSalesvsSCat <- ggarrange(ProfitSCatPlot, SalesSCatPlot, common.legend=TRUE, legend="bottom", ncol = 1, nrow = 2) 
ProfitSalesvsSCat
#
ProfitSCat_Quant <- ggplot(data=Retail, aes(x=Sub.Category, y=Profit_Dollars, size=Quantity, colour=Category)) 
ProfitSCatPlot_Quant <- ProfitSCat_Quant + geom_point(alpha=0.4) + ylab("Profit") + xlab("Sub Category") + ggtitle("Highest Profit Quantity (For each Sub Category")  

SalesSCat_Quant <- ggplot(data=Retail, aes(x=Sub.Category, y=SalesPerQuant_Dollars, size=Quantity, colour=Category))
SalesSCatPlot_Quant <- SalesSCat_Quant + geom_point(alpha=0.4) + ylab("Profit") + xlab("Sub Category") + ggtitle("Highest Profit Quantity (For each Sub Category")  

ProfitSalesvsSCat_Quant <- ggarrange(ProfitSCatPlot_Quant, SalesSCatPlot_Quant, common.legend=TRUE, legend="bottom", ncol = 1, nrow = 2) 
ProfitSalesvsSCat_Quant

#Analysis for Discount
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
#Maximum Discount is for Furniture category and maximum discount is from 0-20%

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

DiscountSubQuantLoss <- ggarrange(DiscountLossTechSG, DiscountLossFurSG, DiscountLossOfficeSG, DiscountvsQuantLossTech, DiscountvsQuantLossFur, DiscountvsQuantLossOff, ncol = 3, nrow = 2) 
DiscountSubQuantLoss
#Large Quantity of Binders give a discount of around 70% or more, for profit < 0



#As a business manager, try to find out the weak areas where you can work to make more profit?

#---------Profit is maximum in Corporate Segment and minimum in Home Office Segment and Unit Sales are maximum in Home Office Segment
#---------Profit is maximum in Central region and least in Southern Region------- Unit Sales is maximum in Southern region
#---------highest profit is for Indiana,Washington & Delaware--------- loss is for Ohio, North Carolina & Texas
#---------Technology and Office Supplies have high profit------- Furniture is least profitable among the three categories
#---------Machines sub-category have highest unit sales--------- Machines have higher unit sales but lesser profit compared to Copiers-------- Binders and Machines sub-categories facing huge loss














         
 
