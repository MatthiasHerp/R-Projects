
#####IS Gold a Safe Haven?######
#####Matthias Herp#####

#Installing Packages
install.packages("readxl") 
install.packages("data.table")
install.packages("xtable")
install.packages("ggplot2")
install.packages("tikzDevice")
install.packages("scales")
install.packages("roll")
install.packages("dplyr")
install.packages("knitr")
install.packages("kableExtra")

#Loading Libraries
library(knitr)
library(kableExtra)
library(ggplot2)
library(xtable)
library(readxl)
library(data.table)
library(ggplot2)
library(scales)
library(roll)
library(dplyr)

## set working directory to path of R-script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

######Homework assignment Slide Set 3#####
#Task 1
#1
Name = c("Hans","Erika","Peter")
Age = c(22,19,20)
Weight = c(72,65,76)
Matrix = data.frame(Name,Age,Weight)

#2
save(Matrix, file = "Matrix.RData")

#3
rm(list = ls())

#4
load("Matrix.RData")

#5
Weight_n = as.numeric(c(72,65,76))

#6
mean(Weight_n)

#Task 2
#1
New_Matrix = data.frame(Matrix$Age,Matrix$Weight)

#2
a <- c("Age","Weight")
for (i in 1:2){
  print(paste0(a[i], " Maximum:", max(New_Matrix[i])))
}
#3
lapply(New_Matrix, max)

#Task3
#2
kable(New_Matrix, format = "latex", booktabs = TRUE, caption = "Table from Task 2") %>%
  writeLines("/Users/maherp/Desktop/Universität/R-Tools Kurs/R & Latech Dateien/Table_from_Task_2.tex")

Additional_Results <- data.frame(mean(Weight_n),lapply(New_Matrix, max))
kable(Additional_Results, format = "latex", booktabs = TRUE, caption = "Additional Results") %>%
  writeLines("/Users/maherp/Desktop/Universität/R-Tools Kurs/R & Latech Dateien/Additional_Results.tex")

#####End, Task 3 in Latex#####


#Importing and editing data

#Importing Data on gold, MSCI world, S&P 500 from excel table
Gold_MSCI_World_SP_500.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/gold_safehaven.xlsx", 
                                                sheet = 1, range = "A1:F5228", col_names = TRUE)) 

#Editing the Gold Data
#Data Frame with the exchange rate, the closing value and the daily returns is built
n <- length(Gold_MSCI_World_SP_500.xlsx$`S&P GSCI Gold Spot - PRICE INDEX`)
a <- as.numeric(as.character(c((Gold_MSCI_World_SP_500.xlsx$`S&P GSCI Gold Spot - PRICE INDEX`[2:n]/Gold_MSCI_World_SP_500.xlsx$`S&P GSCI Gold Spot - PRICE INDEX`[1:(n-1)]-1))))
class(a)
Gold <- data.frame("Exchange_Date"=Gold_MSCI_World_SP_500.xlsx$...1, 
                   "Closing_Value"=Gold_MSCI_World_SP_500.xlsx$`S&P GSCI Gold Spot - PRICE INDEX`,
                   "Return"=c("NA",a))
Gold$Return <-as.numeric(as.character(Gold$Return))
Gold

#Editing the MSCI World Data
#Data Frame with the exchange rate, the closing value and the daily returns is built
n <- length(Gold_MSCI_World_SP_500.xlsx$`MSCI WORLD U$ - PRICE INDEX`)
a <- as.numeric(as.character(c((Gold_MSCI_World_SP_500.xlsx$`MSCI WORLD U$ - PRICE INDEX`[2:n]/Gold_MSCI_World_SP_500.xlsx$`MSCI WORLD U$ - PRICE INDEX`[1:(n-1)]-1))))

MSCI_World <- data.frame("Exchange_Date"=Gold_MSCI_World_SP_500.xlsx$...1, 
                         "Closing_Value"=Gold_MSCI_World_SP_500.xlsx$`MSCI WORLD U$ - PRICE INDEX`,
                         "Return"=c("NA",a))
MSCI_World$Return <- as.numeric(as.character(MSCI_World$Return))

#Editing the S&P 500 Data
#Data Frame with the exchange rate, the closing value and the daily returns is built
n <- length(Gold_MSCI_World_SP_500.xlsx$`S&P 500 COMPOSITE - PRICE INDEX`)
a <- c((Gold_MSCI_World_SP_500.xlsx$`S&P 500 COMPOSITE - PRICE INDEX`[2:n]/Gold_MSCI_World_SP_500.xlsx$`S&P 500 COMPOSITE - PRICE INDEX`[1:(n-1)]-1))

SP_500 <- data.frame("Exchange_Date"=Gold_MSCI_World_SP_500.xlsx$...1, 
                     "Closing_Value"=Gold_MSCI_World_SP_500.xlsx$`S&P 500 COMPOSITE - PRICE INDEX`,
                     "Return"=c("NA",a))
SP_500$Return <- as.numeric(as.character(SP_500$Return))




#Visualizing the Data

#Defining a Datatablle and a vector of names for loops
DataTabellesList = list(Gold, MSCI_World, SP_500)
Indexes <- c("Gold","MSCI_World", "SP_500")
#1.Creating tables with the exchange date and the closing values per variable
for(i in 1:length(Indexes))
{ 
  assign(paste0(Indexes[[i]], "_Date_Value"), 
         data.frame("Exchange_Date"=DataTabellesList[[i]]$`Exchange_Date`,
                    "Value"=DataTabellesList[[i]]$Closing_Value))
}
#2.Joining all the Tables of Closing Values by the Exchange Rate
Date_Values <- left_join(Gold_Date_Value, MSCI_World_Date_Value, by = "Exchange_Date") %>%
  left_join(.,SP_500_Date_Value, by = "Exchange_Date") 

#3.creates a Vektor with the Index Names + _Value for naming the joined table
a <- c()
for(i in 1:length(Indexes))
{ 
  a[i] <- print(paste0(Indexes[[i]], "_Values"))
}
a
Indexes_Values <- a

#4.Naming the Columns of the table and defining the exchange rate as a date variable
colnames(Date_Values) <-c("Exchange_Date", Indexes_Values)
Date_Values$Exchange_Date <- as.Date(Date_Values$Exchange_Date, tryFormats = c("%Y-%m-%d"))

#Counting the number of NAs to check for missing values
length(which(is.na(Date_Values)))


#5.Plotting of all indices closing values with the exchange rate on the x-axis
# and exporting the plot as a pdf to import it in latex
All_Indices_Plot <- ggplot(Date_Values,  aes(x = Exchange_Date)) + 
  geom_line(aes(y = Gold_Values), color = "Gold") + 
  geom_line(aes(y = MSCI_World_Values), color = "Blue") +
  geom_line(aes(y = SP_500_Values), color = "Red") +
  labs(x = "Exchange Date", y = "Closing Values") +
  labs(title="Closing Values of Indices", 
       caption="Gold: Gold, Blue: MSCI_World, Red: SP_500" ) +
  theme_bw() +
  scale_x_date( labels = date_format("%Y-%m-%d"))
All_Indices_Plot
pdf("All_Indices_Plot_D2.pdf", height=5, width=7)
plot(All_Indices_Plot)
dev.off()




#Providing Summary Statistics on the daily returns

#1.Building vectors with the daily returns for each variable called "Variable_Return"
DataTabellesList = list(Gold, MSCI_World, SP_500)
for (i in 1:length(DataTabellesList))
{ 
  assign(paste0(Indexes[[i]], "_Return"), 
         data.table("%Return"=DataTabellesList[[i]]$`Return`[!is.na(DataTabellesList[[i]]$`Return`)]))
}

#Building Summary Statistics Tabel 1 with the minimum, 25% quantile, median, 75% quantile, maximum
#2.Defining a list with all the return vectors
DataTabellesList_Return = list(Gold_Return, MSCI_World_Return, 
                               SP_500_Return)
#3.Running a loop building descriptive statistics tabel for each variable
for (i in 1:length(DataTabellesList_Return))
{ 
  assign(paste0("Summary_Statistics_1_", Indexes[[i]]), 
         c(
           round(min(DataTabellesList_Return[[i]]$`%Return`*100), 5) , 
           round(quantile(DataTabellesList_Return[[i]]$`%Return`*100)[2], 5), 
           round(median(DataTabellesList_Return[[i]]$`%Return`*100), 5), 
           round(quantile(DataTabellesList_Return[[i]]$`%Return`*100)[4], 5),
           round(max(DataTabellesList_Return[[i]]$`%Return`*100), 5)
         ))
}
#4.Merging the variable tables to one
Summary_Statistics_Tabelle_1 <- matrix(c(Summary_Statistics_1_Gold,
                                         Summary_Statistics_1_MSCI_World,
                                         Summary_Statistics_1_SP_500),
                                       ncol=5,  byrow=TRUE)
#5.Defining the Columns and Rows of the Table and exporting it to a tex file for latex
colnames(Summary_Statistics_Tabelle_1) <- c( 
  "Min.","Q25%", "Med.", "Q75%",
  "Max.")
rownames(Summary_Statistics_Tabelle_1) <- Indexes
Summary_Statistics_Tabelle_1 <- as.table(Summary_Statistics_Tabelle_1)
Summary_Statistics_Tabelle_1
print(xtable(Summary_Statistics_Tabelle_1, caption = "Daily Returns in Percent", type = "latex"),
      caption.placement = 'bottom',
      size="\\fontsize{7pt}{8pt}",
      file = "/Users/maherp/Desktop/Universität/R-Tools Kurs/R & Latech Dateien/SummaryStatisticsTabelle1_D2.tex")

kable(Summary_Statistics_Tabelle_1, format = "latex", booktabs = TRUE, caption = "Table 1: Returns in Percent ") %>%
  writeLines("/Users/maherp/Desktop/Universität/R-Tools Kurs/R & Latech Dateien/SummaryStatisticsTabelle1.1_D2.tex")


#Building Summary Statistics Tabel 1 with the mean, standard deviation, interquartile range, number of
#observations, frequency of data
#1.Running a loop building descriptive statistics tabel for each variable
for (i in 1:length(DataTabellesList_Return))
{ 
  assign(paste0("Summary_Statistics_2_", Indexes[[i]]), 
         c(round(mean(DataTabellesList_Return[[i]]$`%Return`*100), 2),
           round(sd(DataTabellesList_Return[[i]]$`%Return`*100), 2),
           round(IQR(DataTabellesList_Return[[i]]$`%Return`*100), 2), 
           nrow(DataTabellesList_Return[[i]]), 
           "Daily"))
}
#2.Merging the variable tables to one
Summary_Statistics_Tabelle_2 <- matrix(c(Summary_Statistics_2_Gold,
                                         Summary_Statistics_2_MSCI_World,
                                         Summary_Statistics_2_SP_500),
                                       ncol=5,  byrow=TRUE)
#3.Defining the Columns and Rows of the Table and exporting it to a tex file for latex
colnames(Summary_Statistics_Tabelle_2) <- c("Mean","SD.","IQR.", 
                                            "Obs.", "Freq.")
rownames(Summary_Statistics_Tabelle_2) <- Indexes
Summary_Statistics_Tabelle_2 <- as.table(Summary_Statistics_Tabelle_2)
Summary_Statistics_Tabelle_2
print(xtable(Summary_Statistics_Tabelle_2, caption = "Daily Returns in Percent", type = "latex"),
      caption.placement = 'bottom',
      size="\\fontsize{7pt}{8pt}",
      file = "/Users/maherp/Desktop/Universität/R-Tools Kurs/R & Latech Dateien/SummaryStatisticsTabelle2_D2.tex")

kable(Summary_Statistics_Tabelle_2, format = "latex", booktabs = TRUE) %>%
  writeLines("/Users/maherp/Desktop/Universität/R-Tools Kurs/R & Latech Dateien/SummaryStatisticsTabelle2.1_D2.tex")


#Rolling Correlation coefficients

#1.Building tables with the excahnge date and the daily returns for each index
for(i in 1:length(Indexes))
{ 
  a <- c(Indexes[i])
  assign(paste0(Indexes[i], "_Date_Returns"), 
         data.frame("Exchange_Date"=DataTabellesList[[i]]$`Exchange_Date`,
                    "Returns" =DataTabellesList[[i]]$`Return`))
}

#2.Naming the columns of the new tables and defining them in a list
colnames(Gold_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[1], "_Returns"))
colnames(MSCI_World_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[2], "_Returns"))
colnames(SP_500_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[3], "_Returns"))
Date_Return <- list(Gold_Date_Returns,MSCI_World_Date_Returns,SP_500_Date_Returns)

#3.Merging the Gold table with the the other two seperatly by the exchange date and defining
#the two tables in a list
for(i in 1:(length(Indexes)-1))
{ 
  a <- Date_Return[1]
  b <- Date_Return[i+1]
  assign(paste0(Indexes[1], "_", Indexes[i+1], "_Date_Returns"), 
         merge(a, b, by.a ="Exchange_Date", by.b = "Exchange_Date",
               all.a = TRUE, 
               all.b = TRUE))
}
Merge_Date_Return <- list(Gold_MSCI_World_Date_Returns,
                          Gold_SP_500_Date_Returns)


#4.Computing rolling Correlations of Gold and the indices over 250 observations
# and defining the columns of the correlation tables
for(i in 1:2)
{ 
  assign(paste0("Correlation_Returns_250_", Indexes[1], "_", Indexes[i+1]), 
         data.frame(Merge_Date_Return[[i]][,1], 
                    roll_cor(Merge_Date_Return[[i]][,2],
                             Merge_Date_Return[[i]][,3], 
                             width = 250, weights = rep(1, 250), center = TRUE,
                             scale = TRUE, min_obs = 250, complete_obs = TRUE,
                             na_restore = FALSE, online = TRUE)))
}
colnames(Correlation_Returns_250_Gold_MSCI_World) <- c("Exchange_Date", "MSCI_World_Rolling_Correlationskoeffizient")
colnames(Correlation_Returns_250_Gold_SP_500) <- c("Exchange_Date", "SP_500_Rolling_Correlationskoeffizient")

#5.Merging the correlation tables and defining the columns as well as making the exchange rate a date variable
Correlation_Returns_250_All <- left_join(Correlation_Returns_250_Gold_MSCI_World, 
                                         Correlation_Returns_250_Gold_SP_500, by = "Exchange_Date")
colnames(Correlation_Returns_250_All) <- c("Exchange_Date","MSCI_World_Rolling_250_Correlationskoeffizient",
                                           "SP_500_Rolling_250_Correlationskoeffizient")
Correlation_Returns_250_All$Exchange_Date <- 
  as.Date(as.character(Correlation_Returns_250_All$Exchange_Date, "%Y-%m-%d"))


#6. Repetition of the computation of the correlation coefficients with 50 observations
for(i in 1:c(length(Indexes)-1))
{ 
  assign(paste0("Correlation_Returns_50_", Indexes[1], "_", Indexes[i+1]), 
         data.frame(Merge_Date_Return[[i]][,1], 
                    roll_cor(Merge_Date_Return[[i]][,2],
                             Merge_Date_Return[[i]][,3], 
                             width = 50, weights = rep(1, 250), center = TRUE,
                             scale = TRUE, min_obs = 50, complete_obs = TRUE,
                             na_restore = FALSE, online = TRUE)))
}


colnames(Correlation_Returns_50_Gold_MSCI_World) <- c("Exchange_Date", "MSCI_World_Rolling_Correlationskoeffizient")
colnames(Correlation_Returns_50_Gold_SP_500) <- c("Exchange_Date", "SP_500_Rolling_Correlationskoeffizient")

Correlation_Returns_50_All <- left_join(Correlation_Returns_50_Gold_MSCI_World,Correlation_Returns_50_Gold_SP_500, by = "Exchange_Date")
colnames(Correlation_Returns_50_All) <- c("Exchange_Date","MSCI_World_Rolling_50_Correlationskoeffizient","SP_500_Rolling_50_Correlationskoeffizient")
Correlation_Returns_50_All$Exchange_Date <- 
  as.Date(as.character(Correlation_Returns_50_All$Exchange_Date, "%Y-%m-%d"))

#7. Merging both correlation data sets as a data frame
Correlation_Returns_250_50_All <- data.frame(merge(Correlation_Returns_250_All, Correlation_Returns_50_All, by = "Exchange_Date"))

#8.Extracting the 20 lowest daily return dates of the MSCI World and the S&P 500
#MSCI World
Lowest_100_MSCI_World <- data.frame(tbl_df(MSCI_World) %>% top_n(-20))
Lowest_100_MSCI_World
Lowest_100_MSCI_World$Exchange_Date <- as.Date(Lowest_100_MSCI_World$Exchange_Date, "%Y-%m-%d")
class(Lowest_100_MSCI_World$Exchange_Date)
class(Lowest_100_MSCI_World)
#S&P 500
Lowest_100_SP_500 <- data.frame(tbl_df(SP_500) %>% top_n(-20))
Lowest_100_SP_500
Lowest_100_SP_500$Exchange_Date <- as.Date(Lowest_100_SP_500$Exchange_Date, "%Y-%m-%d")
class(Lowest_100_SP_500$Exchange_Date)

#9.Plotting the Correlation coefficients for 50 & 250 Observations
#Including vertical lines to indicate the lowest daily return dates of the MSCI World and the S&P 500
Rolling_Correlation_Plot <- ggplot(Correlation_Returns_250_50_All, aes(x = Exchange_Date)) + 
  geom_line(aes(y = MSCI_World_Rolling_250_Correlationskoeffizient), color = "Blue", size=1.2) +
  geom_line(aes(y = SP_500_Rolling_250_Correlationskoeffizient), color = "Red", size=1.2) +
  geom_line(aes(y = MSCI_World_Rolling_50_Correlationskoeffizient), color = "Blue", size=0.1) +
  geom_line(aes(y = SP_500_Rolling_50_Correlationskoeffizient), color = "Red", size=0.1) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[1], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[2], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[3], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[4], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[5], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[6], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[7], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[8], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[9], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[10], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[11], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[12], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[13], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[14], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[15], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[16], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[17], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[18], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[19], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_MSCI_World$Exchange_Date[20], linetype="dotted", color = "blue", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[1], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[2], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[3], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[4], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[5], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[6], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[7], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[8], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[9], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[10], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[11], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[12], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[13], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[14], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[15], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[16], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[17], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[18], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[19], linetype="dotted", color = "red", size=0.51) +
  geom_vline(xintercept = Lowest_100_SP_500$Exchange_Date[20], linetype="dotted", color = "red", size=0.51) +
  labs(x = "Exchange Date", y = "Value of Correlationscoefficient") +
  labs(title="Correlation of Indices with Gold", 
       caption="Blue: MSCI_World, Red: SP_500 (dotted lines represent lowest 20 return dates)") +
  theme_bw() +
  scale_x_date( labels = date_format("%Y-%m-%d")) 
Rolling_Correlation_Plot
#Saving as PDF for latex import
pdf("Rolling_Correlation_Plot.pdf", height=5, width=7)
plot(Rolling_Correlation_Plot)
dev.off()




#Histogramm with a Normal Distribution

#1.Defining a list of the indices and the word histogramm
Indexes_Histogramm <- paste0(Indexes, "_Histogramm")

#Plotting a Histogramm for each indices with 
#The 25% and the 75% quantiles, the mean und and median indicated as vertical lines
#And a normal distribution with the mean and the standard deviation of the indices
for (i in 1:length(DataTabellesList_Return))
{ 
  hist(as.numeric(DataTabellesList_Return[[i]]$`%Return`), density=0, breaks=100, prob=TRUE, 
       xlab=paste0(Indexes[[i]]), ylab="absolute Density", xlim=c(-0.04, 0.04), ylim=c(0, 80), 
       main=paste0("Histogram: Daily Returns of ", Indexes[[i]], "Index"))
  curve(dnorm(x, mean=mean(DataTabellesList_Return[[i]]$`%Return`), 
              sd=sd(DataTabellesList_Return[[i]]$`%Return`)), 
        col="Black", lwd=3, add=TRUE, yaxt="n")
  par(bg = "white")
  legend(0.005,80, legend=c("25% Quantile", "Median", "Mean", "75% Quantile"),
         col=c("red", "blue", "brown", "green"), lty=2:2, cex=0.7, bty = "n")
  abline(v=quantile(DataTabellesList_Return[[i]]$`%Return`)[2], lty="dashed", col=c("red"))
  abline(v=median(DataTabellesList_Return[[i]]$`%Return`), lty="dashed",col=c("blue"))
  abline(v=mean(DataTabellesList_Return[[i]]$`%Return`), lty="dashed",col=c("brown"))
  abline(v=quantile(DataTabellesList_Return[[i]]$`%Return`)[4], lty="dashed",col=c("green"))
}




#Emptying the list in R
rm(list = ls())




#####Appendix#####
#I used 6 Indices with data drom 2010-2020 to do the same analysis
#The correlation plot shows that the results for all indices seem to be similar

library(knitr)
library(kableExtra)
library(tikzDevice)
library(ggplot2)
library(xtable)
library(readxl)
library(data.table)
library(ggplot2)
library(scales)
library(roll)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Gold.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/Gold Spot All Contributors.xlsx", 
                              sheet = 1, range = "A14:N2615", col_names = TRUE)) 

MSCI_World.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/MSCI International World.xlsx",
                                    sheet = 1, range ="A14:O2619", col_names = TRUE))

MSCI_EM.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/MSCI Emerging Markets.xlsx",
                                 sheet = 1, range ="A14:O2619", col_names = TRUE))

EURO_Stoxx.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/EURO STOXX INDEX_USD.xlsx",
                                    sheet = 1, range ="A16:P2580", col_names = TRUE)) #in USD

DB_DAX.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/Deutsche Boerse DAX_USD.xlsx",
                                sheet = 1, range ="A30:P2562", col_names = TRUE)) #in USD

SP_500.xlsx <- setDT(read_excel("/Users/maherp/Desktop/Universität/R-Tools Kurs/DAHALO INPUT 1/S&P 500 Index.xlsx",
                                sheet = 1, range ="A14:O2528", col_names = TRUE))


#Summary statistics Table for 6 indices
DataTabellesList = list(Gold.xlsx, MSCI_World.xlsx, MSCI_EM.xlsx, EURO_Stoxx.xlsx, DB_DAX.xlsx, SP_500.xlsx)
Indexes <- c("Gold","MSCI_World", "MSCI_EM", "SP_500", "DB_DAX", "EURO_Stoxx")

for (i in 1:length(DataTabellesList))
{ 
  assign(paste0(Indexes[[i]], "_Return"), 
         data.table("%Return"=DataTabellesList[[i]]$`%Chg`[!is.na(DataTabellesList[[i]]$`%Chg`)]))
  
}


DataTabellesList_Return = list(Gold_Return, MSCI_World_Return, MSCI_EM_Return, EURO_Stoxx_Return, DB_DAX_Return, SP_500_Return)
for (i in 1:length(DataTabellesList_Return))
{ 
  colnames(DataTabellesList_Return[[i]][1]) <- c("%Returns")
}

for (i in 1:length(DataTabellesList_Return))
{ 
  assign(paste0("Summary_Statistics_", Indexes[[i]]), 
         c(mean(DataTabellesList_Return[[i]]$`%Return`),
           sd(DataTabellesList_Return[[i]]$`%Return`),
           median(DataTabellesList_Return[[i]]$`%Return`),
           IQR(DataTabellesList_Return[[i]]$`%Return`), 
           min(DataTabellesList_Return[[i]]$`%Return`), 
           quantile(DataTabellesList_Return[[i]]$`%Return`)[2], 
           median(DataTabellesList_Return[[i]]$`%Return`), 
           quantile(DataTabellesList_Return[[i]]$`%Return`)[4],
           max(DataTabellesList_Return[[i]]$`%Return`),
           nrow(DataTabellesList_Return[[i]]), 
           max(DataTabellesList_Return[[i]]$`%Return`)-min(DataTabellesList_Return[[i]]$`%Return`),
           "Daily"))
}

Summary_Statistics_Tabelle_D1 <- matrix(c(Summary_Statistics_Gold,
                                       Summary_Statistics_MSCI_World,
                                       Summary_Statistics_MSCI_EM,
                                       Summary_Statistics_SP_500,
                                       Summary_Statistics_DB_DAX,
                                       Summary_Statistics_EURO_Stoxx),
                                     ncol=12,  byrow=TRUE)
colnames(Summary_Statistics_Tabelle_D1) <- c("Mean","Standard Deviation","Median","Inter Quartil Range", 
                                          "Min","quantile_25", "median", "quantile_75",
                                          "Max","Obs", "Range", "Frequency")
rownames(Summary_Statistics_Tabelle_D1) <- Indexes
Summary_Statistics_Tabelle_D1 <- as.table(Summary_Statistics_Tabelle_D1)
Summary_Statistics_Tabelle_D1

#Histogramms for 6 indices
for (i in 1:length(DataTabellesList_Return))
{ 
    hist(DataTabellesList_Return[[i]]$`%Return`, density=0, breaks=100, prob=TRUE, 
         xlab=paste0(Indexes[[i]]), ylab="absolute Density", xlim=c(-0.04, 0.04), ylim=c(0, 80), 
         main="Histogram: Daily Returns of MSCI World Index")
  curve(dnorm(x, mean=mean(DataTabellesList_Return[[i]]$`%Return`), sd=sd(DataTabellesList_Return[[i]]$`%Return`)), 
        col="Black", lwd=3, add=TRUE, yaxt="n")
  legend(0.02,50, legend=c("25% Quantile", "Mean", "75% Quantile"),
         col=c("red", "blue", "green"), lty=2:2, cex=0.7, bty = "n")
  abline(v=quantile(DataTabellesList_Return[[i]]$`%Return`)[2], lty="dashed", col=c("red"))
  abline(v=median(DataTabellesList_Return[[i]]$`%Return`), lty="dashed",col=c("blue"))
  abline(v=quantile(DataTabellesList_Return[[i]]$`%Return`)[4], lty="dashed",col=c("green"))
}


#Rolling correlation for 6 indices
for(i in 1:length(Indexes))
{ 
  a <- c(Indexes[i])
  assign(paste0(Indexes[i], "_Date_Returns"), 
         data.frame("Exchange_Date"=DataTabellesList[[i]]$`Exchange Date`,
                    "Returns" =DataTabellesList[[i]]$`%Chg`))
}

colnames(Gold_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[1], "_Returns"))
colnames(MSCI_World_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[2], "_Returns"))
colnames(MSCI_EM_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[3], "_Returns"))
colnames(SP_500_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[4], "_Returns"))
colnames(DB_DAX_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[5], "_Returns"))
colnames(EURO_Stoxx_Date_Returns) <- c("Exchange_Date" , paste0(Indexes[6], "_Returns"))

Date_Return <- list(Gold_Date_Returns,MSCI_World_Date_Returns,MSCI_EM_Date_Returns,
                    EURO_Stoxx_Date_Returns,DB_DAX_Date_Returns,SP_500_Date_Returns)

for(i in 1:(length(Indexes)-1))
{ 
  a <- Date_Return[1]
  b <- Date_Return[i+1]
  assign(paste0(Indexes[1], "_", Indexes[i+1], "_Date_Returns"), 
         merge(a, b, by.a ="Exchange_Date", by.b = "Exchange_Date",
               all.a = TRUE, 
               all.b = TRUE))
  
}

Merge_Date_Return <- list(Gold_MSCI_World_Date_Returns,Gold_MSCI_EM_Date_Returns,
                          Gold_EURO_Stoxx_Date_Returns,Gold_DB_DAX_Date_Returns,
                          Gold_SP_500_Date_Returns)

for(i in 1:c(length(Indexes)-1))
{ 
  assign(paste0("Correlation_Returns_", Indexes[1], "_", Indexes[i+1]), 
         data.frame(Merge_Date_Return[[i]][,1], 
                    roll_cor(Merge_Date_Return[[i]][,2],
                             Merge_Date_Return[[i]][,3], 
                             width = 250, weights = rep(1, 250), center = TRUE,
                             scale = TRUE, min_obs = 250, complete_obs = TRUE,
                             na_restore = FALSE, online = TRUE)))
}

colnames(Correlation_Returns_Gold_MSCI_World) <- c("Exchange_Date", "MSCI_World_Rolling_Correlationskoeffizient")
colnames(Correlation_Returns_Gold_MSCI_EM) <- c("Exchange_Date", "MSCI_EM_Rolling_Correlationskoeffizient")
colnames(Correlation_Returns_Gold_SP_500) <- c("Exchange_Date", "SP_500_Rolling_Correlationskoeffizient")
colnames(Correlation_Returns_Gold_DB_DAX) <- c("Exchange_Date", "DB_DAX_Rolling_Correlationskoeffizient")
colnames(Correlation_Returns_Gold_EURO_Stoxx) <- c("Exchange_Date", "EURO_Stoxx_Rolling_Correlationskoeffizient")

Correlation_Returns_All <- left_join(Correlation_Returns_Gold_MSCI_World, 
                                     Correlation_Returns_Gold_MSCI_EM, by = "Exchange_Date") %>%
  left_join(.,Correlation_Returns_Gold_SP_500, by = "Exchange_Date") %>%
  left_join(.,Correlation_Returns_Gold_DB_DAX, by = "Exchange_Date") %>%
  left_join(.,Correlation_Returns_Gold_EURO_Stoxx, by = "Exchange_Date")

colnames(Correlation_Returns_All) <- c("Exchange_Date","MSCI_World_Rolling_Correlationskoeffizient",
                                       "MSCI_EM_Rolling_Correlationskoeffizient",
                                       "SP_500_Rolling_Correlationskoeffizient",
                                       "DB_DAX_Rolling_Correlationskoeffizient",
                                       "EURO_Stoxx_Rolling_Correlationskoeffizient"
)
Correlation_Returns_All$Exchange_Date <- 
  as.Date(Correlation_Returns_All$Exchange_Date, "%Y-%m-%d")

Rolling_Correlation_Plot_D1 <- ggplot(Correlation_Returns_All, aes(x = Exchange_Date)) + 
  geom_line(aes(y = MSCI_World_Rolling_Correlationskoeffizient), color = "Pink") +
  geom_line(aes(y = MSCI_EM_Rolling_Correlationskoeffizient), color = "Green") +
  geom_line(aes(y = SP_500_Rolling_Correlationskoeffizient), color = "Red") +
  geom_line(aes(y = DB_DAX_Rolling_Correlationskoeffizient), color = "Black") +
  geom_line(aes(y = EURO_Stoxx_Rolling_Correlationskoeffizient), color = "Blue") +
  labs(x = "Exchange Date", y = "Value of Correlationscoefficient") +
  labs(title="Correlationcoefficients with Gold", 
       caption="Red: SP_500, Green: MSCI_EM, Black: DB_DAX, Blue: EURO_Stoxx, Pink: MSCI_World") +
  theme_bw() +
  scale_x_date( labels = date_format("%Y-%m-%d"))
Rolling_Correlation_Plot_D1

pdf("Rolling_Correlation_Plot_D1.pdf", height=5, width=7)
plot(Rolling_Correlation_Plot_D1)
dev.off()



#Emptying the list in R
rm(list = ls())





