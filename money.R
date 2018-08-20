if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library("ggplot2")
if (!require(igraph)) {
  install.packages("igraph", repos="http://cran.us.r-project.org")
}
library("igraph")
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)


workingDir = '/Users/michaeltauberg/powerlaw/'
setwd(workingDir)

# ------------------------------
# ----------------------
# British Households 
# Source: Wealth and Assets Survey, Office for National Statistics
# https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/adhocs/007203distributionoftotalhouseholdwealthbypercentilepointstotalfinancialwealthnettotalpropertywealthnettotalphysicalwealthandtotalprivatepensionwealthgreatbritainjuly2012tojune2014
# ----------------------
# ------------------------------
csvName = "britain.csv"
data_name = "british_households"
dt = read.csv(csvName)
dt$Total.Household.Wealth..Pounds = as.numeric(as.character(dt$Total.Household.Wealth..Pounds))


households = dt[order(dt$Total.Household.Wealth..Pounds, decreasing=TRUE),]

households$Percentile.points=seq(1,nrow(households)) # get ridd of song titles for easy plotting
p = ggplot(households, aes(x=Percentile.points, y=Total.Household.Wealth..Pounds)) + geom_bar(stat="identity") 
p = p + ggtitle("UK Household wealth (2012-2014)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Top Percentiles") + ylab("Net Wealth") 
ggsave(filename = sprintf("./money/%s_curve.png",data_name) , plot=p, width=15, height=10)


# ------------------------------
# ----------------------
# Billionaires - Forbes data
# ----------------------
# ------------------------------

csvName = "billionaires_2018.csv"
data_name = "billionaires"
dt = read.csv(csvName)
dt$Net.Worth = as.numeric(as.character(dt$Net.Worth))
billionaires = dt

# plot top 20
top_billionaires = billionaires[1:20,]
top_billionaires$Name = factor(top_billionaires$Name, levels = top_billionaires$Name[order(top_billionaires$Net.Worth, decreasing=TRUE)])
p = ggplot(top_billionaires, aes(x=Name, y=Net.Worth)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Forbes Billionaires in the world (2018)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Billionaire") + ylab("Net Worth") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_top20.png",data_name) , plot=p, width=15, height=10)


billionaires$Name=seq(1,nrow(billionaires)) # get ridd of song titles for easy plotting
p = ggplot(billionaires, aes(x=Name, y=Net.Worth)) + geom_bar(stat="identity") 
p = p + ggtitle("All Forbes Billionaires (2018)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Billionaire") + ylab("Net Worth") 
ggsave(filename = sprintf("./money/%s_curve.png",data_name) , plot=p, width=15, height=10)

# ------------------------------
# ----------------------
# SP500 - barcharts.com data
# Downloaded from Barchart.com as of 07-03-2018 02:49am CDT
# ----------------------
# ------------------------------

csvName = "sp-500-index-07-03-2018.csv"
data_name = "sp500"
dt = read.csv(csvName)
dt$Market.Cap = as.numeric(as.character(dt$Market.Cap))
sp500 = dt[order(dt$Market.Cap, decreasing=TRUE),]

# plot top 20
top_sp = sp500[1:20,]
top_sp$Market.Cap = factor(top_sp$Name, levels = top_sp$Name[order(top_sp$Market.Cap, decreasing=TRUE)])
p = ggplot(top_sp, aes(x=Name, y=Market.Cap)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 SP500 companies by Market Cap")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Company") + ylab("Market Cap") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_top20.png",data_name) , plot=p, width=15, height=10)

# plot the full curve
sp500$Name=seq(1,nrow(sp500)) # get ridd of song titles for easy plotting
p = ggplot(sp500, aes(x=Name, y=Market.Cap )) + geom_bar(stat="identity") 
p = p + ggtitle("SP500 companies by Market Cap") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Company") + ylab("Market Cap") 
ggsave(filename = sprintf("./money/%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# ------------------------------
# ----------------------
# Mergers and Acquisitions
# ----------------------
# ------------------------------
# csvName = "MA Statistics - Worldwide, Regions, Industries  Countries.csv"
# data_name = "mergers"
# dt = read.csv(csvName)
# dt$value_of_deal = as.numeric(as.character(dt$value_of_deal))
# mergers = dt[order(dt$value_of_deal, decreasing=TRUE),]
# 
# mergers$target = factor(mergers$target, levels = mergers$target[order(mergers$value_of_deal, decreasing=TRUE)])
# p = ggplot(mergers, aes(x=target, y=value_of_deal)) + geom_bar(stat="identity") 
# p = p + ggtitle("Top 30 largest mergers/aquisitions")
# p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
# p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
# p = p + xlab("Target Company") + ylab("Value of Deal") 
# #p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
# ggsave(filename = sprintf("./money/%s_top30.png",data_name) , plot=p, width=15, height=10)
# 

# ------------------------------
# ----------------------
# IPOs
# ----------------------
# ------------------------------

csvName = "ipos_2017_2018.csv"
data_name = "ipos"
dt = read.csv(csvName)
dt$money_raised = as.numeric(as.character(dt$money_raised))
ipos = dt[order(dt$money_raised, decreasing=TRUE),]

top_ipos = ipos[1:20,]
top_ipos$Company = factor(top_ipos$Company, levels = top_ipos$Company[order(top_ipos$money_raised, decreasing=TRUE)])
p = ggplot(top_ipos, aes(x=Company, y=money_raised)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 IPOs companies by amount of money raised")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Company") + ylab("Money Raised (millions of $)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_top20.png",data_name) , plot=p, width=15, height=10)

ipos$Company=seq(1,nrow(ipos)) 
ipos$Company = factor(ipos$Company, levels = ipos$Company[order(ipos$money_raised, decreasing=TRUE)])
p = ggplot(ipos, aes(x=Company, y=money_raised)) + geom_bar(stat="identity") 
p = p + ggtitle("All IPOs (2017-2018)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab(" Company") + ylab("Money Raised (millions of $)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_all.png",data_name) , plot=p, width=15, height=10)


# ------------------------------
# ----------------------
# World GDP
# ----------------------
# ------------------------------

csvName = "country_gdp.csv"
data_name = "countries_gdp"
dt = read.csv(csvName)
dt$Country = gsub("\xca","",dt$Country)
dt$GDP = as.numeric(as.character(dt$GDP))
gdp = dt[order(dt$GDP, decreasing=TRUE),]

top_gdp = gdp[1:20,]
top_gdp$Country = factor(top_gdp$Country, levels = top_gdp$Country[order(top_gdp$GDP, decreasing=TRUE)])
p = ggplot(top_gdp, aes(x=Country, y=GDP)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Countries by GDP (Millions of US dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Country") + ylab("GDP (Millions of US dollars)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_top20.png",data_name) , plot=p, width=15, height=10)

gdp$Country=seq(1,nrow(gdp)) 
gdp$Country = factor(gdp$Country, levels = gdp$Country[order(gdp$GDP, decreasing=TRUE)])
p = ggplot(gdp, aes(x=Country, y=GDP)) + geom_bar(stat="identity") 
p = p + ggtitle("All Countries by GDP (Millions of US dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Country") + ylab("GDP (Millions of US dollars)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_all.png",data_name) , plot=p, width=15, height=10)



# ------------------------------
# ----------------------
# Cities
# Source: U.S. Bureau of Economic Analysis
# ----------------------
# ------------------------------

csvName = "cities_gdp2.csv"
data_name = "cities_gdp"
dt = read.csv(csvName)
dt$X2016_gdp = as.numeric(as.character(dt$X2016_gdp))
city_gdp = dt[order(dt$X2016_gdp, decreasing=TRUE),]

top_city_gdp = city_gdp[1:20,]
top_city_gdp$city = factor(top_city_gdp$city, levels = top_city_gdp$city[order(top_city_gdp$X2016_gdp, decreasing=TRUE)])
p = ggplot(top_city_gdp, aes(x=city, y=X2016_gdp)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 US Cities by GDP (Millions of US dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("City") + ylab("GDP (Millions of US dollars)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_top20.png",data_name) , plot=p, width=15, height=10)

city_gdp$X2011 = NULL
city_gdp$X2012 = NULL
city_gdp$X2013 = NULL
city_gdp$X2014 = NULL
city_gdp$X2015 = NULL

#city_gdp$city=seq(1,nrow(city_gdp)) 
city_gdp$city = factor(city_gdp$city, levels = city_gdp$city[order(city_gdp$X2016_gdp, decreasing=TRUE)])
p = ggplot(city_gdp, aes(x=X2016.Rank., y=X2016_gdp)) + geom_bar(stat="identity") 
p = p + ggtitle("All Major US Cities by GDP (Millions of US dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("City") + ylab("GDP (Millions of US dollars)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./money/%s_all.png",data_name) , plot=p, width=15, height=10)


# ------------------------------
#  -------
# use igraph to get alpha values
# ----------
# ------------------------------

fit_households = fit_power_law(households$Total.Household.Wealth..Pounds.) 
fit_billionaires = fit_power_law(billionaires$Net.Worth) 
fit_sp500 = fit_power_law(sp500$Market.Cap)
fit_ipos = fit_power_law(ipos$money_raised)
fit_cities = fit_power_law(city_gdp$X2016_gdp)
fit_countries = fit_power_law(gdp$GDP)

# ------------------------------
#  -------
# fit the curves and plot together
# ----------
# ------------------------------
