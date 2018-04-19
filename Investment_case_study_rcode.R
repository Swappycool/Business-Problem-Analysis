
#---------------Investment case study ------------------------

# Load ggplot package 

library(ggplot2)
library(ggthemes)
library(plotly)

## Checkpoint 1: Data Cleaning 1--------------------------------------

# Load both files "rounds" and "companies" in your R console.  

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)

companies <- read.delim("companies.txt", stringsAsFactors = F)

#--------------------------------------------------------------------

## --------------------Check Granularity-----------------------------

#--------------------------------------------------------------------

# Convert all permalinks of both the files into lowercase/uppercase.
# Permalink is a unique id for each company, So calculate unique permalink in rounds2 and companies file.

rounds2$company_permalink <- tolower(rounds2$company_permalink)

no_of_com_permalinks_rounds <- unique(rounds2$company_permalink)

length(no_of_com_permalinks_rounds)

companies$permalink <- tolower(companies$permalink)

no_of_company_permalinks <- unique(companies$permalink)

length(no_of_company_permalinks)

#--------------------------------------------------------------------


# find records that do not match, it should be 0
permalinks_dont_match <- subset(companies, !companies$permalink %in% rounds2$company_permalink)

#--------------------------------------------------------------------

# merging the two data frames companies and rounds2
# Change the column name first in rounds file  

colnames(rounds2)[1] <- "permalink"

# merging the two frames

master_frame <- merge(x = companies, y = rounds2, by = "permalink")

#--------------------------------------------------------------------



#Total number of NA values present in column raised_amount_usd
sum(is.na(master_frame$raised_amount_usd))

# Replacing all NA values in raised amount column to 0

master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)==T] <- 0


#--------------------------------------------------------------------
## Checkpoint 3: Funding Type Analysis-------------------------------

# Average of venture type funding

venture <- subset(master_frame,funding_round_type=="venture")
mean(venture$raised_amount_usd)

#--------------------------------------------------------------------
# Average of angel type funding

angel <- subset(master_frame,funding_round_type=="angel")
mean(angel$raised_amount_usd)

#--------------------------------------------------------------------
# Average of seed type funding

seed <- subset(master_frame,funding_round_type=="seed")
mean(seed$raised_amount_usd)

#--------------------------------------------------------------------  
# Average of private_equity type funding

private_equity <- subset(master_frame,funding_round_type=="private_equity")
mean(private_equity$raised_amount_usd)

#--------------------------------------------------------------------
##Checkpoint 4: Country Analysis-------------------------------------

# First check the structure of country_code variable
str(master_frame$country_code)


highest_funding_countries <- aggregate(raised_amount_usd~country_code, master_frame, sum)

# sorting the above frame

highest_funding_countries_sorted <- highest_funding_countries[order(highest_funding_countries$raised_amount_usd, decreasing = T), ]

# extracting top 9 countries

top9 <- head(highest_funding_countries_sorted, 9)

# Check the top 3 english speaking countries from top9 dataframe
View(top9)

#-------------------------------------------------------------------- 
### Checkpoint 5: Sector Analysis1-----------------------------------

# Extracting word before "|" for each sector in the new frame

# creating a vector with primary sector names
primary_vector <- gsub("\\|.*", "", master_frame$category_list)

# replacing the category_list column of master_frame with new vector
master_frame$category_list <- primary_vector

# Use mapping file "mapping_file.csv" to map priority sector to one of the 8 main sectors

# import mapping file
mapping_file <- read.csv("mapping_file.csv", header = T, stringsAsFactors = F)


# merging sector_analysis_frame with mapping_file

sector_analysis_merged <- merge(x = master_frame, y = mapping_file, by = "category_list", all.x = T)



# Replace Blank cells with mode i.e "Social, Finance, Analytics, Advertising"
sector_analysis_merged$main_sector<- as.factor(sector_analysis_merged$main_sector)
summary(sector_analysis_merged$main_sector)

# Check the levels of main_sector column

levels(sector_analysis_merged$main_sector)

# 2nd level of main_sector is blank so, replace it with mode of this column i.e "Social, Finance, Analytics, Advertising"
levels(sector_analysis_merged$main_sector)[2] <-"Social, Finance, Analytics, Advertising"

# Also, replace NA values of this column with mode i.e "Social, Finance, Analytics, Advertising"
sector_analysis_merged[ which(is.na(sector_analysis_merged$main_sector)),]$main_sector<- "Social, Finance, Analytics, Advertising" 
#-------------------------------------------------------------------- 
##Checkpoint 6: Sector Analysis 2------------------------------------


# ------US, UK and IND analysis (venture type; investments between 5 - 15 million USD)-----

# So the D1, D2 and D3 are as follows: 
# D1 <- USA
# D2 <- GBR
# D3 <- IND


# The total number (or count) of investments for each main sector in a separate column
# USA Count
USA <- subset(sector_analysis_merged , country_code == "USA" & funding_round_type == "venture" & raised_amount_usd > 5e+06 & raised_amount_usd < 15e+06 )

USA_count<-table(USA$main_sector)

USA_count<-data.frame(USA_count)

colnames(USA_count)<-c("main_sector","USA_Freq")

USA<- merge(x = USA, y = USA_count, by = "main_sector", all.x = T)

# GBR Count

GBR <- subset(sector_analysis_merged , country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd > 5e+06 & raised_amount_usd < 15e+06 )

GBR_count<-table(GBR$main_sector)

GBR_count<-data.frame(GBR_count)

colnames(GBR_count)<-c("main_sector","GBR_Freq")

GBR<- merge(x = GBR, y = GBR_count, by = "main_sector", all.x = T) 


# IND Count 

IND <- subset(sector_analysis_merged , country_code == "IND" & funding_round_type == "venture" & raised_amount_usd > 5e+06 & raised_amount_usd < 15e+06 )

IND_count<-table(IND$main_sector)

IND_count<-data.frame(IND_count)

colnames(IND_count)<-c("main_sector","IND_Freq")

IND<- merge(x = IND, y = IND_count, by = "main_sector", all.x = T) 


# The total amount invested in each main sector in a separate column

# For USA 

USA_Total_investment <-aggregate(raised_amount_usd~main_sector,USA,FUN=sum)

colnames(USA_Total_investment)[2]<- "Total_Investment"

USA<- merge(x = USA, y = USA_Total_investment, by = "main_sector", all.x = T)

# Total investment in USA 
sum(USA_Total_investment$Total_Investment)

# For GBR

GBR_Total_investment <-aggregate(raised_amount_usd ~main_sector,GBR,FUN=sum)

colnames(GBR_Total_investment)[2]<- "Total_Investment"

GBR<- merge(x = GBR, y = GBR_Total_investment, by = "main_sector", all.x = T)

# Total investment in GBR

sum(GBR_Total_investment$Total_Investment)


# For IND

IND_Total_investment <-aggregate(raised_amount_usd~main_sector,IND,FUN=sum)

colnames(IND_Total_investment)[2]<- "Total_Investment"

IND<- merge(x = IND, y = IND_Total_investment, by = "main_sector", all.x = T)

# Total investment in IND

sum(IND_Total_investment$Total_Investment)

#-------------------------------------------------------------------- 

#Checkpoint 7: Plots------------------------------------------------


#ggplot2 Graphs

# Pie Chart 

# Create a new dataframe containing venture, seed and private equity funding type 

Investment_pie <-subset(sector_analysis_merged, funding_round_type =="venture"|funding_round_type =="seed"|funding_round_type =="private_equity")

# Aggregate the total investment in these 3 funding type

Aggregate_pie <-aggregate(raised_amount_usd~funding_round_type,Investment_pie ,FUN=sum)
Aggregate_pie$raised_amount_usd<-Aggregate_pie$raised_amount_usd/(10^9)
Aggregate_pie$raised_amount_usd<-round(Aggregate_pie$raised_amount_usd, digits=2)

# Now, Make the pie chart

Pie <-ggplot(Aggregate_pie, aes(x = factor(1),y= raised_amount_usd, fill = factor(funding_round_type))) +geom_bar(width =1,stat = "identity")

Pie + coord_polar(theta = "y")

plot_ly(data =Aggregate_pie, labels = ~funding_round_type, values = ~raised_amount_usd,type = "pie",
          textposition = 'inside',
          textinfo = 'label+value',
          hoverinfo= 'text',
          text = ~paste('$', raised_amount_usd, 'billions'),
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1))) %>%
      
          layout(title="Pie Chart for funding type distribution",
                          height = 700,
                          width = 800)

# 2nd Bar chart 

ggplot(top9,aes(x=reorder(country_code,-raised_amount_usd),y=raised_amount_usd))+geom_bar(stat="identity", color = "red",fill = "grey")

# 3rd chart : This should show the number of investments in the top 3 sectors of the top 3 countries on one chart (for the chosen investment type i.e venture).

ggplot(subset(sector_analysis_merged,country_code=="USA"|country_code=="GBR"|country_code=="IND"|country_code=="Others"),aes(x= factor(main_sector)))+geom_bar(color = "red",fill = "blue")+facet_wrap(~country_code)+xlab("Main Sector")+theme(axis.text.x = element_text(colour = "black", angle=90))
ggplot(subset(sector_analysis_merged,country_code=="USA"|country_code=="GBR"|country_code=="IND"|country_code=="Others"),aes(x= factor(country_code), fill = factor(main_sector)))+geom_bar(position = "dodge")+xlab("Countries")


#------------------------------------------------------------------------------------------The End