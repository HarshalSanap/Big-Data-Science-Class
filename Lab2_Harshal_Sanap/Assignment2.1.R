#### load required libraries
library(ggplot2)
### Read csv data into R dataframe
disease_data <- read.csv("Trended_3-4-50_Chronic_Disease.csv")

### list all the field names (Variables in the dataset)
names(disease_data)
summ
### Subset data by specific column
## subset only the year 2010
data_2010 <- cancer_data[cancer_data$YEAR == 2010,]
## subset by other fields (ex. Geography and RegionName)
# data_LaMesa <- cancer_data[cancer_data$Geography == 'La Mesa',]
# data_EAST <- cancer_data[cancer_data$RegionName == 'EAST',]
### Generate some statistics
## aggregate region by names, then calculate the mean for three rates
mean_year <- aggregate(disease_data[, c("Stroke.Deaths","CHD.Deaths","Diabetes.Deaths","COPD.Deaths","Asthma.Deaths","Cancer.Deaths")], by=list(disease_data$Year), FUN=sum, na.rm=TRUE)
print(mean_year)
## remove the row UNKNOWN
mean_region <- mean_region[mean_region$RegionName != "UNKNOWN",]

mean_year1 <- mean_year[mean_year$Group.1<=2005,]
### Make a visual plot ! aes: aesthetic mappings, geom_bar: rectangle bars
ggplot(mean_year1, aes(mean_year1$Group.1,(mean_year1$Stroke.Deaths))) + geom_bar(stat="identity",aes(fill= factor(mean_year1$Group.1)), width=0.5) +
  ggtitle("Lung Cancer in San Deigo by Regions (Mean of Age Adjusted Rate)")+
  ylab("Mean of Age Adjusted Rate") +
  xlab("Regions of San Deigo") +
  theme(legend.position="right") +
  theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1))+
  list()

library(reshape2)
dfm <- melt(mean_year1[,c('Group.1','Stroke.Deaths','Diabetes.Deaths','COPD.Deaths')],id.vars = 1)
ggplot(dfm,aes(x = Group.1,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + 
  scale_y_log10()

## show the plot & save it to file
ggsave("myFigure.png", width = 12, height = 6)