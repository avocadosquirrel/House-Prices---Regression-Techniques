# HOUSE PRICES - ADVANCED REGRESSION TECHNIQUES (KAGGLE/DSS)


library(DataExplorer)
library(tidyverse)
library(ggmap)
#library(ggplot2) #not required, since ggplot2 is included in tidyverse!

version

getwd()

#create an output directory
output_dir=paste(getwd(),"/", sep="")
ifelse(!dir.exists(output_dir), dir.create(output_dir),FALSE)

houses <- read.csv('train.csv', sep=",", header=TRUE)
print(houses)

houses_1422 <- read.csv('houses_1422.csv', sep=",", header=TRUE)
print(houses_1422)

houses_cleaned <- read.csv('houses_cleaned.csv', sep=",", header=TRUE)
print(houses_cleaned)

str(houses)

summary(houses)

introduce(houses)

#create_report(houses)                     # not required -> use y-version only :) (redundant)
create_report(houses, y='SalePrice') 

# really a lot of vars have 0 NAN !!! :)
colSums(sapply(houses, is.na))

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Missing(houses[,colSums(is.na(houses)) > 0])   #, with = FALSE
plot_Missing(houses_1422[,colSums(is.na(houses)) > 0])   #, with = FALSE


ggplot(data=houses) +
  geom_point(mapping=aes(x=LotArea, y=SalePrice, colour=OverallQual, alpha=0.5)) +
  scale_color_gradient(low="orange", high="magenta") +
  scale_x_log10() 


# !!! need to remove those observations where price just corresponds to an empty parcel !!! how to detect?? :)
ggplot(data=houses, mapping=aes(x=LotArea, y=SalePrice, colour=OverallQual)) +
  geom_smooth() +
  geom_point(alpha=0.5) +
  scale_x_log10() 

# removing the 38 lines where Bsmt vars are missing did not improve a lot! 
ggplot(data=houses_1422, mapping=aes(x=LotArea, y=SalePrice, colour=OverallQual)) +
  geom_smooth() +
  geom_point(alpha=0.5) +
  scale_x_log10() 

# the cheepest houses are terribly cheep?!
ggplot(data=houses_1422, mapping=aes(x=OverallQual, y=SalePrice, colour=LotArea)) +
  geom_smooth() +
  geom_point(alpha=0.3) +
  scale_color_gradient(low="orange", high="magenta") #+
#  scale_x_log10() 

# the cheepest houses are terribly cheep?!
ggplot(data=houses_1422, mapping=aes(x=PoolArea, y=GrLivArea, colour=OverallQual)) +
  geom_smooth() +
  geom_point(alpha=0.4) #+
#  scale_x_log10() 

# jitter is not added like this! :( ... why isn't the trendline added, either?!?
ggplot(data=houses_1422, mapping=aes(x=GarageCars, y=GarageArea, colour=LotArea, jitter(GarageCars))) +
  geom_smooth() +
  geom_point(alpha=0.3) +
  scale_color_gradient(low="orange", high="magenta") #+
#  scale_x_log10() 

ggplot(data=houses_1422, mapping=aes(x=GarageArea, y=SalePrice, colour=GarageCars)) +
  geom_smooth() +
  scale_color_gradient(low="orange", high="magenta") +
  theme(panel.background = element_blank(), legend.key = element_blank()) +
  geom_point(alpha=0.5) +
  scale_x_log10() 

ggplot(data=houses_1422, mapping=aes(x=GarageCars, y=GarageArea, colour=SalePrice)) +
  geom_smooth() +
  geom_point(alpha=0.5) +
  scale_color_gradient(low="orange", high="magenta") +
  scale_x_log10() 



# houses_cleaned

colSums(sapply(houses_cleaned, is.na))

ggplot(data=houses_cleaned, mapping=aes(x=GrLivArea, y=SalePrice, colour=Fireplaces_ordinal)) +
  geom_smooth() +
  scale_color_gradient(low="orange", high="magenta") +
  theme(panel.background = element_blank(), legend.key = element_blank()) +
  geom_point(alpha=0.5) +
  scale_x_log10() 
