library(RCurl)
library(XML)
library(ggplot2)
library(sqldf)
library(dplyr)
library(mosaic)
library(xlsx)

#Reading html file and parsing it to convert it into data.frame
htm <-
  htmlParse(
    "C:/Users/Mayank/Desktop/myank_project/The World's Most Valuable Brands List.html"
  )
class(htm)
tables_1 <- readHTMLTable(htm)
class(tables_1)
str(tables_1, max.level = 1)
tables_2 = tables_1[[18]]
head(tables_3)
names(tables_2)
tables_3 <- tables_2[, c(-1)]
str(tables_3)

#convert all the variables to char to remove unnecesary symbols
tables_3$Rank <- as.character(tables_3$Rank)
tables_3$Brand <- as.character(tables_3$Brand)
tables_3$`Brand Value` <- as.character(tables_3$`Brand Value`)
tables_3$`1-Yr Value Change` <-
  as.character(tables_3$`1-Yr Value Change`)
tables_3$`Brand Revenue` <- as.character(tables_3$`Brand Revenue`)
tables_3$`Company Advertising` <-
  as.character(tables_3$`Company Advertising`)
tables_3$Industry <- as.character(tables_3$Industry)



#omitting NA's
tables_4<-na.omit(tables_3)

#deleting missing values
tables_4<-tables_4[tables_4$`Company Advertising`!="-",]



#string manipulation for brand value
tables_4$`Brand Value_1` = as.character(gsub("\\$", "", tables_4$`Brand Value`))
temp <- strsplit(tables_4$`Brand Value_1`, split = " ")
tables_4$br_val <- sapply(temp, function(i)
  as.numeric(i[[1]]))
tables_4$br_lv <- sapply(temp, function(i)
  as.character(i[[2]]))

#converting from millions to billions
tables_4$br_val[tables_4$br_lv == "M"] <-
  tables_4$br_val[tables_4$br_lv == "M"] / 1000
tables_4$br_lv <- "B"
tables_4$`Brand Value` <- tables_4$br_val
tables_4 <- tables_4[, c(-8, -9, -10)]






#string manipulation for brand revenue
tables_4$`Brand Revenue_1` = as.character(gsub("\\$", "", tables_4$`Brand Revenue`))
temp1 <- strsplit(tables_4$`Brand Revenue_1`  , split = " ")
tables_4$br_rev <- sapply(temp1, function(i)
  as.numeric(i[[1]]))
tables_4$br_metric <- sapply(temp1, function(i)
  as.character(i[[2]]))

#converting from millions to billions
tables_4$br_rev[tables_4$br_metric == "M"] <-
  tables_4$br_rev[tables_4$br_metric == "M"] / 1000
tables_4$br_metric <- "B"
tables_4$`Brand Revenue` <- tables_4$br_rev
tables_4 <- tables_4[, c(-8, -9, -10)]


#string manipulation for company advertising
tables_4$`Company Advertising_1` = as.character(gsub("\\$", "", tables_4$`Company Advertising`))
temp2 <- strsplit(tables_4$`Company Advertising_1`, split = " ")
tables_4$com_adv <- sapply(temp2, function(i)
  as.numeric(i[[1]]))
tables_4$com_adv_metric <- sapply(temp2, function(i)
  as.character(i[[2]]))


#converting from millions to billions
tables_4$com_adv[tables_4$com_adv_metric == "M"] <-
  tables_4$com_adv[tables_4$com_adv_metric == "M"] / 1000
tables_4$com_adv_metric <- "B"
tables_4$`Company Advertising` <- tables_4$com_adv
tables_4 <- tables_4[, c(-8, -9, -10)]

#removing #from rank variable
tables_4$Rank<-as.character(gsub("\\#","",tables_4$Rank))


#Converting data types for visualization
tables_4$Industry<-as.character(tables_4$Industry)
tables_4$Brand<-as.character(tables_4$Brand)
tables_4$`Brand Value`<-as.numeric(tables_4$`Brand Value`)
tables_4$`Brand Revenue`<-as.integer(tables_4$`Brand Revenue`)

#renaming variables
names(tables_4)[3]<-"Brand_Value"
names(tables_4)[5]<-"Brand_Revenue"
names(tables_4)[6]<-"Company_Advertising"

#exporting file
library(xlsx)
write.xlsx(tables_4,"C:/Users/Mayank/Desktop/Tableau_project/brands.xlsx",row.names = FALSE)


tables_4<-tables_4%>%filter(
  Industry == "Technology" |
    Industry == "Luxury" |
    Industry == "Automotive" |
    Industry == "Financial Services" 
)

#Data Visualizations
#Technology
technology<-tables_4%>%filter(Industry == "Technology")

#plot aesthetics
tech<-ggplot(technology,aes(x = Company_Advertising,y = Brand_Revenue,color= Brand))

#geometry+x/y axis & main title + geometry text settings
tec<-tech+geom_point(aes(col=Brand,size=Brand_Value))+
  labs(title="Technology",x="Company Advertising in Billions of $",y="Brand_Revenue")+
  geom_text(aes(label=Brand),alpha=0.9,nudge_x = 0.21,nudge_y = 0.15)

#legends settings
te<-tec+scale_color_discrete(name="Brand",guide=FALSE)+
  scale_size_continuous(name="Brand Value in $ (Billions)",
                        breaks = c(30,60,100),
                        labels = c(30,60,100))

#theme settings for plot and panel
tech_graph<-te+theme(panel.background = element_rect(fill = "white",color = "grey50" ),
                     panel.grid.major = element_line(color = "grey",size=0.05),
                     panel.grid.minor = element_line(colour = "grey",size=0.05),
                     plot.title = element_text(size = 15,color = "black",face="bold"))
#x/y axis scale settings
tech_graph+scale_x_continuous(breaks = seq(0,4,1))+
  scale_y_continuous(breaks = seq(0,200,50))











#Luxury
luxury<-tables_4%>%filter(Industry == "Luxury")

#plot aesthetics
lux<-ggplot(luxury,aes(x = Company_Advertising,y = Brand_Revenue,color= Brand))

#geometry+x/y axis & main title + geometry text settings
lu<-lux+geom_point(aes(col=Brand,size=Brand_Value))+
  labs(title="Luxury",x="Company Advertising in Billions of $",y="Brand_Revenue")+
  geom_text(aes(label=Brand),alpha=0.9,nudge_x = 0.21,nudge_y = 0.15)

#legends settings
l<-lu+scale_color_discrete(name="Brand",guide=FALSE)+
  scale_size_continuous(name="Brand Value in $ (Billions)",
                        breaks = c(10.0,28.1),
                        labels = c(10.0,28.1))

#theme settings for plot and panel
lux_graph<-l+theme(panel.background = element_rect(fill = "white",color = "grey50" ),
                   panel.grid.major = element_line(color = "grey",size=0.05),
                   panel.grid.minor = element_line(colour = "grey",size=0.05),
                   plot.title = element_text(size = 15,color = "black",face="bold"))
#x/y axis scale settings
lux_graph+scale_x_continuous(breaks = seq(0,8,0.1))+
  scale_y_continuous(breaks = seq(0,12,2))






#Automotive
automotive<-tables_4%>%filter(Industry == "Automotive")

#plot aesthetics
auto<-ggplot(automotive,aes(x = Company_Advertising,y = Brand_Revenue,color= Brand))

#geometry+x/y axis & main title + geometry text settings
aut<-auto+geom_point(aes(col=Brand,size=Brand_Value))+
  labs(title="Automotive",x="Company Advertising in Billions of $",y="Brand_Revenue")+
  geom_text(aes(label=Brand),alpha=0.9,nudge_x = 0.21,nudge_y = 0.15)

#legends settings
au<-aut+scale_color_discrete(name="Brand",guide=FALSE)+
  scale_size_continuous(name="Brand Value in $ (Billions)",
                        breaks = c(6.2,20.0,37.8),
                        labels = c(6.2,20.0,37.8))

#theme settings for plot and panel
auto_graph<-au+theme(panel.background = element_rect(fill = "white",color = "grey50" ),
                     panel.grid.major = element_line(color = "grey",size=0.05),
                     panel.grid.minor = element_line(colour = "grey",size=0.05),
                     plot.title = element_text(size = 15,color = "black",face="bold"))
#x/y axis scale settings
auto_graph+scale_x_continuous(breaks = seq(0.8,6,0.1))+
  scale_y_continuous(breaks = seq(0,180,10))





#Financial Services
finance<-tables_4%>%filter(Industry == "Financial Services")

#plot aesthetics
fin<-ggplot(finance,aes(x = Company_Advertising,y = Brand_Revenue,color= Brand))

#geometry+x/y axis & main title + geometry text settings
fi<-fin+geom_point(aes(col=Brand,size=Brand_Value))+
  labs(title="Finance",x="Company Advertising in Billions of $",y="Brand_Revenue")+
  geom_text(aes(label=Brand),alpha=0.9,nudge_x = 0.21,nudge_y = 0.15)

#legends settings
f<-fi+scale_color_discrete(name="Brand",guide=FALSE)+
  scale_size_continuous(name="Brand Value in $ (Billions)",
                        breaks = c(7.0,12.0,23.4),
                        labels = c(7.0,12.0,23.4))

#theme settings for plot and panel
finance_graph<-f+theme(panel.background = element_rect(fill = "white",color = "grey50" ),
                       panel.grid.major = element_line(color = "grey",size=0.05),
                       panel.grid.minor = element_line(colour = "grey",size=0.05),
                       plot.title = element_text(size = 15,color = "black",face="bold"))
#x/y axis scale settings
finance_graph+scale_x_continuous(breaks = seq(0,6,0.1))+
  scale_y_continuous(breaks = seq(0,100,10))