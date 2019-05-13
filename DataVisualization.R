
# From a CRISP-DM Process perspective - this involves more "DATA UNDERSTANDING" stage
# Data Visualization in R
# Meant for individuals to learn basics of data visualization using certain techniques, approaches, packages 
# DV0151EN

library(ggplot2)
mtcars <- as.data.frame(mtcars)
str(mtcars)
dim(mtcars)

# 1. Bar Chart ----
# No of occurrences of each cylinder value
qplot(mtcars$cyl,
      geom = "bar",fill=I("blue"),
      xlab = "Cylinders",
      ylab="No. of Vehicles",
      main="Cylinders in mtcars")

# 2. Histogram ----
qplot(mtcars$hp,
      geom="histogram",
      binwidth = 25,
      color=I("black"),
      xlim=c(50,350),
      xlab="Horsepower",
      ylab="No of cars",
      alpha=I(0),
      main="Histogram")

# 3. Pie Chart ----

# first create a stacked bar plot
barp <- ggplot(mtcars,
               aes(x=1,y=sort(mtcars$carb),fill=sort(mtcars$carb))) + geom_bar(stat = "identity")
print(barp)
# Then transform stacked bar plot to pie chart
barp <- barp + coord_polar(theta = "y")
print(barp)
# Then remove labels
barp <- barp + theme(
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank()
) + labs(y="Carburetors")
print(barp)

# 4. Scatter plots ----
qplot(mpg,wt,data = mtcars) # Basic scatter plot
# can use scatterplot3d(), scatterplot(), plot() also in addition to qplot() for scatter plots
ggplot(mtcars,aes(x=mpg,y=wt))+geom_point(shape=1) #more customization using ggplot
ggplot(mtcars,aes(x=mpg,y=wt))+geom_point(shape=19) #changing shape of dots in the plot
mtcars$cylFactor <- factor(mtcars$cyl) #3rd variable as a shape
ggplot(mtcars,aes(x=mpg,y=wt,shape=cylFactor))+geom_point() # 4cylinder, 6cylinder and 8cylinder cars are represented in different shapes
ggplot(mtcars,aes(x=mpg,y=wt))+geom_point(shape=19, colour="blue") #Add color option

# 3rd variable as a color, and then compare between cyl and cylFactor
ggplot(mtcars,aes(x=mpg,y=wt,colour=cylFactor))+geom_point(shape=19) 
ggplot(mtcars,aes(x=mpg,y=wt,colour=cyl))+geom_point(shape=19)

ggplot(mtcars,aes(x=mpg,y=wt,color=cyl))+geom_point(shape=19) 

# Adding Labels to plot, title etc
ggplot(mtcars,aes(x=mpg,y=wt,colour=cylFactor))+
  geom_point(shape=19)+
  xlab("Miles per Gallon")+
  ylab("Weight")+
  labs(colour="Cylinders")+
  ggtitle("Scatterplot using mtcars dataset")


# 5. Line plots and Regression ----
# Historical data of 4 European stock market indices
EuStockDF <- as.data.frame(EuStockMarkets)
head(EuStockDF)
ggplot(EuStockDF,aes(x=c(1:nrow(EuStockDF)),y=DAX))+ geom_line()
ggplot(EuStockDF,aes(x=c(1:nrow(EuStockDF)),y=DAX))+ 
  geom_line(size=1.5,colour="light blue")+
  labs(x="Time",y="Stocks")
# Drawing for multiple stocks
all_stocks <- ggplot()+
  geom_line(data = EuStockDF,aes(x=c(1:nrow(EuStockDF)),y=DAX),size=1,colour="light blue")+
  geom_line(data = EuStockDF,aes(x=c(1:nrow(EuStockDF)),y=SMI),size=1,colour="red")+
  geom_line(data = EuStockDF,aes(x=c(1:nrow(EuStockDF)),y=CAC),size=1,colour="purple")+
  geom_line(data = EuStockDF,aes(x=c(1:nrow(EuStockDF)),y=FTSE),size=1,colour="green")+
  labs(x="Time",y="Stocks")
print(all_stocks)
legend_stocks <- all_stocks +
  xlab("Days")+
  ylab("Price")+
  ggtitle("Eu Stocks")
print(legend_stocks)

# Basic Linear regression plot
ggplot(mtcars,aes(x=mpg,y=wt))+
  geom_point(shape=19)+
  geom_smooth(method = "lm",se=FALSE, color="red")

#Set confidence interval of a Linear reg model by setting se=TRUE
ggplot(mtcars,aes(x=mpg,y=wt))+
  geom_point(shape=19)+
  geom_smooth(method = "lm",se=TRUE, color="red")+
  xlab("Miles per Gallon")+
  ylab("Weight")+
  labs(colour="Cylinders")+
  ggtitle("Linear Regression using mtcars dataset")

#Gaussian Regression
ggplot(mtcars,aes(x=mpg,y=wt,color=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = "auto",se=TRUE, color="red")+
  xlab("Miles per Gallon")+
  ylab("Weight")+
  labs(colour="Cylinders")+
  ggtitle("Gaussian Regression using mtcars dataset")

# 6. Word Clouds ----
dir.create("/Users/kamal/Desktop/wordcloud")
download.file("https://ibm.box.com/shared/static/cmid70rpa7xe4ocitcga1bve7r0kqnia.txt",
              destfile = "/Users/kamal/Desktop/wordcloud/churchill_speeches.txt", quiet = TRUE )
#file.path(...,fsep = .Platform$file.sep)
library(tm)
library(wordcloud)
dirpath <- "/Users/kamal/Desktop/wordcloud"
speech <- Corpus(DirSource(dirpath))
inspect(speech)

# data cleaning efforts
speech <- tm_map(speech, content_transformer(tolower))
speech <- tm_map(speech,removeNumbers)
speech <- tm_map(speech,removeWords,stopwords("english"))
speech <- tm_map(speech,removeWords,c("floccinaucinihilipification","squirrelled"))
speech <- tm_map(speech,removePunctuation)
speech <- tm_map(speech,stripWhitespace)

# create a term document matrix
dtm <- TermDocumentMatrix(speech)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word=names(v),freq=v)
head(d,10)

# Simple word cloud
wordcloud(words = d$word,freq = d$freq)

wordcloud(words = d$word,freq = d$freq,
          min.freq = 1,
          max.words = 250,
          colors = brewer.pal(8,"Dark2"),
          random.order = FALSE)

# 7. Radar Charts
# please ensure these libraries are available - ggplot2, ggradar, dplyr, scales
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)
library(ggplot2)
library(scales)
library(dplyr)
library(ggradar)
library(IRkernel)

# Radar Charts are a way to display multivariate data within one plot
# Could be used to compare different car types, or by a company to visualize
# and analyze several marketing and investment strategies
mtcars %>%
  add_rownames(var="group") %>%
  mutate_each(list(rescale),-group) %>%
  head(3) %>%
  select(1:10) -> mtcars_radar
options(warn = -1)
ggradar(mtcars_radar)
IRkernel::set_plot_options(width=950,height=600,units='px')
ggradar(mtcars_radar)

library(ggradar)

suppressPackageStartupMessages(library(dplyr))
library(scales)
library(tibble)

mtcars %>%
  rownames_to_column( var = "group" ) %>%
  mutate_at(vars(-group),list(rescale)) %>%
  tail(4) %>% select(1:10) -> mtcars_radar

ggradar(mtcars_radar) 

# 8. Waffle Charts ----
# Visualize data in relation to a whole

install.packages("waffle")
library(waffle)
# sample data with a named vector with household data
expenses <- c(`Health($43,212)` = 43212,
              `Education($113,412)` = 113412,
              `Transportation($20,231)` = 20231,
              `Entertainment($28,145)` = 28145)
waffle(expenses/1235, rows=5, size=0.3,
       colors=c("#c7d4b6","#a3aabd","#a0d0de","#97b5cf"),
       title="Imaginery Household Expenses Each Year",
       xlab="1 square = $934")

# Lot of different approaches, techniques can be followed.
# It is important to practise it and leverage some of the libraries / packages already in R
# This will help while performing Data Analysis / Data Wrangling / Exploratory Data Analysis etc.

# Please refer to http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html for Radar plots



