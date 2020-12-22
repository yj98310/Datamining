#시각화를 위한 R 코드작성
install.packages("ggplot2")
library('ggplot2')

setwd("C:/data/visualization")

#Data loading
data.housing <-read.csv("BostonHousing.csv") #데이터 load
summary(data.housing) #데이터 요약
names(data.housing)[14] <- c("CAT.MEDV") #CAT..medv를 CAT.MEDV로 변경
data.housing$CAT.MEDV <- as.factor(data.housing$CAT.MEDV)

######Data Visualization

#1. simple bar chart
count <- table(data.housing$CHAS) #count라는 새로운 변수에 값 저장
barplot(count)
#barplog using ggplot2
count.df <- as.data.frame(count) #table 형태의 count를 df 형태로 바꿈
total <- 0
for (i in 1:length(count.df[,2])) total <- total + count.df[i,2]
#total <- nrow(data.housing) 도 같은 값

new_col <- data.frame(ratio=c(count.df[,2]/total)) #비율 정의
count.df <- cbind(count.df, new_col)

p <- ggplot(data=count.df, aes(x=var1, y=ratio)) # x축 y축 값 정의
p + geom_bar(stat="identity")+xlab('CHAT')+ylab('% of CAT.MEDV')

#Scatter plot
plot(data.housing$LSTAT, data.housing$MEDV, xlab="MEDV", ylab="LSTAT")
ggplot(data=data.housing, aes(x=LSTAT, y=MEDV, color=CAT.MEDV))+geom_point()

#Histogram, Boxplot, Heatmap, Matrixplot

#Amtrack passenger data aggregation
am_data <- read.csv("Amtrak.csv")
names(am_data)[1] <- "date" # 첫 번째 col의 이름을 date로 바꿈
names(am_data)[2] <- "passenger"

x <- NULL
x <- gsub("/", " ", am_data$date) #/를 공백으로 변경
x <- strsplit(x, " ") # 공백으로 쪼개기
x <- t(matrix(unlist(x),3,159))

x_frame <- data.frame(x)
names(x_frame)[1] <- "day"
names(x_frame)[2] <- "month"
names(x_frame)[3] <- "year"

install.packages("sqldf")
library('sqldf')
#Merge data
amtrak_data <- cbind(am_data, x_frame )
am_month_traffic <- sqldf("select month, passenger from amtrak_data group by month")

g <- ggplot(data=am_month_traffic, aes(x=am_month_traffic$month, y=am_month_traffic$passenger))
g+geom_line()
