
#' ---
#' title: "R code"
#' author: "Jei"
#' date: "5/27/22"
#' ---

library(ggplot2)
data <- read.csv("data/Call.csv")
data2 <- read.csv("data/NonCall.csv")

# Model 1 (without control)
logistic <- glm(DV_incorrect ~ home_receive, family = "binomial", data = data)
summary(logistic)

# Model 2 (with control)
logistic_control <- glm(DV_incorrect ~ home_receive+star_receive+US_receive, family = "binomial", 
                        data = data)
summary(logistic_control)

# Model 2 (without control)
logistic2 <- glm(DV_incorrect ~ home_commit, family = "binomial", data = data2)
summary(logistic2)

# Model 2 (with control)
logistic2_control <- glm(DV_incorrect ~ home_commit+star_commit+US_commit+time.1, family = "binomial", data = data2)
summary(logistic2)


agg <- aggregate(DV_incorrect~home_receive, data = data, mean)
agg$home_receive2 <- factor(agg$home_receive,
                            labels = c('Away','Home'))

agg2 <- aggregate(DV_incorrect~home_commit, data = data2, mean)
agg2$home_commit2 <- factor(agg$home_receive,
                            labels = c('Away','Home'))

# Figure 1: Call vs Home
ggplot(agg, aes(x = home_receive2, y = DV_incorrect, fill = home_receive2)) +
  geom_col(position = 'dodge') + 
  geom_text(aes(label = round(DV_incorrect,4)*100),position=position_dodge(width=1.75))+
  scale_fill_brewer(palette = "BuPu")+
  theme_bw()+
  theme(legend.position='',axis.title.x=element_blank())+
  ylab('Percentage of Incorrect Call')+
  ggtitle('Model 1: Call (DV) vs Home (IV)')

# Figure 2: Non-call vs Home
ggplot(agg2, aes(x = home_commit2, y = DV_incorrect, fill = home_commit2)) +
  geom_col(position = 'dodge') + 
  geom_text(aes(label = round(DV_incorrect,4)*100),position=position_dodge(width=1.75))+
  scale_fill_brewer(palette = "BuPu")+
  theme_bw()+
  theme(legend.position='',axis.title.x=element_blank())+
  ylab('Percentage of Incorrect Non-Call')+
  ggtitle('Model 2: Non-Call (DV) vs Home (IV)')

# Figure 3: Non-call vs Time
x <- c(1,2,3,4)
y <- c(69, 78, 94, 99)
df <- data.frame(x,y)
df$x2 <- factor(df$x,
                labels = c('>1.5','1.5-1.0','1.0-0.5','<0.5'))
ggplot(df,aes(x=x2, y=y,group=1))+
  geom_line(color="light blue")+
  geom_point(color="light blue")+
  geom_text(aes(label = y))+
  xlab('Fractional Minute (min)')+
  ylab('Frequency of Incorrect Non-Call')+
  theme_bw()+
  ggtitle('Model 2: Non-Call (DV) vs Time (IV)')




