options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
library(ggpubr)
library(showtext)
library(showtextdb)
theme_set(theme_grey())  # pre-set the bw theme.
data("midwest", package = "ggplot2")

saturated_fatty_acids <- c(5.1, 7.5, 8.0, 8.3, 8.5, 9.5, 10.0, 10.0, 13.50, 16.0, 18.0, 19.0, 22.0) #Original data
CHD_deaths <- c(1.3, .1, 1.1, 0, 1.8, 1.9, .8, .4, 1.9, 3.2, 2.2, 3.3, 4.4)
x=data.frame(saturated_fatty_acids,CHD_deaths)
x_summary=summary(lm(saturated_fatty_acids~CHD_deaths,data=x))
x_p=x_summary$coefficients["CHD_deaths","Pr(>|t|)"]

showtext_auto

lm_eqn <- function(x){
  m <- lm(CHD_deaths ~ saturated_fatty_acids, x);
  eq <- substitute(italic(y) == a + b* italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

gg_fat <-ggplot(x,aes(x=saturated_fatty_acids,y=CHD_deaths))+
  geom_point(data=x,size=2) +
  geom_smooth(method='lm',se=FALSE,colour="cadetblue")+ #se=False for no standard error stuff
  xlim(c(0, 25)) + 
  ylim(c(0, 5))+
  theme(text = element_text(family = "MicrosoftYaHeiLight", color = "grey20"))+
  labs(title="% Calories as Saturated Fatty Acids vs Deaths from Coronary Heart Disease (CHD)",
      subtitle="Original data replicated from Seven Countries Study", 
       y="CHD Deaths per 100 persons", 
       x="% Calories, Saturated fatty acids",
       caption = "Source: Keys et al. (1980)")+
  theme(plot.title = element_text(colour="black",size=15, face="bold", margin = margin(10, 0, 10, 0)))+
  annotate("text", 15, 1.3, label = lm_eqn(x), parse=TRUE,
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 17.2, 1.0, label = paste("r =",format(x_summary$r.squared^.5,digits=2)),
         family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 13, 1.0, label = paste("p =",format(x_p,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)
plot(gg_fat)

ggfat_w_equation <- gg_fat + geom_text(x = 15, y = 4, label = lm_eqn(x), parse = TRUE)
plot(ggfat_w_equation)

saturated_fatty_acids2 <- c(5.1, 7.5, 8.0, 8.3, 8.5, 9.5, 10.0, 10.0, 13.50, 16.0, 18.0, 19.0, 22.0) ##Assume France has same saturated fat consumption
CHD_deaths2 <- c(1.3, .1, 1.1, 0, 1.8, 1.9, .8, .4, 1.9, 3.2, 2.2, 3.3, 1.1) ## Assume 4x less rate of heart attack. Can assume 5.2 at most. 
x2=data.frame(saturated_fatty_acids2,CHD_deaths2)
x2_summary=summary(lm(saturated_fatty_acids2 ~ CHD_deaths2,x2))
cor(x2$saturated_fatty_acids2, x2$CHD_deaths2) 
x_p2=x2_summary$coefficients["CHD_deaths2","Pr(>|t|)"]

lm_eqn <- function(x2){
  m <- lm(CHD_deaths2 ~ saturated_fatty_acids2, x2);
  eq <- substitute(italic(y) == a + b* italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

gg_fat2 <-ggplot(x2,aes(x=saturated_fatty_acids2,y=CHD_deaths2))+
  geom_point(data=x,size=2) +
  geom_smooth(method='lm',se=FALSE,colour="deeppink4")+ #se=False for no standard error stuff
  xlim(c(0, 25)) + 
  ylim(c(0, 5))+
  theme(text = element_text(family = "MicrosoftYaHeiLight", color = "grey20"))+
  labs(subtitle="With US excluded and France included.", 
       y="CHD Deaths per 100 persons", 
       x="% Calories, Saturated fatty acids", 
       title="% Calories as Saturated Fatty Acids vs Deaths from Coronary Heart Disease (CHD)", 
       caption = "Source: J. Yerushamly and Herman E. Hillaboe (1957)")+
  theme(plot.title = element_text(colour="black",size=15, face="bold", margin = margin(10, 0, 10, 0)))+
  annotate("text", 15, 1.3, label = lm_eqn(x2), parse=TRUE,
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 17.2, 1.0, label = paste("r =",format(x2_summary$r.squared^.5,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 13, 1.0, label = paste("p =",format(x_p2,digits=2)),
         family = "MicrosoftYaHeiLight", size = 4)
plot(gg_fat2)

saturated_fatty_acids_1 <- c(5.1, 7.5, 8.0, 8.3, 8.5, 9.2, 9.5, 10.0, 10.0, 13.50, 16.0, 18.0, 19.0, 22.0)
CHD_deaths_1 <- c(1.3, .1, 1.1, 0, 1.8, .5, 1.9, .8, .4, 1.9, 3.2, 2.2, 3.3, 4.4)
x_1=data.frame(saturated_fatty_acids_1,CHD_deaths_1)

ggscatter(x_1,x ="saturated_fatty_acids_1", y="CHD_deaths_1",add="reg.line",xlab="% Calories as Saturated Fatty Acids vs Deaths from Coronary Heart Disease (CHD)", 
          ylab="CHD Deaths per 100 persons") +
  stat_cor(label.x = 3, label.y = 5) +
  stat_regline_equation(label.x = 3, label.y = 4.5)


fat_per_total <- c(8.0,19.9,33.0,35.0,36.0,40.2)
CHD_deaths_1000 <- c(.6,1.7,3.9,5.5,5.6,6.9)
Figure1=data.frame(fat_per_total,CHD_deaths_1000)
linearModFigure1 <- lm(fat_per_total~CHD_deaths_1000,data=Figure1)
summaryFigure1 <- summary(linearModFigure1)
Figure1_p=summaryFigure1$coefficients["CHD_deaths_1000","Pr(>|t|)"]

lm_eqn_fat <- function(x){
  m <- lm(fat_per_total ~ CHD_deaths_1000, Figure1);
  eq <- substitute(italic(y) == a + b* italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

Re_Figure1 <-ggplot(Figure1,aes(x=fat_per_total,y=CHD_deaths_1000))+
  geom_point(data=Figure1,size=2) +
  geom_smooth(method='lm',se=FALSE,colour="cadetblue")+ #se=False for no standard error stuff
  xlim(c(0, 50)) + 
  ylim(c(0, 8))+
  theme(text = element_text(family = "MicrosoftYaHeiLight", color = "grey20"))+
  labs(subtitle="Reproduced from Keys data only taking Age 55-59 data", 
       y="CHD Deaths per 1000 persons", 
       x="Fat Calories as % of Total", 
       title="Fat Calories as % of Total vs Deaths per 1000 from Degenerative Heart Disease", 
       caption = "Source: Keys (1953)")+
  theme(plot.title = element_text(colour="black",size=15, face="bold", margin = margin(10, 0, 10, 0)))+
  annotate("text", 15, 4, label = lm_eqn_fat(Figure1), parse=TRUE,
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 18, 3.5, label = paste("r =",format(summary(linearModFigure1)$r.squared^.5,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 12, 3.5, label = paste("p =",format(Figure1_p,digits=2)),
         family = "MicrosoftYaHeiLight", size = 4)
plot(Re_Figure1)

fat_per_total_with_inuit <- c(8.0,19.9,33.0,35.0,36.0,40.2,39.0) ## lower estimate using data from Maynard et al
CHD_deaths_1000_with_inuit_lower <- c(.6,1.7,3.9,5.5,5.6,6.9,1.6)
Figure6=data.frame(fat_per_total_with_inuit,CHD_deaths_1000_with_inuit_lower)
summaryFigure6=summary(lm(fat_per_total_with_inuit ~ CHD_deaths_1000_with_inuit_lower,data=Figure6))
Figure6_p=summaryFigure6$coefficients["CHD_deaths_1000_with_inuit_lower","Pr(>|t|)"]
                                      
lm_eqn_fat_inuit <- function(x){
  m <- lm(fat_per_total_with_inuit ~ CHD_deaths_1000_with_inuit_lower, Figure6);
  eq <- substitute(italic(y) == a + b* italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

Figure6_plot <-ggplot(Figure6,aes(x=fat_per_total_with_inuit,y=CHD_deaths_1000_with_inuit_lower))+
  geom_point(data=Figure6,size=2) +
  geom_smooth(method='lm',se=FALSE,colour="cadetblue")+ #se=False for no standard error stuff
  xlim(c(0, 50)) + 
  ylim(c(0, 8))+
  theme(text = element_text(family = "MicrosoftYaHeiLight", color = "grey20"))+
  labs(subtitle="Addition of Inuit data point based on lower estimate from Maynard et al", 
       y="CHD Deaths per 1000 persons", 
       x="Fat Calories as % of Total", 
       title="Fat Calories as % of Total vs Deaths per 1000 from Degenerative Heart Disease", 
       caption = "Source: J. Yerushamly and Herman E. Hillaboe (1957)")+
  theme(plot.title = element_text(colour="black",size=15, face="bold", margin = margin(10, 0, 10, 0)))+
  annotate("text", 15, 4, label = lm_eqn_fat_inuit(Figure6), parse=TRUE,
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 18, 3.5, label = paste("r =",format(summaryFigure6$r.squared^.5,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 12, 3.5, label = paste("p =",format(Figure6_p,digits=2)),
         family = "MicrosoftYaHeiLight", size = 4)
plot(Figure6_plot)


fat_per_total_with_inuit2 <- c(8.0,19.9,33.0,35.0,36.0,40.2,39.0,29.0) ## Adding french estimate using Yerushalmy data
CHD_deaths_1000_with_inuit_lower2 <- c(.6,1.7,3.9,5.5,5.6,6.9,1.6,2.82) ##29% and 2.82 deaths per 1000
Figure7=data.frame(fat_per_total_with_inuit2,CHD_deaths_1000_with_inuit_lower2)
summaryFigure7=summary(lm(fat_per_total_with_inuit2 ~ CHD_deaths_1000_with_inuit_lower2,data=Figure7))
Figure7_p=summaryFigure7$coefficients["CHD_deaths_1000_with_inuit_lower2","Pr(>|t|)"]

lm_eqn_fat_inuit2 <- function(x){
  m <- lm(fat_per_total_with_inuit2 ~ CHD_deaths_1000_with_inuit_lower2, Figure7);
  eq <- substitute(italic(y) == a + b* italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
Figure7plot <-ggplot(Figure7,aes(x=fat_per_total_with_inuit2,y=CHD_deaths_1000_with_inuit_lower2))+
  geom_point(data=Figure7,size=2) +
  geom_smooth(method='lm',se=FALSE,colour="cadetblue")+ #se=False for no standard error stuff
  xlim(c(0, 50)) + 
  ylim(c(0, 8))+
  theme(text = element_text(family = "MicrosoftYaHeiLight", color = "grey20"))+
  labs(subtitle="Addition of French data-point based on Yerushalmy paper", 
       y="CHD Deaths per 1000 persons", 
       x="Fat Calories as % of Total", 
       title="Fat Calories as % of Total vs Deaths per 1000 from Degenerative Heart Disease", 
       caption = "Source: J. Yerushamly and Herman E. Hillaboe (1957)")+
  theme(plot.title = element_text(colour="black",size=15, face="bold", margin = margin(10, 0, 10, 0)))+
  annotate("text", 15, 4, label = lm_eqn_fat_inuit2(Figure7), parse=TRUE,
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 18, 3.5, label = paste("r =",format(summaryFigure7$r.squared^.5,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 12, 3.5, label = paste("p =",format(Figure7_p,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)
plot(Figure7plot)

fat_per_total_with_inuit3 <- c(8.0,19.9,33.0,35.0,36.0,40.2,39.0,20.0) ## Adding Chile estimate from Yerushalmy data to inuit data
CHD_deaths_1000_with_inuit_lower3 <- c(.6,1.7,3.9,5.5,5.6,6.9,1.6,6.03) ## Doesn't differentiate between men and women
Figure8=data.frame(fat_per_total_with_inuit3,CHD_deaths_1000_with_inuit_lower3)
summaryFigure8=summary(lm(fat_per_total_with_inuit3 ~ CHD_deaths_1000_with_inuit_lower3,data=Figure8))
Figure8_p=summaryFigure8$coefficients["CHD_deaths_1000_with_inuit_lower3","Pr(>|t|)"]

Figure8plot <-ggplot(Figure8,aes(x=fat_per_total_with_inuit3,y=CHD_deaths_1000_with_inuit_lower3))+
  geom_point(data=Figure8,size=2) +
  geom_smooth(method='lm',se=FALSE,colour="cadetblue")+ #se=False for no standard error stuff
  xlim(c(0, 50)) + 
  ylim(c(0, 8))+
  theme(text = element_text(family = "MicrosoftYaHeiLight", color = "grey20"))+
  labs(subtitle="Addition of French data-point based on Yerushalmy paper", 
       y="CHD Deaths per 1000 persons", 
       x="Fat Calories as % of Total", 
       title="Fat Calories as % of Total vs Deaths per 1000 from Degenerative Heart Disease", 
       caption = "Source: J. Yerushamly and Herman E. Hillaboe (1957)")+
  theme(plot.title = element_text(colour="black",size=15, face="bold", margin = margin(10, 0, 10, 0)))+
  annotate("text", 15, 4, label = lm_eqn_fat_inuit2(Figure8), parse=TRUE,
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 18, 3.5, label = paste("r =",format(summaryFigure8$r.squared^.5,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)+
  annotate("text", 12, 3.5, label = paste("p =",format(Figure8_p,digits=2)),
           family = "MicrosoftYaHeiLight", size = 4)
plot(Figure8plot)


