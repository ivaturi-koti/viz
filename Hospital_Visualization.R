install.packages("missForest")
library(LCA)
library(car)
library(lattice)
library(ggplot2)
library(dplyr)
library(missForest)

spc.raw<-read.csv("HCAHPS.csv")
set.seed(3450)
iris.imp <- missForest(spc.mis, xtrue = spc.raw, verbose = TRUE)
spc.mis <- prodNA(spc.raw$H_RECMND_DY, noNA = 0.2)



table(is.na(spc.raw))
spc.raw$Operating.Margin
str(spc.raw$mcrnum)
spc.raw$Yearh<-as.factor(spc.raw$Yearh)

spc.raw2<-spc.raw %>% mutate_each_(funs(scale(.)%>% as.vector),
                  vars=c("operating.revenue","OM", "total.operating.expense", "ROA", "Return.on.Equity"))


spc.raw$Recoscore<-logit(spc.raw$H_RECMND_DY)


str(spc.raw$H_RECMND_DY)


spc.raw$recoscore
spc.raw$H_RECMND_DY<-as.integer(spc.raw$H_RECMND_DY)
str(spc.raw$H_RECMND_DY)
str(spc.raw$Yearh)
xyplot(
  ROA ~
    Yearh, 
  data=spc.raw,
  groups=Yearh,
  auto.key=TRUE
)


densityplot(
  spc.raw$operating.revenue,
  main="Kernel Density of Petal Length", 
  type="percent", 
)

?densityplot


spc.raw<-filter(spc.raw,operating.revenue>0)
spc.raw<-filter(spc.raw, commscore>0)
spc.raw<-filter(spc.raw, carescore>0)
spc.raw<-filter(spc.raw, facilityscore>0)
spc.raw$operating.revenue.log<-log(spc.raw$operating.revenue)
ROA.invs<-filter(ROA.invs, ROA<5)

boxplot(ROA ~ Yearh, data=spc.raw)


summary(ROA.invs$ROA)
# Performance over time
ggplot(spc.raw[1500:5000,], aes(x=Yearh, y=OM, group=mcrnum, color=Yearh))+geom_line()+labs(title="OM over time")

ggplot(spc.raw[1500:500,], aes(x=Yearh, y=Return.on.Equity, group=mcrnum, color=Yearh))+geom_point()+labs(title="ROE over time")

ggplot(ROA.invs, aes(x=Yearh, y=ROA, group=mcrnum, color=Yearh))+geom_line()+labs(title="ROA over time")


max(spc.raw$ROA)
summary(ROA.invs$ROA)
summary(spc.raw$ROA)



#Performance over time faceted by mapp8 and bsc

ggplot(spc.raw, aes(x=Yearh, y=operating.revenue.log, group=mcrnum, color=Yearh))+geom_point()+labs(title="Operating revenue over time")
ggplot(spc.raw, aes(x=Yearh, y=OM, group=mcrnum, color=Yearh))+geom_line()+labs(title="OM over time")+facet_wrap(~bsc)

ggplot(spc.raw, aes(x=Yearh, y=Return.on.Equity, group=mcrnum, color=Yearh))+geom_line()+labs(title="ROE over time")+facet_wrap(~mapp8)
ggplot(spc.raw, aes(x=Yearh, y=Return.on.Equity, group=mcrnum, color=Yearh))+geom_line()+labs(title="ROE over time")++facet_wrap(~bsc)


ggplot(spc.raw[1:1000,], aes(x=Yearh, y=ROA, group=mcrnum, color=Yearh))+geom_line()+labs(title="ROA over time")+facet_wrap(~mapp8)
ggplot(spc.raw, aes(x=Yearh, y=ROA, group=mcrnum, color=Yearh))+geom_line()+labs(title="ROA over time")+facet_wrap(~bsc)


ROA.invs<-select(spc.raw, ROA, Yearh)

spc.raw
ggplot(spc.raw, aes(x=carescore, y=operating.revenue, color=Yearh))+geom_point()
str(spc.raw$Yearf)


ggplot(spc.raw, aes(x=carescore, y=operating.revenue.log, color=Yearh))+geom_point()

ggplot(spc.raw, aes(x=Yearh, y=OM,group=Yearf))+geom_line()+ stat_summary(aes(group = 1), geom = "point", fun.y = mean,
               shape = 17, size = 3) + facet_grid(. ~mapp8)

pairs(~commscore+ carescore + facilityscore + operating.revenue+total.operating.expense, data=spc.raw, lower.panel=NULL,
      main="Simple Scatter Matrix"
)

+scale_y_continuous(trans = "log")+scale_x_continuous(trans = "log")
-------------------------------------------------------------------------------------------------------------








spc.performance<-select(spc.raw, OM, ROA, Return.on.Equity,Yearh)
spc.satisfaction<-select(spc.raw, carescore, facilityscore, commscore, Recoscore, Yearh)
str(spc.combined)

spc.combined<-select(spc.raw, operating.revenue, total.operating.expense,carescore, facilityscore, commscore, Recoscore, Yearh)
ggpairs(spc.combined, ggplot2::aes(color=Yearh),title="Correlational matrix", lower.panel=NULL)
ggpairs(iris, ggplot2::aes(color=Species))


ggduo(
  spc.performance, spc.satisfaction,
  types = list(continous="smooth_lm"),
  title = "Between correlation",
  xlab="Satisfaction",
  ylab="Performance"
)












