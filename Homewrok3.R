library(dplyr)
library(tidyr)
# load data
dd <-read.csv("Table3.csv")
# create a new dataset to store information of point estimate from the table
dd_PE <- (dd
          %>% select(risktype, info.type,PE_CL,PE_FL,PE_NY,PE_TE)
          %>% rename(California=PE_CL,Florida=PE_FL,New_York=PE_NY,Texas=PE_TE)
          %>% gather(key="location","estimates",-risktype,-info.type)
)
# create a new dataset to store information of confidence intervals
dd_CI <- (dd
          %>% select(risktype, info.type,X95..Range._CL,X95..Range._FL,X95..Range._NY,X95..Range_TE)
          %>% rename(California=X95..Range._CL,Florida=X95..Range._FL,New_York=X95..Range._NY,Texas=X95..Range_TE)
          %>% gather(key="location","CI",-risktype,-info.type)
          %>% separate(CI,c("LL","UL"),sep = "[–]")
)
# comebine two subsets together
dd2 <- (left_join(dd_PE,dd_CI))
# delete "," in numerical data of upper limit and lower limit of CI
dd3 <- (dd2
        %>% mutate(estimates = sub(x=estimates,pattern=",",replacement="")
                   , estimates = as.numeric(estimates)
                   , LL = sub(x=LL,pattern=",",replacement="")
                   , UL = sub(x=UL,pattern=",",replacement="")
                   , LL = as.numeric(LL)
                   , UL = as.numeric(UL))
)
#since it makes no sense to translate the x-axis into contimuous scale, so errorbar drawed instead of a ribbon
gg <- (ggplot(dd3, aes(x=risktype,y=estimates,color=location))
       + geom_point()
       + geom_errorbar(aes(ymin=LL,ymax=UL))
       + facet_grid(location~info.type)
)

print(gg)
