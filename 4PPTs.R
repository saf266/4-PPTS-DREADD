# Data for DREADD Project Round 2, repeated PPTs, 2 before birth of pups, 2 after birth of pups. N=10 females

#Inputing data


library(tidyverse)
library(readxl)
library(purrr)
library(dplyr)
library(stringr)
library(tibble)
library(recoder)
library(ggplot2)
library(ggthemes)


raw.data <- list.files(path ="/Users/santiagoforero/Desktop/DREADD Round 2 Repeated PPTs/", recursive = TRUE, full.names = TRUE)  
all.rawdata <- map(raw.data, function(x){
  data <- read_xlsx(x) %>% 
    group_by(Behavior) %>%
    filter(Behavior != "NA") %>%
    summarise(sum = sum(Duration_sf)) %>% 
    filter(str_detect(string = Behavior, pattern = "huddle"))
  # That loads the file, filters NA behaviors, sums the durations, only leaves HUDDLES
  colnames(data) <- c("Behavior", "Sum")
  if(nrow(data) == 0){
    data <- tibble(Behavior = c("Left huddle", "Right huddle"),
                   Sum = c(0, 0))
  }
  
  # this invents null data for missing rats
  
  if(nrow(data) == 1 & str_detect(data$Behavior, "Right")){
    data <- add_row(data, Behavior = "Left huddle", Sum = 0)
  }
  if(nrow(data) == 1 & str_detect(data$Behavior, "Left")){
    data <- add_row(data, Behavior = "Right huddle", Sum = 0)
  } # adds missing behavior IF dataframe has 1 column (i.e. missing one) and adds the missing one
  data$Behavior <- recode(data$Behavior, `Left huddle` = "Left", `Right huddle` = "Right")
  return(data)
})


#Finding each PPT and add Condition, Birth, id

#MO-08-27

#MO-08-27-PPT1, Partner on Left, Saline 
all.rawdata[[1]]$Behavior <- recode(all.rawdata[[1]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[1]] <- add_column(all.rawdata[[1]], Condition = c("Saline"))
all.rawdata[[1]] <- add_column(all.rawdata[[1]], id = c("MO-08-27"))
all.rawdata[[1]] <- add_column(all.rawdata[[1]], PPT = c("1"))
all.rawdata[[1]] <- add_column(all.rawdata[[1]], Birth = c("Before"))


#MO-08-27-PPT2, Partner on the Right, C21 
all.rawdata[[2]]$Behavior <- recode(all.rawdata[[2]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[2]] <- add_column(all.rawdata[[2]], Condition = c("C21"))
all.rawdata[[2]] <- add_column(all.rawdata[[2]], id = c("MO-08-27"))
all.rawdata[[2]] <- add_column(all.rawdata[[2]], PPT = c("2"))
all.rawdata[[2]] <- add_column(all.rawdata[[2]], Birth = c("Before"))



#MO-08-27-PPT3, Partner on the Left, C21 
all.rawdata[[3]]$Behavior <- recode(all.rawdata[[3]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[3]] <- add_column(all.rawdata[[3]], Condition = c("C21"))
all.rawdata[[3]] <- add_column(all.rawdata[[3]], id = c("MO-08-27"))
all.rawdata[[3]] <- add_column(all.rawdata[[3]], PPT = c("3"))
all.rawdata[[3]] <- add_column(all.rawdata[[3]], Birth = c("After"))




#MO-08-27-PPT4, Partner on the Left, Saline 
all.rawdata[[4]]$Behavior <- recode(all.rawdata[[4]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[4]] <- add_column(all.rawdata[[4]], Condition = c("Saline"))
all.rawdata[[4]] <- add_column(all.rawdata[[4]], id = c("MO-08-27"))
all.rawdata[[4]] <- add_column(all.rawdata[[4]], PPT = c("4"))
all.rawdata[[4]] <- add_column(all.rawdata[[4]], Birth = c("After"))


#MO-09.2-06

#MO-09.2-06 PPT1, Partner on the Right, C21 
all.rawdata[[5]]$Behavior <- recode(all.rawdata[[5]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[5]] <- add_column(all.rawdata[[5]], Condition = c("C21"))
all.rawdata[[5]] <- add_column(all.rawdata[[5]], id = c("MO-09.2-06"))
all.rawdata[[5]] <- add_column(all.rawdata[[5]], PPT = c("1"))
all.rawdata[[5]] <- add_column(all.rawdata[[5]], Birth = c("Before"))


#MO-09.2-06 PPT2, Partner on the Right, Saline 
all.rawdata[[6]]$Behavior <- recode(all.rawdata[[6]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[6]] <- add_column(all.rawdata[[6]], Condition = c("Saline"))
all.rawdata[[6]] <- add_column(all.rawdata[[6]], id = c("MO-09.2-06"))
all.rawdata[[6]] <- add_column(all.rawdata[[6]], PPT = c("2"))
all.rawdata[[6]] <- add_column(all.rawdata[[6]], Birth = c("Before"))


#MO-09.2-06 PPT3, Partner on the Right, Saline 
all.rawdata[[7]]$Behavior <- recode(all.rawdata[[7]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[7]] <- add_column(all.rawdata[[7]], Condition = c("Saline"))
all.rawdata[[7]] <- add_column(all.rawdata[[7]], id = c("MO-09.2-06"))
all.rawdata[[7]] <- add_column(all.rawdata[[7]], PPT = c("3"))
all.rawdata[[7]] <- add_column(all.rawdata[[7]], Birth = c("After"))



#MO-09.2-06 PPT4, Partner on the Right, C21 
all.rawdata[[8]]$Behavior <- recode(all.rawdata[[8]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[8]] <- add_column(all.rawdata[[8]], Condition = c("C21"))
all.rawdata[[8]] <- add_column(all.rawdata[[8]], id = c("MO-09.2-06"))
all.rawdata[[8]] <- add_column(all.rawdata[[8]], PPT = c("4"))
all.rawdata[[8]] <- add_column(all.rawdata[[8]], Birth = c("After"))




#PPP-02-5

#PPP-02-5 PPT1, Partner on the Left, Saline 
all.rawdata[[9]]$Behavior <- recode(all.rawdata[[9]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[9]] <- add_column(all.rawdata[[9]], Condition = c("Saline"))
all.rawdata[[9]] <- add_column(all.rawdata[[9]], id = c("PPP-02-5"))
all.rawdata[[9]] <- add_column(all.rawdata[[9]], PPT = c("1"))
all.rawdata[[9]] <- add_column(all.rawdata[[9]], Birth = c("Before"))


#PPP-02-5 PPT2, Partner on the Right, C21 
all.rawdata[[10]]$Behavior <- recode(all.rawdata[[10]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[10]] <- add_column(all.rawdata[[10]], Condition = c("C21"))
all.rawdata[[10]] <- add_column(all.rawdata[[10]], id = c("PPP-02-5"))
all.rawdata[[10]] <- add_column(all.rawdata[[10]], PPT = c("2"))
all.rawdata[[10]] <- add_column(all.rawdata[[10]], Birth = c("Before"))


#PPP-02-5 PPT3, Partner on the Left, C21 
all.rawdata[[11]]$Behavior <- recode(all.rawdata[[11]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[11]] <- add_column(all.rawdata[[11]], Condition = c("C21"))
all.rawdata[[11]] <- add_column(all.rawdata[[11]], id = c("PPP-02-5"))
all.rawdata[[11]] <- add_column(all.rawdata[[11]], PPT = c("3"))
all.rawdata[[11]] <- add_column(all.rawdata[[11]], Birth = c("After"))

#PPP-02-5 PPT4, Partner on the Right, Saline 
all.rawdata[[12]]$Behavior <- recode(all.rawdata[[12]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[12]] <- add_column(all.rawdata[[12]], Condition = c("Saline"))
all.rawdata[[12]] <- add_column(all.rawdata[[12]], id = c("PPP-02-5"))
all.rawdata[[12]] <- add_column(all.rawdata[[12]], PPT = c("4"))
all.rawdata[[12]] <- add_column(all.rawdata[[12]], Birth = c("After"))


#PPP-04-6

#PPP-04-6 PPT1, Partner on the Left, C21 
all.rawdata[[13]]$Behavior <- recode(all.rawdata[[13]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[13]] <- add_column(all.rawdata[[13]], Condition = c("C21"))
all.rawdata[[13]] <- add_column(all.rawdata[[13]], id = c("PPP-04-6"))
all.rawdata[[13]] <- add_column(all.rawdata[[13]], PPT = c("1"))
all.rawdata[[13]] <- add_column(all.rawdata[[13]], Birth = c("Before"))



#PPP-04-6 PPT2, Partner on the Right, Saline 
all.rawdata[[14]]$Behavior <- recode(all.rawdata[[14]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[14]] <- add_column(all.rawdata[[14]], Condition = c("Saline"))
all.rawdata[[14]] <- add_column(all.rawdata[[14]], id = c("PPP-04-6"))
all.rawdata[[14]] <- add_column(all.rawdata[[14]], PPT = c("2"))
all.rawdata[[14]] <- add_column(all.rawdata[[14]], Birth = c("Before"))


#PPP-04-6 PPT3, Partner on the Right, Saline 
all.rawdata[[15]]$Behavior <- recode(all.rawdata[[15]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[15]] <- add_column(all.rawdata[[15]], Condition = c("Saline"))
all.rawdata[[15]] <- add_column(all.rawdata[[15]], id = c("PPP-04-6"))
all.rawdata[[15]] <- add_column(all.rawdata[[15]], PPT = c("3"))
all.rawdata[[15]] <- add_column(all.rawdata[[15]], Birth = c("After"))


#PPP-04-6 PPT4, Partner on the Right, C21 
all.rawdata[[16]]$Behavior <- recode(all.rawdata[[16]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[16]] <- add_column(all.rawdata[[16]], Condition = c("C21"))
all.rawdata[[16]] <- add_column(all.rawdata[[16]], id = c("PPP-04-6"))
all.rawdata[[16]] <- add_column(all.rawdata[[16]], PPT = c("4"))
all.rawdata[[16]] <- add_column(all.rawdata[[16]], Birth = c("After"))


#PPP-05-5


#PPP-05-5 PPT1, Partner on the Left, C21 
all.rawdata[[17]]$Behavior <- recode(all.rawdata[[17]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[17]] <- add_column(all.rawdata[[17]], Condition = c("C21"))
all.rawdata[[17]] <- add_column(all.rawdata[[17]], id = c("PPP-05-5"))
all.rawdata[[17]] <- add_column(all.rawdata[[17]], PPT = c("1"))
all.rawdata[[17]] <- add_column(all.rawdata[[17]], Birth = c("Before"))


#PPP-05-5 PPT2, Partner on the Left, Saline 
all.rawdata[[18]]$Behavior <- recode(all.rawdata[[18]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[18]] <- add_column(all.rawdata[[18]], Condition = c("Saline"))
all.rawdata[[18]] <- add_column(all.rawdata[[18]], id = c("PPP-05-5"))
all.rawdata[[18]] <- add_column(all.rawdata[[18]], PPT = c("2"))
all.rawdata[[18]] <- add_column(all.rawdata[[18]], Birth = c("Before"))


#PPP-05-5 PPT3, Partner on the Right, C21 
all.rawdata[[19]]$Behavior <- recode(all.rawdata[[19]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[19]] <- add_column(all.rawdata[[19]], Condition = c("C21"))
all.rawdata[[19]] <- add_column(all.rawdata[[19]], id = c("PPP-05-5"))
all.rawdata[[19]] <- add_column(all.rawdata[[19]], PPT = c("3"))
all.rawdata[[19]] <- add_column(all.rawdata[[19]], Birth = c("After"))


#PPP-05-5 PPT4, Partner on the Left, Saline 
all.rawdata[[20]]$Behavior <- recode(all.rawdata[[20]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[20]] <- add_column(all.rawdata[[20]], Condition = c("Saline"))
all.rawdata[[20]] <- add_column(all.rawdata[[20]], id = c("PPP-05-5"))
all.rawdata[[20]] <- add_column(all.rawdata[[20]], PPT = c("4"))
all.rawdata[[20]] <- add_column(all.rawdata[[20]], Birth = c("After"))


#PPP-09-5

#PPP-09-5 PPT1, Partner on the Right, C21 
all.rawdata[[21]]$Behavior <- recode(all.rawdata[[21]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[21]] <- add_column(all.rawdata[[21]], Condition = c("C21"))
all.rawdata[[21]] <- add_column(all.rawdata[[21]], id = c("PPP-09-5"))
all.rawdata[[21]] <- add_column(all.rawdata[[21]], PPT = c("1"))
all.rawdata[[21]] <- add_column(all.rawdata[[21]], Birth = c("Before"))

#PPP-09-5 PPT2, Partner on the Right, Saline 
all.rawdata[[22]]$Behavior <- recode(all.rawdata[[22]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[22]] <- add_column(all.rawdata[[22]], Condition = c("Saline"))
all.rawdata[[22]] <- add_column(all.rawdata[[22]], id = c("PPP-09-5"))
all.rawdata[[22]] <- add_column(all.rawdata[[22]], PPT = c("2"))
all.rawdata[[22]] <- add_column(all.rawdata[[22]], Birth = c("Before"))

#PPP-09-5 PPT3, Partner on the Right, Saline 
all.rawdata[[23]]$Behavior <- recode(all.rawdata[[23]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[23]] <- add_column(all.rawdata[[23]], Condition = c("Saline"))
all.rawdata[[23]] <- add_column(all.rawdata[[23]], id = c("PPP-09-5"))
all.rawdata[[23]] <- add_column(all.rawdata[[23]], PPT = c("3"))
all.rawdata[[23]] <- add_column(all.rawdata[[23]], Birth = c("After"))

#PPP-09-5 PPT4, Partner on the Left, C21 
all.rawdata[[24]]$Behavior <- recode(all.rawdata[[24]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[24]] <- add_column(all.rawdata[[24]], Condition = c("C21"))
all.rawdata[[24]] <- add_column(all.rawdata[[24]], id = c("PPP-09-5"))
all.rawdata[[24]] <- add_column(all.rawdata[[24]], PPT = c("4"))
all.rawdata[[24]] <- add_column(all.rawdata[[24]], Birth = c("After"))


#PPP-11-5
#PPP-11-5 PPT1, Partner on the Right, C21 
all.rawdata[[25]]$Behavior <- recode(all.rawdata[[25]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[25]] <- add_column(all.rawdata[[25]], Condition = c("C21"))
all.rawdata[[25]] <- add_column(all.rawdata[[25]], id = c("PPP-11-5"))
all.rawdata[[25]] <- add_column(all.rawdata[[25]], PPT = c("1"))
all.rawdata[[25]] <- add_column(all.rawdata[[25]], Birth = c("Before"))

#PPP-11-5 PPT2, Partner on the Left, Saline 
all.rawdata[[26]]$Behavior <- recode(all.rawdata[[26]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[26]] <- add_column(all.rawdata[[26]], Condition = c("Saline"))
all.rawdata[[26]] <- add_column(all.rawdata[[26]], id = c("PPP-11-5"))
all.rawdata[[26]] <- add_column(all.rawdata[[26]], PPT = c("2"))
all.rawdata[[26]] <- add_column(all.rawdata[[26]], Birth = c("Before"))

#PPP-11-5 PPT3, Partner on the Left, C21 
all.rawdata[[27]]$Behavior <- recode(all.rawdata[[27]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[27]] <- add_column(all.rawdata[[27]], Condition = c("C21"))
all.rawdata[[27]] <- add_column(all.rawdata[[27]], id = c("PPP-11-5"))
all.rawdata[[27]] <- add_column(all.rawdata[[27]], PPT = c("3"))
all.rawdata[[27]] <- add_column(all.rawdata[[27]], Birth = c("After"))

#PPP-11-5 PPT4, Partner on the Left, Saline 
all.rawdata[[28]]$Behavior <- recode(all.rawdata[[28]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[28]] <- add_column(all.rawdata[[28]], Condition = c("Saline"))
all.rawdata[[28]] <- add_column(all.rawdata[[28]], id = c("PPP-11-5"))
all.rawdata[[28]] <- add_column(all.rawdata[[28]], PPT = c("4"))
all.rawdata[[28]] <- add_column(all.rawdata[[28]], Birth = c("After"))


#PV-10-9

#PV-10-9 PPT1, Partner on the Left, C21 
all.rawdata[[29]]$Behavior <- recode(all.rawdata[[29]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[29]] <- add_column(all.rawdata[[29]], Condition = c("C21"))
all.rawdata[[29]] <- add_column(all.rawdata[[29]], id = c("PV-10-9"))
all.rawdata[[29]] <- add_column(all.rawdata[[29]], PPT = c("1"))
all.rawdata[[29]] <- add_column(all.rawdata[[29]], Birth = c("Before"))

#PV-10-9 PPT2, Partner on the Left, Saline 
all.rawdata[[30]]$Behavior <- recode(all.rawdata[[30]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[30]] <- add_column(all.rawdata[[30]], Condition = c("Saline"))
all.rawdata[[30]] <- add_column(all.rawdata[[30]], id = c("PV-10-9"))
all.rawdata[[30]] <- add_column(all.rawdata[[30]], PPT = c("2"))
all.rawdata[[30]] <- add_column(all.rawdata[[30]], Birth = c("Before"))

#PV-10-9 PPT3, Partner on the Left, Saline 
all.rawdata[[31]]$Behavior <- recode(all.rawdata[[31]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[31]] <- add_column(all.rawdata[[31]], Condition = c("Saline"))
all.rawdata[[31]] <- add_column(all.rawdata[[31]], id = c("PV-10-9"))
all.rawdata[[31]] <- add_column(all.rawdata[[31]], PPT = c("3"))
all.rawdata[[31]] <- add_column(all.rawdata[[31]], Birth = c("After"))

#PV-10-9 PPT4, Partner on the Right, C21 
all.rawdata[[32]]$Behavior <- recode(all.rawdata[[32]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[32]] <- add_column(all.rawdata[[32]], Condition = c("C21"))
all.rawdata[[32]] <- add_column(all.rawdata[[32]], id = c("PV-10-9"))
all.rawdata[[32]] <- add_column(all.rawdata[[32]], PPT = c("4"))
all.rawdata[[32]] <- add_column(all.rawdata[[32]], Birth = c("After"))


#PV-12-2

#PV-12-2 PPT1, Partner on the Right, Saline 
all.rawdata[[33]]$Behavior <- recode(all.rawdata[[33]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[33]] <- add_column(all.rawdata[[33]], Condition = c("Saline"))
all.rawdata[[33]] <- add_column(all.rawdata[[33]], id = c("PV-12-2"))
all.rawdata[[33]] <- add_column(all.rawdata[[33]], PPT = c("1"))
all.rawdata[[33]] <- add_column(all.rawdata[[33]], Birth = c("Before"))

#PV-12-2 PPT2, Partner on the Left, C21 
all.rawdata[[34]]$Behavior <- recode(all.rawdata[[34]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[34]] <- add_column(all.rawdata[[34]], Condition = c("C21"))
all.rawdata[[34]] <- add_column(all.rawdata[[34]], id = c("PV-12-2"))
all.rawdata[[34]] <- add_column(all.rawdata[[34]], PPT = c("2"))
all.rawdata[[34]] <- add_column(all.rawdata[[34]], Birth = c("Before"))

#PV-12-2 PPT3, Partner on the Left, C21 
all.rawdata[[35]]$Behavior <- recode(all.rawdata[[35]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[35]] <- add_column(all.rawdata[[35]], Condition = c("C21"))
all.rawdata[[35]] <- add_column(all.rawdata[[35]], id = c("PV-12-2"))
all.rawdata[[35]] <- add_column(all.rawdata[[35]], PPT = c("3"))
all.rawdata[[35]] <- add_column(all.rawdata[[35]], Birth = c("After"))

#PV-12-2 PPT4, Partner on the Left, Saline 
all.rawdata[[36]]$Behavior <- recode(all.rawdata[[36]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[36]] <- add_column(all.rawdata[[36]], Condition = c("Saline"))
all.rawdata[[36]] <- add_column(all.rawdata[[36]], id = c("PV-12-2"))
all.rawdata[[36]] <- add_column(all.rawdata[[36]], PPT = c("4"))
all.rawdata[[36]] <- add_column(all.rawdata[[36]], Birth = c("After"))


#VTA-2-9

#VTA-2-9 PPT1, Partner on the Right, Saline 
all.rawdata[[37]]$Behavior <- recode(all.rawdata[[37]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[37]] <- add_column(all.rawdata[[37]], Condition = c("Saline"))
all.rawdata[[37]] <- add_column(all.rawdata[[37]], id = c("VTA-2-9"))
all.rawdata[[37]] <- add_column(all.rawdata[[37]], PPT = c("1"))
all.rawdata[[37]] <- add_column(all.rawdata[[37]], Birth = c("Before"))

#VTA-2-9 PPT2, Partner on the Left, C21 
all.rawdata[[38]]$Behavior <- recode(all.rawdata[[38]]$Behavior, Left = "Partner", Right = "Stranger")
all.rawdata[[38]] <- add_column(all.rawdata[[38]], Condition = c("C21"))
all.rawdata[[38]] <- add_column(all.rawdata[[38]], id = c("VTA-2-9"))
all.rawdata[[38]] <- add_column(all.rawdata[[38]], PPT = c("2"))
all.rawdata[[38]] <- add_column(all.rawdata[[38]], Birth = c("Before"))

#VTA-2-9 PPT3, Partner on the Right, Saline 
all.rawdata[[39]]$Behavior <- recode(all.rawdata[[39]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[39]] <- add_column(all.rawdata[[39]], Condition = c("Saline"))
all.rawdata[[39]] <- add_column(all.rawdata[[39]], id = c("VTA-2-9"))
all.rawdata[[39]] <- add_column(all.rawdata[[39]], PPT = c("3"))
all.rawdata[[39]] <- add_column(all.rawdata[[39]], Birth = c("After"))

#VTA-2-9 PPT4, Partner on the Right, C21 
all.rawdata[[40]]$Behavior <- recode(all.rawdata[[40]]$Behavior, Left = "Stranger", Right = "Partner")
all.rawdata[[40]] <- add_column(all.rawdata[[40]], Condition = c("C21"))
all.rawdata[[40]] <- add_column(all.rawdata[[40]], id = c("VTA-2-9"))
all.rawdata[[40]] <- add_column(all.rawdata[[40]], PPT = c("4"))
all.rawdata[[40]] <- add_column(all.rawdata[[40]], Birth = c("After"))


#Plotting raw data

library(lme4)
library(emmeans)
library(lmerTest)

cleandata <-bind_rows(all.rawdata) %>% 
  select(Behavior,Sum,Condition,id,Birth) %>%
  # mutate(PPT=as.character(PPT)) %>% 
  group_by(id) %>% 
  mutate(Birth = factor(Birth,levels=c("Before", "After")), Condition= factor(Condition, levels = c("Saline", "C21")))
# levels(cleandata$Birth)  <- c("Before", "After")
# levels(cleandata$Condition)  <- c("Saline", "C21")

alldatalmer<- lmer(Sum~ Behavior*Condition*Birth + (1|id), data=cleandata)
summary(alldatalmer)
anova(alldatalmer, ddf = "Kenward-Roger")
hist(resid(alldatalmer))
plot(predict(alldatalmer), resid(alldatalmer))
plotlmer(alldatalmer)
#Looking at Difference in Behavior, holding Condition & Birth constant
emmeans(alldatalmer, pairwise~ Behavior|Condition*Birth)

ggplot(cleandata, aes(x = Behavior, y = Sum, color = id, group = id)) + geom_point() + geom_line() + facet_wrap(vars(Birth, Condition))

#Looking at Difference in Condition, holding Behavior & Birth
emmeans(alldatalmer, pairwise~ Condition|Behavior*Birth)

ggplot(cleandata, aes(x = Condition, y = Sum, color = id, group = id)) + geom_point() + geom_line() + facet_wrap(vars(Birth, Behavior))

#Looking at Difference in Birth, holding Condition & Behavior 
emmeans(alldatalmer, pairwise~ Birth|Condition*Behavior)

ggplot(cleandata, aes(x = Birth, y = Sum, color = id, group = id)) + geom_point() + geom_line() + facet_wrap(vars(Condition, Behavior))


emmip(alldatalmer, Behavior ~ Birth | Condition)

emmip(alldatalmer, Behavior ~ Condition | Birth, CIs = T)

df <- data.frame(alldatalmer = c("Before", "After"))

emmip(alldatalmer, Condition ~ Birth | Behavior)

emmip(alldatalmer, Condition ~ Birth*Behavior, CIs = T)

ggplot()




##
Partnerbeforedata<- cleandata %>% 
  filter(Behavior != "Stranger") %>% 
  filter(Birth != "After") %>% 
  group_by(id)

Plotpartnerbeforedata<-ggplot(data=(Partnerbeforedata), aes(x=Condition, y=Sum)) + labs(x= "", y= "Time In Seconds") +
  geom_bar(stat = "summary", fun.y="mean", width = 0.5, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5,  position = position_dodge(width = 0.5)) + ggtitle("Partner") + 
  theme(plot.title = element_text(hjust = 0.5, size=20,face="bold"))

Plotpartnerbeforedata

Plot<-ggplot(data=(Partnerbeforedata), aes(x=Condition, y=Sum)) + labs(x= "", y= "Time In Seconds")+
  geom_point(aes(color=factor = Condition)) + geom_line(aes(color = id)) +
  geom_bar(stat = "summary", fun.y="mean", width = 0.5, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, position = position_dodge(width = 0.5)) + 
  ggtitle("Paired") +
  theme(plot.title = element_text(hjust = 0.5, size=20,face="bold"))

Plot

partnerbeforelm<- lm(Sum~ Condition, data=Partnerbeforedata)

(emmeans(partnerbeforelm,"Condition")) %>% 
  contrast("pairwise")

partnerbeforelm<- lmer(Sum~Condition + (1|id), data=Partnerbeforedata)
emmeans(partnerbeforelm,"Condition") %>% 
  contrast("pairwise")


#After pup birth

Partnerafterdata<- cleandata %>% 
  filter(Behavior != "Stranger") %>% 
  filter(Birth != "Before") %>% 
  group_by(id)

Plotpartnerafterdata<-ggplot(data=(Partnerafterdata), aes(x=Condition, y=Sum)) + labs(x= "", y= "Time In Seconds") +
  geom_bar(stat = "summary", fun.y="mean", width = 0.5, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5,  position = position_dodge(width = 0.5)) + ggtitle("Partner") + 
  theme(plot.title = element_text(hjust = 0.5, size=20,face="bold"))

Plotpartnerafterdata



partnerafterlm<- lmer(Sum~Condition + (1|id), data=Partnerafterdata)
emmeans(partnerafterlm,"Condition") %>% 
  contrast("pairwise")




#Comparing Partner - Stranger Before Birth

PPTbeforedata<- cleandata %>% 
  filter(Birth != "Before") %>% 
  group_by(id)



