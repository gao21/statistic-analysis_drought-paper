# statistic-analysis_drought-paper
##statistic analysis of plant community respond to drought, ANOVA

library(ggplot2)

library(Rmisc)

library(ggpubr)

library(dplyr) # for group_by and summarise_each function

library(plotrix)# for std error unction

library(multcomp)

library(lsmeans)

library(lme4)

##input data

cover=read.csv("/Users/chenguanggao/Desktop/Data and codes/Calculated resistance and recovery.csv")

head(cover)

cover1<-subset(cover,soil_types!="C")

head(cover1)

plot(x=cover1$plot,y=cover1$resis)




#### resistance

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(resilience, type = "mean_se")
  
#visualation of data

bxp <- ggboxplot(
  cover1, x = "soil_types", y = "resis", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

#Identify outliers by groups:

library(rstatix)

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(resis)

#Check normality assumption

model_resistance<- lm(resis~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_resistance))# Create a QQ plot of residuals

#computation

library(car)

model_resistance=lm(resis~soil_types*sterilization*EM,data = cover1)

anova_resistance<-Anova(model_resistance)

anova_resistance

#effect size

eta_squared(anova_resistance)

#one-way ANOVA

model_resistance1=lm(resis~soil_types,data = cover)

Anova(model_resistance1)

#post-hoc test

library(multcomp)

library(emmeans)

marginal=lsmeans(model_resistance1,~soil_types)

cld(marginal,
    alpha=0.05,
    Letters=letters,
    adjust="tukey")





##### BN-recovery

head(cover1)

plot(x=cover1$plot,y=cover1$bn_recovery)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(bn_recovery, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "bn_recovery", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(bn_recovery)

model_recovery_bn<- lm(bn_recovery~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_recovery_bn))

#computation
model_recovery_bn=lm(log(bn_recovery)~soil_types*sterilization*EM,data = cover1)

plot(model_recovery_bn)#model is ok for next anova

anova_recovery<-Anova(model_recovery_bn)

anova_recovery

#effect size

eta_squared(anova_recovery)

#one way anova

model_recovery1=lm(log(bn_recovery)~soil_types,data = cover)

Anova(model_recovery1)

#post-hoc test

marginal = lsmeans(model_recovery1, 
                   ~ soil_types)

cld(marginal,
    alpha=0.05,
    Letters=letters,     
    adjust="tukey") 




#####IN-recovery

head(cover1)

plot(x=cover1$plot,y=cover1$in_recovery)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(in_recovery, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "in_recovery", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(in_recovery)

#normality test

model_inrecovery<- lm(in_recovery~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_inrecovery))

#computation

model_inrecovery<-lm(in_recovery~soil_types*sterilization*EM,data = cover1)

plot(model_inrecovery)

anova_inrecovery<-Anova(model_inrecovery)

anova_inrecovery

#effect size

eta_squared(model_inrecovery)

#one way anova

model_inrecovery1<-lm(in_recovery~soil_types,data = cover)

anova_inrecovery1<-Anova(model_inrecovery1)

anova_inrecovery1

#post-hoc test

marginal = lsmeans(anova_inrecovery1, 
                   ~ soil_types,data=cover)

cld(marginal,
    alpha=0.05,
    Letters=letters,    
    adjust="tukey")




####grasses, resistance

head(cover1)

plot(x=cover1$plot,y=cover1$resis.grass)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(resis.grass, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "resis.grass", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(resis.grass)

#normality test

model_resisgra<- lm(resis.grass~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_resisgra))

#computation

model_resisgra<-lm(resis.grass~soil_types*sterilization*EM,data = cover1)

plot(model_resisgra)

anova_inrecovery<-Anova(model_resisgra)

anova_inrecovery

#effect size

eta_squared(model_resisgra)


##grasses,bn-recovery

head(cover1)

plot(x=cover1$plot,y=cover1$bn_recovery.grass)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(bn_recovery.grass, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "bn_recovery.grass", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(bn_recovery.grass)

#normality test

model_bnrecovery_gra<- lm(bn_recovery.grass~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_bnrecovery_gra))

#computation

model_bnrecovery_gra<-lm(log(bn_recovery.grass)~soil_types*sterilization*EM,data = cover1)

plot(model_bnrecovery_gra)

anova_inrecovery<-Anova(model_bnrecovery_gra)

anova_inrecovery

#effect size

eta_squared(model_bnrecovery_gra)


##grasses, in-recovery

head(cover1)

plot(x=cover1$plot,y=cover1$in_recovery.grass)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(in_recovery.grass, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "in_recovery.grass", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(in_recovery.grass)

#normality test

model_inrecovery_gra<- lm(in_recovery.grass~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_inrecovery_gra))

#computation

model_inrecovery_gra<-lm(log(in_recovery.grass)~soil_types*sterilization*EM,data = cover1)

plot(model_inrecovery_gra)

anova_inrecovery<-Anova(model_inrecovery_gra)

anova_inrecovery

#effect size

eta_squared(model_inrecovery_gra)




##forbs,resistance

head(cover1)

plot(x=cover1$plot,y=cover1$resis.forb)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(resis.forb, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "resis.forb", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(resis.forb)

#normality test

model_resisforb<- lm(resis.forb~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_resisforb))

#computation

model_resisforb<-lm(resis.forb~soil_types*sterilization*EM,data = cover1)

plot(model_resisforb)

anova_inrecovery<-Anova(model_resisforb)

anova_inrecovery

#effect size

eta_squared(model_resisforb)

#one way anova

model_resisforb1<-lm(resis.forb~soil_types,data = cover)

anova_resisforb1<-Anova(model_resisforb1)

anova_resisforb1

#post-hoc test

marginal = lsmeans(anova_resisforb1, 
                   ~ soil_types,data=cover)

cld(marginal,
    alpha=0.05,
    Letters=letters,    
    adjust="tukey")


##forbs,bn-recovery

head(cover1)

plot(x=cover1$plot,y=cover1$bn_recovery.forb)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(bn_recovery.forb, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "bn_recovery.forb", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(bn_recovery.forb)

#normality test

model_bnrecovery_forb<- lm(bn_recovery.forb~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_bnrecovery_forb))

#computation

model_bnrecovery_forb<-lm(bn_recovery.forb~soil_types*sterilization*EM,data = cover1)

plot(model_bnrecovery_forb)

anova_bnrecovery_forb<-Anova(model_bnrecovery_forb)

anova_bnrecovery_forb

#effect size

eta_squared(anova_bnrecovery_forb)


##forbs, in-recovery

head(cover1)

plot(x=cover1$plot,y=cover1$in_recovery.forb)

cover1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(in_recovery.forb, type = "mean_sd")
  
bxp <- ggboxplot(
  cover1, x = "soil_types", y = "in_recovery.forb", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(in_recovery.forb)

#normality test

model_inrecovery_forb<- lm(in_recovery.forb~soil_types*sterilization*EM,data = cover1)

ggqqplot(residuals(model_inrecovery_forb))

#computation

model_inrecovery_forb<-lm(in_recovery.forb~soil_types*sterilization*EM,data = cover1)

plot(model_inrecovery_forb)

anova_inrecovery_forb<-Anova(model_inrecovery_forb)

anova_inrecovery_forb

#effect size

eta_squared(model_inrecovery_forb)




##legumes,resistance

cover_legume=read.csv("/Users/chenguanggao/Desktop/Data and codes/calculated cover_legume.csv")

head(cover_legume)

cover_legume1<-subset(cover_legume,soil_types!="C")

head(cover_legume1)

cover_legume1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(resis_legume1, type = "mean_sd")
  
bxp <- ggboxplot(
  cover_legume1, x = "soil_types", y = "resis_legume1", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover_legume1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(resis_legume1)

#normality test

model_resislegume<- lm(resis_legume1~soil_types*sterilization*EM,data = cover_legume1)

ggqqplot(residuals(model_resislegume))

#computation

model_resislegume<-lm(resis_legume1~soil_types*sterilization*EM,data = cover_legume1)

plot(model_resislegume)

anova_resislegume<-Anova(model_resislegume)

anova_resislegume

#effect size

eta_squared(anova_resislegume)

#one way anova

model_resislegume1<-lm(resis_legume1~soil_types,data = cover_legume1)

aov_resislegume1<-Anova(model_resislegume1)

aov_resislegume1

#post-hoc test

marginal = lsmeans(model_resislegume1, 
                   ~ soil_types,data=cover_legume)

cld(marginal,
    alpha=0.05,
    Letters=letters,    
    adjust="tukey")


#legumes,in-recovery

head(cover_legume1)

cover_legume1 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(in_recovery.legume1, type = "mean_sd")
  
bxp <- ggboxplot(
  cover_legume1, x = "soil_types", y = "in_recovery.legume1", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover_legume1%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(in_recovery.legume1)

#normality test

model_inrecovery_legume<- lm(in_recovery.legume1~soil_types*sterilization*EM,data = cover_legume1)

ggqqplot(residuals(model_inrecovery_legume))

#computation

model_inrecovery_legume<-lm(in_recovery.legume1~soil_types*sterilization*EM,data = cover_legume1)

plot(model_inrecovery_legume)

anova_inrecoverylegume<-Anova(model_inrecovery_legume)

anova_inrecoverylegume

#effect size

eta_squared(model_inrecovery_legume)


#legumes,bn-recovery

head(cover_legume1)

cover_legume2<-na.omit(cover_legume1)

cover_legume2 %>%
  group_by(soil_types, sterilization, EM) %>%
  get_summary_stats(bn_recovery.legume1, type = "mean_sd")
  
bxp <- ggboxplot(
  cover_legume2, x = "soil_types", y = "bn_recovery.legume1", 
  color = "sterilization", palette = "jco", facet.by = "EM"
)

bxp

cover_legume2%>%
  group_by(soil_types, sterilization, EM) %>%
  identify_outliers(bn_recovery.legume1)

#normality test

model_inrecovery_legume<- lm(bn_recovery.legume1~soil_types*sterilization*EM,data = cover_legume2)

ggqqplot(residuals(model_inrecovery_legume))

#computation

model_inrecovery_legume<-lm(bn_recovery.legume1~soil_types*sterilization*EM,data = cover_legume2)

plot(model_inrecovery_legume)

anova_inrecoverylegume<-Anova(model_inrecovery_legume)

anova_inrecoverylegume

#effect size

eta_squared(model_inrecovery_legume)
