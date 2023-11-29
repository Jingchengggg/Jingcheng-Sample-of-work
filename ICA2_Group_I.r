# Read the data
ReferendumResults<-read.csv("ReferendumResults.csv", header=TRUE)

################################################################################
########################## Exploratory data analysis ###########################
################################################################################

###
### Step 1: Calculate the Proportion of Leave Votes and label the missing values
###

Proportion_leave<-ReferendumResults$Leave/ReferendumResults$NVotes
ReferendumResults_new<-transform(ReferendumResults, 
                                 P_leave=Proportion_leave, 
                                 Leave=NULL) #insert the data into the data set

# label the missing data as "NA"
ReferendumResults_new[804:1070,49]<-"NA"

###
### Step 2: Aggregate or Extract variables based on original data
###

### 2.1 Aggregate Age into 4 Groups (under 18, young adults, adults, elderlys)
Age_un18 <- (ReferendumResults_new$Age_0to4 + ReferendumResults_new$Age_5to7
          +ReferendumResults_new$Age_8to9 + ReferendumResults_new$Age_10to14
          +ReferendumResults_new$Age_15 + ReferendumResults_new$Age_16to17)

Age_young_adults <-(ReferendumResults_new$Age_18to19 
                    + ReferendumResults_new$Age_20to24
                    + ReferendumResults_new$Age_25to29)

Age_adults<-  (ReferendumResults_new$Age_30to44 
              + ReferendumResults_new$Age_45to59 + ReferendumResults_new$Age_60to64)

Age_elderlys<- (ReferendumResults_new$Age_65to74 + ReferendumResults_new$Age_75to84
               + ReferendumResults_new$Age_85to89 + ReferendumResults_new$Age_90plus)

# replace the original variables with updated ones
ReferendumResults_new<-transform(ReferendumResults_new,
                                 Age_un18= Age_un18,
                                 Age_young_adults=Age_young_adults,
                                 Age_adults=Age_adults,
                                 Age_elderlys=Age_elderlys,
                                 Age_0to4=NULL, Age_5to7=NULL, Age_8to9=NULL, Age_10to14=NULL,
                                 Age_15=NULL, Age_16to17=NULL, Age_18to19=NULL, Age_20to24=NULL,
                                 Age_25to29=NULL, Age_30to44=NULL, Age_45to59=NULL, Age_60to64=NULL,
                                 Age_65to74=NULL, Age_75to84=NULL, Age_85to89=NULL, Age_90plus=NULL
                                 ) 

### 2.2 Separate people who owned house with mortgage from people who without mortgage 
Owned_Mort<-ReferendumResults_new$Owned-ReferendumResults_new$OwnedOutright
Owned_No_Mort<-ReferendumResults_new$OwnedOutright

# copy column DE and insert it again to make these two variables are next to each other
ReferendumResults_new<-transform(ReferendumResults_new,
                                 Owned_Mort=Owned_Mort,
                                 Owned_No_Mort=Owned_No_Mort,
                                 Owned=NULL, OwnedOutright=NULL)

### 2.3 Based definition in 2011 census, social grades can be defined into:

# AB: Higher & intermediate managerial, administrative, professional occupations
# C1: Supervisory, clerical & junior managerial, administrative, professional occupations
# C2: Skilled manual occupations	
# DE: Semi-skilled & unskilled manual occupations, Unemployed and lowest grade occupations
# The current data set provides the % of households in grade C1C2DE, C2DE, and DE.
# Therefore, we will use the current information to define 
# the new variables: AB, C1C2, DE
AB<-100-ReferendumResults_new$C1C2DE # get % ofAB based on % of C1C2DE
C1C2<-ReferendumResults_new$C1C2DE-ReferendumResults_new$DE

# Extract C1, C2 from original C1C2DE and DE
DE<-ReferendumResults_new$DE 

# copy column DE and insert it again to make these three variables are next to each other
ReferendumResults_new<-transform(ReferendumResults_new,
                                 Grade_AB=AB,
                                 Grade_C1C2=C1C2,
                                 Grade_DE=DE, 
                                 C1C2DE=NULL, C2DE=NULL, DE=NULL)

###
### Step 3: Apply PCA
### 

### 3.1 A function used to help illustrating the correlations

# The function is for combined use with function pairs() to replace the original 
# upper panel plots with the correlation between two covariates.
# The higher absolute value of the correlation, the bigger the font size.
# (This function is be adapted from an idea originally posted on Stackoverflow)
show.cor <- function(x, y, cex.cor, ...)
{
  usr <- par("usr")
  par(usr = c(0, 1, 0, 1))
  corr.xy <- round(cor(x, y),2) 
  corr_size<-((abs(corr.xy))+1)^(2)*0.8
  col_set<- "black"
    if (corr.xy<0) col_set<-"blue"
    text(0.5,0.5, corr.xy, cex=corr_size, col=col_set)
}

### 3.2 Apply PCA and new social&finance status index created
pdf("Figure_1.pdf")
pairs(ReferendumResults_new[,c(21:24,26,27,35:37)],upper.panel=show.cor,pch=20 )

# As shown in the plot, we found noticeable correlation among 9 covariates:
# "Unemp" "UnempRate_EA" "HigherOccup"  "RoutineOccupOrLTU"
# "Deprived" "MultiDepriv"         
# "Grade_AB" "Grade_C1C2" "Grade_DE"
dev.off()

PCA_social_fin_status<-prcomp(ReferendumResults_new[,c(21:24,26,27,35:37)], scale.=TRUE)
print(PCA_social_fin_status)
print(summary(PCA_social_fin_status))
# The first two PC2 captures 94.43% of the information, and the third PC
# will only add 2% to the Cumulative Proportion of Variance, 
# which we consider it as insignificant.

# Based on the coefficients, we find the higher the first PC index, the more
# proportion of people in this ward have higher level jobs, and the less
# proportion of people who are unemployed or deprived. Therefore, we name the
# first PC index as Social status and Finance condition index (soc_fin_index).

# The second column in "PCA_social_fin_status$rotation" shows a large positive
# coefficient for Grade_C1C2. This implies that we need a large proportion 
# of Lower middle or working class. We call it (lower_mid_work_index)
Original_soc_fin_data<-as.matrix(scale(ReferendumResults_new[,c(21:24,26,27,35:37)]),ncol=9)
PCs_social_fin_status<-matrix(PCA_social_fin_status$rotation[1:9,1:2], nrow=9)
Social_fin_status_index<-Original_soc_fin_data%*%PCs_social_fin_status

# replace the original data with new index created
ReferendumResults_new<-transform(ReferendumResults_new,
                                 soc_fin_index=Social_fin_status_index[,1],
                                 lower_mid_work_index=Social_fin_status_index[,2],
                                 Unemp=NULL, UnempRate_EA=NULL, HigherOccup=NULL, 
                                 RoutineOccupOrLTU=NULL, Deprived=NULL, MultiDepriv=NULL,
                                 Grade_AB=NULL, Grade_C1C2=NULL, Grade_DE=NULL) 


###
### Step 4: Use Hierarchical Clustering on Region Name
###

# Means of the numeric covariates for each region 
RegionMeans <- aggregate(ReferendumResults_new[,-c(1:3,5,22)], by=list(ReferendumResults_new$RegionName), FUN=mean)
rownames(RegionMeans) <- RegionMeans[,1]
RegionMeans <- scale(RegionMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(RegionMeans) # Pairwise distances
ClusTree <- hclust(Distances, method="ward.D") # Do the clustering with "Ward Method"
par(mfrow=c(1,1))
par(mar=c(3,3,3,1), mgp=c(2,0.75,0)) # Set plot margins
pdf("Figure_2.pdf")
plot(ClusTree, xlab="Region name", ylab="Separation", cex.main=0.8)
abline(h=9, col="red", lty=2)
dev.off()

# Based on the plot, we cut trees into 3 groups
NewGroups <- cutree(ClusTree, k=3)
cat("\nThe New groups after clustering \n")
print(NewGroups, width=90)

# Based on the tree
# Group 1: East Midlands, London; 
# Group 2: South West, South East, East of England, North East; 
# Group 3: West Midlands, North West, Yorkshire and The Humber;
ReferendumResults_new<-transform(ReferendumResults_new, Area_Groups=rep(0,1070))

# assign group number to each region
ReferendumResults_new$Area_Groups[ReferendumResults_new$RegionName== "East Midlands"|
                              ReferendumResults_new$RegionName=="London"] <- "G1"

ReferendumResults_new$Area_Groups[ReferendumResults_new$RegionName== "South West" |
                              ReferendumResults_new$RegionName=="South East"  |
                              ReferendumResults_new$RegionName=="East of England" |
                              ReferendumResults_new$RegionName== "North East"] <- "G2"

ReferendumResults_new$Area_Groups[ReferendumResults_new$RegionName=="West Midlands"  |
                            ReferendumResults_new$RegionName=="North West" |
                            ReferendumResults_new$RegionName=="Yorkshire and The Humber"] <- "G3"

ReferendumResults_new<-transform(ReferendumResults_new,
                                 RegionName=NULL)


###
### Step 5: Plots for selected variables that show clear relationship with P_leave
###

# extract all the available data to model
ReferendumResults_ava<-ReferendumResults_new[1:803,] 
ReferendumResults_ava$P_leave<-as.numeric(ReferendumResults_ava$P_leave)

# Boxplots
pdf("Figure_3.pdf")
par(mfrow=c(1,2))
boxplot(P_leave~AreaType, data=ReferendumResults_ava, 
        ylab="Proportion of leave votes", xlab = "AreaType")
points(c(mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$AreaType=="E06"]),
         mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$AreaType=="E07"]),
         mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$AreaType=="E08"]),
         mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$AreaType=="E09"])),
       pch=16,col="red",cex=2)

boxplot(P_leave~Area_Groups, data=ReferendumResults_ava, 
        ylab="Proportion of leave votes", xlab = "Area_Groups")
points(c(mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$Area_Groups=="G1"]),
         mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$Area_Groups=="G2"]),
         mean(ReferendumResults_ava$P_leave[ReferendumResults_ava$Area_Groups=="G3"])),
       pch=16,col="red",cex=2)
dev.off()

# Scatterplots
pdf("Figure_4.1.pdf")
par(mfrow=c(2,2))
plot(ReferendumResults_ava$AdultMeanAge, ReferendumResults_ava$P_leave,
     xlab = "Mean age of adult permanent residents", 
     ylab = "Proportion of leave votes", pch = 16)
plot(ReferendumResults_ava$lower_mid_work_index, ReferendumResults_ava$P_leave,
     xlab = "Lower Mid Work Index", ylab = "Proportion of leave votes", 
     pch = 16)
plot(ReferendumResults_ava$White, ReferendumResults_ava$P_leave,
     xlab = "% of white permanent residents", 
     ylab = "Proportion of leave votes", pch = 16)
plot(ReferendumResults_ava$NoQuals, ReferendumResults_ava$P_leave,
     xlab = "% of permanent residents with no qualifications", 
     ylab = "Proportion of leave votes", pch = 16)
dev.off()

pdf("Figure_4.2.pdf")
par(mfrow=c(2,2))
plot(ReferendumResults_ava$Density, ReferendumResults_ava$P_leave,
     xlab = "Population density (permanent residents per hectare)",
     ylab = "Proportion of leave votes", pch = 16,col="darkblue")
plot(ReferendumResults_ava$soc_fin_index, ReferendumResults_ava$P_leave,
     xlab = "Social-financail-condition index", 
     ylab = "Proportion of leave votes", pch = 16,col="darkblue")
plot(ReferendumResults_ava$L4Quals_plus, ReferendumResults_ava$P_leave,
     xlab = "% of permanent residents with degree level or above",
     ylab = "Proportion of leave votes", pch = 16,
     col="darkblue")
plot(ReferendumResults_ava$PrivateRent, ReferendumResults_ava$P_leave,
     xlab = "% of households renting from private landlords", 
     ylab = "Proportion of leave votes", pch = 16,col="darkblue")
dev.off()
par(mfrow=c(1,1))


################################################################################
################################ Model Building ################################
################################################################################

###
### Step 6: The chosen of GLM, "c-log-logg" link, and quasibinomial family  
###

### 6.1 why GLM
predict_leave.lm1 <- lm(P_leave ~ 
                          AreaType + 	Postals  + 	Residents  + MeanAge+ AdultMeanAge
                        + White + Black + Asian + Indian +Pakistani+
                          + SocialRent+ PrivateRent
                        + Students + Density
                        + Area_Groups 
                        + Owned_Mort + Owned_No_Mort
                        + soc_fin_index +lower_mid_work_index    
                        + NoQuals+ L1Quals+ L4Quals_plus
                        +NVotes+ Households
                        +Age_un18+ Age_young_adults + Age_adults + Age_elderlys,
                        data=ReferendumResults_ava)
lm1.stdres <-rstandard(predict_leave.lm1)

pdf("Figure_5.pdf")
par(mfrow=c(1,1))
qqnorm(lm1.stdres, xlab="Quantiles of Standard Normal Distribution", ylab="Standardised Residuals",
       main="Normal Q-Q Plot of Residuals")
abline(a=0,b=1,col="blue")
plot(predict_leave.lm1,which=1,pch=16)
dev.off()
# the errors deviate from normal distribution and the variance is not constant
# so we choose GLM instead of simple linear model

### 6.2 why "c-log-log" link
predict_leave.glm1 <- glm(P_leave ~ 
                            AreaType + 	Postals  + 	Residents  + MeanAge+ AdultMeanAge
                          + White + Black + Asian + Indian +Pakistani+
                            + SocialRent+ PrivateRent
                          + Students + Density
                          + Area_Groups 
                          + Owned_Mort + Owned_No_Mort
                          + soc_fin_index +lower_mid_work_index    
                          + NoQuals+ L1Quals+ L4Quals_plus
                          + NVotes+ Households
                          + Age_un18+ Age_young_adults + Age_adults + Age_elderlys,
                          weights=NVotes,
                          data=ReferendumResults_ava,
                          family=binomial(link="cloglog") )

predict_leave.glm2<-update(predict_leave.glm1, family=binomial(link="probit"))
predict_leave.glm3<-update(predict_leave.glm1, family=binomial(link="logit"))
cat("\nThe Summary for three glms with different link functions \n")
print(summary(predict_leave.glm1)) #Residual deviance:  45872; AIC: 53098 
print(summary(predict_leave.glm2)) #Residual deviance:  45949; AIC: 53176
print(summary(predict_leave.glm3)) #Residual deviance:  45932; AIC: 53159
# "clog-log" link has the smallest AIC and Residual deviance

### 6.3 why quasibinomial family 

# check overdispersion
STDB.glm1<-rstandard(predict_leave.glm1)
fitted_glm1<-fitted(predict_leave.glm1)

pdf("Figure_6.1.pdf")
plot(fitted_glm1, STDB.glm1, pch=16, xlab = "Fitted Values",ylab="Standardised Residuals",
     main="Model with binomial family")
abline(0,0, col="blue")
abline(h=2, col="blue",lty=2) # Most points not in +-2 range, there exits overdispersion
abline(h=-2, col="blue",lty=2) 
dev.off()

# after using quasibinomial family
predict_leave.glm1_quasi<-update(predict_leave.glm1, family=quasibinomial(link="cloglog"))
STDB.glm1_quasi<-rstandard(predict_leave.glm1_quasi)
fitted_glm1_quasi<-fitted(predict_leave.glm1)

pdf("Figure_6.2.pdf")
plot(fitted_glm1_quasi, STDB.glm1_quasi, pch=16, xlab = "Fitted Values",ylab="Standardised Residuals",
     main="Model with quasibinomial family")
abline(0,0, col="blue")
abline(h=2, col="blue",lty=2)
abline(h=-2, col="blue",lty=2)
dev.off()

### 
### Step 7: Find and Add interactions
###

### Interactions between categorial and numerical covariates

# To make the difference in slope more clear, we choose similar colors for 
# the groups that behave similarly
library(ggplot2)

# Figure 7.1 NoQuals and Area_groups 
ggplot(ReferendumResults_ava, aes(x = NoQuals, y = P_leave, color = Area_Groups)) +
  geom_point() + scale_color_manual(values = c("grey","azure4","red"))+
  labs(x = "NoQuals", y = "The Proportion of Leave Votes") + theme_classic()
ggsave("Figure_7.1.pdf", device="pdf")

# Figure 7.2 Age_elderlys and Area_groups 
ggplot(ReferendumResults_ava, aes(x = Age_elderlys, y = P_leave, color = Area_Groups)) +
  geom_point()  + scale_color_manual(values = c("grey","darkblue","chocolate1"))+
  labs(x = "The proportion of elderly permanent residents", y = "The Proportion of Leave Votes") + 
  theme_classic() + geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_7.2.pdf", device="pdf")

# Figure 7.3 Age_un18 and Area_groups 
ggplot(ReferendumResults_ava, aes(x = Age_un18, y = P_leave, color = Area_Groups)) +
  geom_point()  + scale_color_manual(values = c("grey","azure4","red"))+
  labs(x = "The proportion of under 18 permanent residents", y = "The Proportion of Leave Votes")+
  theme_classic()
ggsave("Figure_7.3.pdf", device="pdf")

# Figure 7.4 Area_Groups and soc_fin_index 
ggplot(ReferendumResults_ava, aes(x = soc_fin_index, y = P_leave, color = Area_Groups)) +
  geom_point()   + scale_color_manual(values = c("grey","darkblue","chocolate1"))+
  labs(x = "Social status & financial condition index", y = "The Proportion of Leave Votes") + 
  theme_classic()  + geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_7.4.pdf", device="pdf")

# Figure 8.1 White and Area_Groups
ggplot(ReferendumResults_ava, aes(x = White, y = P_leave, color = Area_Groups)) +
  geom_point() +
  labs(x = "% of permanent residents self-identifying as white", y = "The Proportion of Leave Votes")
ggsave("Figure_8.1.pdf", device="pdf")

# Figure 8.2 Asian and Area_Groups
ggplot(ReferendumResults_ava, aes(x = Asian, y = P_leave, color = Area_Groups)) +
  geom_point() +
  labs(x = "% of permanent residents self-identifying as Asian", y = "The Proportion of Leave Votes")
ggsave("Figure_8.2.pdf", device="pdf")

# Figure 8.3 Indian and Area_Groups
ggplot(ReferendumResults_ava, aes(x = Indian, y = P_leave, color = Area_Groups)) +
  geom_point() +
  labs(x = "% of permanent residents self-identifying as Indian", y = "The Proportion of Leave Votes")
ggsave("Figure_8.3.pdf", device="pdf")

# Figure 8.4 Pakistani*AreaType
ggplot(ReferendumResults_ava, aes(x = Pakistani, y = P_leave, color = AreaType)) +
  geom_point() +
  labs(x = "% of permanent residents self-identifying as Pakistani", y = "The Proportion of Leave Votes")
ggsave("Figure_8.4.pdf", device="pdf")


### Interactions between numerical covariates

# Split the numerical covariates into 3 groups, from 0 to lower quantile, 
# lower quantile to upper quantile, and upper quantile to the maximum.
summary(ReferendumResults_ava$White)
ReferendumResults_ava$White_grouped <- cut(ReferendumResults_ava$White, breaks = c(0.00,72.00,96.15,99.70),
                                           include.lowest = TRUE)
summary(ReferendumResults_ava$Asian)
ReferendumResults_ava$Asian_grouped <- cut(ReferendumResults_ava$Asian, breaks = c(0.00,2.15,13.85,81.50),
                                           include.lowest = TRUE)
summary(ReferendumResults_ava$Black)
ReferendumResults_ava$Black_grouped<- cut(ReferendumResults_ava$Black, breaks = c(0.00,1.000,7.700,50.500),
                                          include.lowest = TRUE)
summary(ReferendumResults_ava$Indian)
ReferendumResults_ava$Indian_grouped<- cut(ReferendumResults_ava$Indian, breaks = c(0.00,0.50,3.30,47.70),
                                           include.lowest = TRUE)
summary(ReferendumResults_ava$Pakistani)
ReferendumResults_ava$Pakistani_grouped<- cut(ReferendumResults_ava$Pakistani, breaks = c(0.00,0.200,2.000,72.300),
                                              include.lowest = TRUE )

# Figure 9.1 Asian and soc_fin_index
ggplot(ReferendumResults_ava, aes(x = soc_fin_index, y = P_leave, color = Asian_grouped)) +
  geom_point() +
  scale_color_manual(values = c("chocolate1","cadetblue2","black"))+
  labs(x = "soc_fin_index", y = "The Proportion of Leave Votes") +
  theme_classic()+geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_9.1.pdf", device="pdf")

# Figure 9.2 White and soc_fin_index
ggplot(ReferendumResults_ava, aes(x = soc_fin_index, y = P_leave, color = White_grouped)) +
  geom_point() +  scale_color_manual(values = c("chocolate1","cadetblue2","black"))+
  labs(x = "soc_fin_index", y = "The Proportion of Leave Votes") + theme_classic()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_9.2.pdf", device="pdf")

# Figure 9.3 White and Owned_No_Mort 
ggplot(ReferendumResults_ava, aes(x = Owned_No_Mort, y = P_leave, color = White_grouped)) +
  geom_point() +
  scale_color_manual(values = c("chocolate1","cadetblue2","black"))+
  labs(x = "Owned_No_Mort", y = "The Proportion of Leave Votes") +
  theme_classic()+geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_9.3.pdf", device="pdf")

# Figure 9.4 White and Residents
ggplot(ReferendumResults_ava, aes(x = Residents, y = P_leave, color = White_grouped)) +
  geom_point() +
  scale_color_manual(values = c("chocolate1","cadetblue2","black"))+
  labs(x = "Residents", y = "The Proportion of Leave Votes") +
  theme_classic()+geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_9.4.pdf", device="pdf")

# Figure 10.1 Asian and L4Quals_plus
ggplot(ReferendumResults_ava, aes(x = L4Quals_plus, y = P_leave, color = Asian_grouped)) +
  geom_point() +
  scale_color_manual(values = c("grey","deepskyblue4","red"))+
  labs(x = "L4Quals_plus", y = "The Proportion of Leave Votes") + 
  theme_classic()+geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_10.1.pdf", device="pdf")

# Figure 10.2 White and L4Quals_plus
ggplot(ReferendumResults_ava, aes(x = L4Quals_plus, y = P_leave, color = White_grouped)) +
  geom_point() +
  scale_color_manual(values = c("red","deepskyblue4","grey"))+
  labs(x = "L4Quals_plus", y = "The Proportion of Leave Votes") + 
  theme_classic()+ theme_classic()+geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_10.2.pdf", device="pdf")

# Figure 10.3 MeanAge and L4Quals_plus
summary(ReferendumResults_ava$MeanAge)
ReferendumResults_ava$MeanAge_grouped <- cut(ReferendumResults_ava$MeanAge, 
                                             breaks = c(0,35.70,41.40,47.30),
                                             include.lowest = TRUE)

ggplot(ReferendumResults_ava, aes(x = L4Quals_plus, y = P_leave, color = MeanAge_grouped)) +
  geom_point() + scale_color_manual(values = c("red","deepskyblue4","grey"))+
  labs(x = "L4Quals_plus", y = "The Proportion of Leave Vote") + theme_classic()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_10.3.pdf", device="pdf")

# Figure 10.4 NoQuals and Students
summary(ReferendumResults_ava$Students)
ReferendumResults_ava$Students_grouped <- cut(ReferendumResults_ava$Students, 
                                              breaks = c(0,2.900,6.350,63.900),
                                              include.lowest = TRUE)

ggplot(ReferendumResults_ava, aes(x = NoQuals, y = P_leave, color = Students_grouped)) +
  geom_point()+ scale_color_manual(values = c("grey","deepskyblue4","red"))+
  labs(x = "NoQuals", y = "The Proportion of Leave Votes") + theme_classic()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("Figure_10.4.pdf", device="pdf")


###
### Step 8: The development of draft models and Final model
###

### 8.1 The first draft model with all covariates involved
draft_predict_leave.glm1 <- glm(P_leave ~ 
                           AreaType + 	Postals  + 	Residents  + MeanAge+ AdultMeanAge
                         + White + Black + Asian + Indian +Pakistani+
                           + SocialRent+ PrivateRent
                         + Students + Density
                         + Area_Groups 
                         + Owned_Mort + Owned_No_Mort
                         + soc_fin_index +lower_mid_work_index    
                         + NoQuals+ L1Quals+ L4Quals_plus
                         +NVotes+ Households
                         +Age_un18+ Age_young_adults + Age_adults + Age_elderlys
                         , 
                         weights=NVotes,
                         data=ReferendumResults_ava,
                         family=quasibinomial(link="cloglog") )
cat("\nThe summary for our first draft prediction model (draft_predict_leave.glm1) \n")
print(summary(draft_predict_leave.glm1))

### 8.2 The second draft model with all covariates and interactions added
draft_predict_leave.glm2<-update(draft_predict_leave.glm1,.~.

                #Add interactions:
                + Area_Groups*NoQuals
                + Area_Groups*Age_un18 
                + Area_Groups*Age_elderlys  
                + Area_Groups*soc_fin_index
                
                + White*Area_Groups
                + Asian*Area_Groups 
                + Indian*Area_Groups
                + Pakistani*AreaType
                
                + Asian*soc_fin_index
                + White*soc_fin_index
                + White*Owned_No_Mort 
                + White*Residents
                
                + Asian*L4Quals_plus 
                + White*L4Quals_plus
                + MeanAge*L4Quals_plus
                + NoQuals*Students)

cat("\nThe summary for our model after adding all the interactions (draft_predict_leave.glm2) \n")
print(summary(draft_predict_leave.glm2))

### 8.3 Use anova to try removing the covariates with a p-value over 0.05 and 
### no presence in interactions at the same time
draft_predict_leave.glm3<-update(draft_predict_leave.glm2,.~.
                                 -Postals
                                 -Households
                                 -Age_young_adults -Age_adults)
cat("\nThe summary for draft_predict_leave.glm3 \n")
print(summary(draft_predict_leave.glm3))
cat("\nANOVA test between draft_predict_leave.glm3 and draft_predict_leave.glm2 \n")
print(anova(draft_predict_leave.glm3,draft_predict_leave.glm2,test = "F"))
cat("p-value=0.7878(>0.05), the simpler model is preferred \n")

draft_predict_leave.glm4<-update(draft_predict_leave.glm3,.~.
                                 - L1Quals -Owned_Mort) 
cat("\nThe summary for draft_predict_leave.glm4 \n")
print(draft_predict_leave.glm4)
cat("\nANOVA test between draft_predict_leave.glm4 and draft_predict_leave.glm3 \n")
print(anova(draft_predict_leave.glm4,draft_predict_leave.glm3,test = "F"))
cat("\np-value=0.1323(>0.05), the simpler model is preferred \n")

draft_predict_leave.glm5<-update(draft_predict_leave.glm4,.~.
                                 -PrivateRent-SocialRent)
cat("\nThe summary for draft_predict_leave.glm5 \n")
print(summary(draft_predict_leave.glm5))
cat("\nANOVA test between draft_predict_leave.glm5 and draft_predict_leave.glm4 \n")
print(anova(draft_predict_leave.glm5,draft_predict_leave.glm4,test = "F"))
cat("\np-value=0.1073(>0.05), the simpler model is preferred \n")


# After deleting all the interactions with a relatively large p-value
# one by one and checking the anova test, we find none of these interactions
# can be deleted.
# draft_predict_leave.glm6<-update(draft_predict_leave.glm5,.~.
                            #-Area_Groups*Age_elderlys +Area_Groups+Age_elderlys) 
# anova(draft_predict_leave.glm6,draft_predict_leave.glm5,test = "F")

# draft_predict_leave.glm6<-update(draft_predict_leave.glm5,.~.
                          #-Area_Groups*Age_un18+Area_Groups+Age_un18) 
# anova(draft_predict_leave.glm6,draft_predict_leave.glm5,test = "F")

# draft_predict_leave.glm6<-update(draft_predict_leave.glm5,.~.
                          #-Indian*Area_Groups+Indian+Area_Groups) 
# anova(draft_predict_leave.glm6,draft_predict_leave.glm5,test = "F")

# draft_predict_leave.glm6<-update(draft_predict_leave.glm5,.~.
                          #-AreaType*Pakistani+AreaType+Pakistani) 
# anova(draft_predict_leave.glm6,draft_predict_leave.glm5,test = "F")


### 8.4 Final Model
final_predict_leave<-draft_predict_leave.glm5
cat("\nThe summary for our final model (final_predict_leave)  \n")
print(summary(final_predict_leave))
# Some of the covariates still has a p-value greater than 0.05, 
# that's mainly because they are involved with an interaciton,
# and we choose to maintain it.


### 8.5 Final Model Checking

# The diagnostic plots:
STDB<-rstandard(final_predict_leave)
fitted_predict<-fitted(final_predict_leave)
pdf("Figure_11.pdf")
par(mfrow=c(2,2))

plot(fitted_predict, STDB, pch=16, 
     xlab = "Fitted Values",ylab="Standardised Residuals")
abline(0,0, col="blue")
abline(h=2, col="blue",lty=2)
abline(h=-2, col="blue",lty=2)

plot(final_predict_leave,which=2)
plot(fitted_predict, ReferendumResults_ava$P_leave, 
     xlab="Fitted Value of P_leave", ylab="Observed Value of P_leave",pch=16)
abline(0,1,col="blue")
plot(final_predict_leave,which=4)
dev.off()

# Influential observations
cook <- cooks.distance(final_predict_leave)
list(cook[cook>0.01317957]) # 8/(n-2*k)=8/(803-4*49)=0.01317957
influencial_data<-ReferendumResults_ava[c(2,13,85 ,107,157,235,240,
                                          303,318,353, 419,429,481,538,618,
                                          650,780),]
table(influencial_data$AreaType) # show the locations of these influential obervations
table(influencial_data$Area_Groups) 
# these influential observations distribute over different regions 
# and share no noticeable similarities


################################################################################
############################### The predictions ################################
################################################################################

ReferendumResults_miss<-ReferendumResults_new[804:1070,] 
predicted_P_leave<-predict(final_predict_leave, type='response', newdata=ReferendumResults_miss, se.fit=TRUE)
cat("\nPredicted proportion of Leave votes for the rest 267 wards \n")
print(predicted_P_leave$fit) #predicted P_leave
cat("\nStandard Error for Predictions \n")
print(predicted_P_leave$se.fit) 

# calculate the SD of prediction error
var_P_hati<-(predicted_P_leave$se.fit)^(2)
#the dispersion parameter of our final model is 41.82193
var_Yi<-(41.82193*predicted_P_leave$fit*(1-predicted_P_leave$fit))/ReferendumResults_miss$NVotes
sd_predict_error<-(var_Yi+var_P_hati)^(1/2)
print(sd_predict_error) 

# output the required file
final_output<-data.frame(ID =ReferendumResults_miss$ID,
                  Predicted_Proportion_of_Leave_Votes=predicted_P_leave$fit,
                  Prediction_Error_SD=sd_predict_error)
cat("\nDataframe for ID, Predicted Value, and Prediction Error SD \n")
print(final_output)#show the final required file

# export the required data file with no headers
write.table(final_output, file="ICA2_Group_I_pred.dat", 
            append = FALSE, sep = " ",
            row.names = FALSE, col.names = FALSE)


