setwd("F:/Dropbox/stoneDS1817/2021년_감명환_데이터")


#### 자료 읽기  #####
library(dplyr)

rawdata = read.csv("karmdata.csv", header = T)
summary(rawdata)



# karmdata = filter(rawdata,OP_Code > 0)
karmdata = rawdata

### factor 자료 설정 ###

karmdata$Dx_Code <- factor(karmdata$Dx_Code,levels=c(1,2,3,4,5,6),labels=c("Mn. Prognathism", "Mn. Retrognathism", "Facial Asymmetry", "OSA", "Bimaxillary Protrusion", "Syndrome(Exclusion)"))
karmdata$OP_Code <- factor(karmdata$OP_Code,levels=c(1,2,3,4,5),labels=c("LFI+BSSO", "LFI+BSSO+GN", "LFI+BSSO+Segmentation", "LFI+BSSO+Segmentation+GN", "ASO"))

karmdata$Genioplasty <- factor(karmdata$Genioplasty, levels=c(0,1), labels =c("No","Yes"))
karmdata$Segmentation <- factor(karmdata$Segmentation, levels=c(0,1), labels =c("No","Yes"))
karmdata$Turbinectomy <- factor(karmdata$Turbinectomy, levels=c(0,1), labels =c("No","Yes"))
karmdata$Ext <- factor(karmdata$Ext, levels=c(0,1), labels =c("No","Yes"))
karmdata$Grinding <- factor(karmdata$Grinding, levels=c(0,1), labels =c("No","Yes"))
karmdata$Glossectomy <- factor(karmdata$Glossectomy, levels=c(0,1), labels =c("No","Yes"))
karmdata$IBG <- factor(karmdata$IBG, levels=c(0,1), labels =c("No","Yes"))
karmdata$Face_Lift <- factor(karmdata$Face_Lift, levels=c(0,1), labels =c("No","Yes"))

karmdata$Pre_Medical_Hx <- factor(karmdata$Pre_Medical_Hx, levels=c(0,1,2,3,4,5,6,7,8,9,10), labels =c("No", "Cardiovascular disease", "pulmonologic disease", "Hematologic disease", "endocrine disease", "Hepatologic disease", "psychologic disease", "Genetic disease", "neurologic disease", "Allergy", "other"))

karmdata$PMH_1 <- factor(karmdata$PMH_1, levels=c(0,1), labels =c("No","Cardiovascular disease"))
karmdata$PMH_2 <- factor(karmdata$PMH_2, levels=c(0,1), labels =c("No","pulmonologic disease"))
karmdata$PMH_3 <- factor(karmdata$PMH_3, levels=c(0,1), labels =c("No","Hematologic disease"))
karmdata$PMH_4 <- factor(karmdata$PMH_4, levels=c(0,1), labels =c("No","endocrine disease"))
karmdata$PMH_5 <- factor(karmdata$PMH_5, levels=c(0,1), labels =c("No","Hepatologic disease"))
karmdata$PMH_6 <- factor(karmdata$PMH_6, levels=c(0,1), labels =c("No","psychologic disease"))
karmdata$PMH_7 <- factor(karmdata$PMH_7, levels=c(0,1), labels =c("No","Genetic disease"))
karmdata$PMH_8 <- factor(karmdata$PMH_8, levels=c(0,1), labels =c("No","neurologic disease"))
karmdata$PMH_9 <- factor(karmdata$PMH_9, levels=c(0,1), labels =c("No","Allergy"))
karmdata$PMH_10 <- factor(karmdata$PMH_10, levels=c(0,1), labels =c("No","other"))

karmdata$Pre_OP_Hx <- factor(karmdata$Pre_OP_Hx, levels=c(0,1,2,3,4,5,6,7,8), labels =c("No","orthgnathic surgery", "Cleft op", "orofacial op", "chest op", "abdomen op ", "Limbs op", "spine op", "other"))

karmdata$Pre_OP_1 <- factor(karmdata$Pre_OP_1, levels=c(0,1), labels =c("No","orthgnathic surgery"))
karmdata$Pre_OP_2 <- factor(karmdata$Pre_OP_2, levels=c(0,1), labels =c("No","Cleft op"))
karmdata$Pre_OP_3 <- factor(karmdata$Pre_OP_3, levels=c(0,1), labels =c("No","orofacial op"))
karmdata$Pre_OP_4 <- factor(karmdata$Pre_OP_4, levels=c(0,1), labels =c("No","chest op"))
karmdata$Pre_OP_5 <- factor(karmdata$Pre_OP_5, levels=c(0,1), labels =c("No","abdomen op "))
karmdata$Pre_OP_6 <- factor(karmdata$Pre_OP_6, levels=c(0,1), labels =c("No","Limbs op"))
karmdata$Pre_OP_7 <- factor(karmdata$Pre_OP_7, levels=c(0,1), labels =c("No","spine op"))
karmdata$Pre_OP_8 <- factor(karmdata$Pre_OP_8, levels=c(0,1), labels =c("No","other"))

karmdata$Allergy_Hx <- factor(karmdata$Allergy_Hx, levels=c(0,1), labels =c("No","Yes"))

karmdata$Sex  <- factor( karmdata$Sex ,levels=c(1,2),labels=c("Male","Female"))
karmdata$ASA  <- factor( karmdata$ASA ,levels=c(1,2,3,4),labels=c("ASA 1","ASA 2","ASA 3","ASA 4"))

karmdata$PreOP_Blood_Type_N <- factor(karmdata$PreOP_Blood_Type_N, levels=c(1,2,3,4), labels =c("A","B","AB","O"))

karmdata$TFg <- factor(karmdata$TF, levels=c(0,1,2,3,4,5,6,7))
karmdata$TF_B <- factor(karmdata$TF_B, levels=c(0,1), labels =c("No","Yes"))

karmdata$group <- karmdata$TF_B
karmdata$Anes <- factor(karmdata$Anes, levels=c(1,2,3), labels =c("TIVA","Sevo","Des"))

summary(karmdata)



library(gmodels)

TFtable=with(karmdata,table(group,OP_Code))
TFtable


chisq.test(TFtable)

with(karmdata,CrossTable(group,OP_Code,expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                        chisq = TRUE, fisher=TRUE ))


########## 두 그룹에 영향을 끼치는 요소의 동질성  분석 ##########
library(tableone)
library(ggplot2)


pt_charvar <-c("ASA","Sex","Age","Height","Weight","BMI","Pre_Medical_Hx", "PMH_1", "PMH_2", "PMH_3", "PMH_4", "PMH_5", "PMH_6", "PMH_7", "PMH_8", "PMH_9", "PMH_10",
"Pre_OP_Hx", "PreOP_SBP", "PreOP_DBP", "PreOP_HR", "PreOP_Temp")
op_charvar <-c("Dx_Code","OP_Code","Genioplasty", "Segmentation", "Turbinectomy", "Ext", "Grinding", "Glossectomy", "IBG", "Face_Lift", "N_Adjun_Sx")
anes_charvar <-c("To_Ane_Ti_min", "To_Surg_Ti_min", "Anes", "Anes_B", "OR_Propofol", "OR_RemiF", "OR_Thiopental", "OR_Esmerone", "OR_Vecuronium", "OR_Cis_atracurium", 
                 "OR_Dopamine", "OR_Nicardifine", "OR_HS", "OR_PSA", "OR_NS", "OR_Crystalloid", "OR_Volulyte", "OR_D5W", "OR_Total", "OR_Fluid", "OR_Blood_Loss", "OR_UO")

preop_charvar <-c("PreOP_Hb", "PreOP_Hct", "PreOP_Platelet", "PreOP_aPTT", "PreOP_INR", "PreOP_PT", "PreOP_Total_Protein", "PreOP_Albumin",
                  "PreOP_Total_Bilirubin", "PreOP_Alk_Phosphatase", "PreOP_AST", "PreOP_ALT", "PreOP_Creatinine")


TFvar <-c( "AutoTF", "TF", "TFg")
ABGA_var  <- c( "ABGA1_pH", "ABGA2_pH", "ABGA3_pH", "ABGA4_pH", "ABGA5_pH", "ABGA1_Hb", "ABGA2_Hb", "ABGA3_Hb", "ABGA4_Hb", "ABGA5_Hb", 
                    "ABGA1_Hct", "ABGA2_Hct", "ABGA3_Hct", "ABGA4_Hct", "ABGA5_Hct")

vital_var <-c("mPVI", "mCompliance", "mBIS", "mSpO2", "mEtCO2", "mPIP", "mPulse", "mTemp", "mA_Syst", "mA_Diast", "mA_Mean")



var_intrest <- c(TFvar)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)




var_intrest <- c(pt_charvar)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)



var_intrest <- c(op_charvar)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)


with(karmdata,CrossTable(group,OP_Code,expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                         chisq = TRUE, fisher=TRUE ))



with(karmdata,CrossTable(group,Genioplasty,expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                         chisq = TRUE, fisher=TRUE ))


with(karmdata,CrossTable(group,Segmentation,expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                         chisq = TRUE, fisher=TRUE ))


mosaicplot(karmdata$group~karmdata$Genioplasty)

mosaicplot(~  Genioplasty +Segmentation + group, data = karmdata, color = TRUE)

model <- glm(group ~Genioplasty +Segmentation,family=binomial(link='logit'),data = karmdata)

summary(model)


mosaicplot(~  Genioplasty +Segmentation +Ext + group, data = karmdata, color = TRUE)

model <- glm(group ~Genioplasty +Segmentation+ Ext,family=binomial(link='logit'),data = karmdata)

summary(model)



var_intrest <- c(pt_charvar,op_charvar, preop_charvar,anes_charvar )

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "Ext", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)



with(karmdata,CrossTable(Ext, OP_Code,    expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                         chisq = TRUE, fisher=TRUE ))

karmdataext = filter(rawdata,OP_Code < 3)
with(karmdataext,CrossTable(Ext, OP_Code,    expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                         chisq = TRUE, fisher=TRUE ))


karmdataext2 = filter(rawdata,OP_Code >2 )
with(karmdataext2,CrossTable(Ext, OP_Code,    expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                            chisq = TRUE, fisher=TRUE ))




var_intrest <- c(anes_charvar)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)



model <- glm(group ~ To_Ane_Ti_min +  To_Surg_Ti_min + OR_HS +  OR_PSA + OR_Crystalloid + OR_Volulyte + OR_Total +  OR_Blood_Loss + OR_UO,family=binomial(link='logit'),data = karmdata)

summary(model)

step_vs <- step(model, direction = "backward")

model <- glm(group ~ OR_Total+ OR_Blood_Loss + To_Surg_Ti_min ,family=binomial(link='logit'),data = karmdata)

summary(model)

step_vs <- step(model, direction = "backward")




var_intrest <- c(preop_charvar)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)

library(ggplot2)


density(group~PreOP_INR)

ggplot(data = karmdata) +
    geom_density(mapping=aes(x=PreOP_INR, colour = group))

ggplot(data = karmdata) +
  geom_density(mapping=aes(x=PreOP_PT, colour = group))

ggplot(data = karmdata) +
  geom_density(mapping=aes(x=PreOP_Hb, colour = group))

ggplot(data = karmdata) +
  geom_density(mapping=aes(x=To_Surg_Ti_min, colour = group))

ggplot(data = karmdata, aes(x=To_Surg_Ti_min, y = group,colour = group)) +
  geom_point()

ggplot(data = karmdata, aes(x=To_Surg_Ti_min, y = group,colour = group)) +
  geom_boxplot()

ggplot(data = karmdata) +
  geom_density(mapping=aes(x=Age, colour = group))


model <- glm(group ~ Age + BMI,family=binomial(link='logit'),data = karmdata)

summary(model)




var_intrest <- c(ABGA_var)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)


var_intrest <- c(vital_var)

Tablegroup <- CreateTableOne(vars = var_intrest, strata = "group", data = karmdata, includeNA = TRUE, test = TRUE)
print(Tablegroup, smd = TRUE)
summary(Tablegroup)


################################################




logistic_fit = glm(group ~ PreOP_Hb + To_Surg_Ti_min + Grinding, data =karmdata, family = binomial)

summary(logistic_fit)

### odds ratio #####

exp(coef(logistic_fit))
exp(confint(logistic_fit))


#################

library(rpart)

library(rpart.plot)

tree <- rpart(group ~ PreOP_Hb, data = karmdata,  method = "anova")
 rpart.plot(tree)

 tree <- rpart(group ~ PreOP_Hb, data = karmdata,  method = "exp")
 rpart.plot(tree)
 


karmdataHb= filter(rawdata,OP_Code < 3)
with(karmdataext,CrossTable(Ext, OP_Code,    expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                            chisq = TRUE, fisher=TRUE ))



karmdata$Hbgroup <- ifelse(karmdata$PreOP_Hb < 12, 0, 1)

karmdata$Hbgroup

karmdata$Hbgroup <- factor(karmdata$Hbgroup, levels=c(0,1), labels =c("<12",">=12"))

karmdata$Hbgroup

with(karmdata,CrossTable(group, Hbgroup,    expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                         chisq = TRUE, fisher=TRUE ))

OR_Blood_Loss

tree <- rpart(group ~ PreOP_Hb + OR_Blood_Loss, data = karmdata,  method = "class")
rpart.plot(tree)


tree <- rpart(group ~ PreOP_Hb + Genioplasty , data = karmdata,  method = "class")
rpart.plot(tree)

tree <- rpart(group ~ PreOP_Hb + Segmentation  , data = karmdata,  method = "class")
rpart.plot(tree)

tree <- rpart(group ~ PreOP_Hb + Grinding  , data = karmdata,  method = "anova")
rpart.plot(tree)


#####################
library(ggplot2)
library(pROC)
library(MASS)

theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(karmdata, aes(PreOP_Hb)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=group), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

g <- ggplot(karmdata, aes(OR_Blood_Loss)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=group), 
                   binwidth =50, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  



# Histogram on a Continuous (Numeric) Variable
g <- ggplot(karmdata, aes(To_Surg_Ti_min)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=group), 
                   binwidth = 10, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(karmdata, aes(OR_Crystalloid)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=group), 
                   binwidth = 100, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  



# Histogram on a Continuous (Numeric) Variable
g <- ggplot(karmdata, aes(OR_Volulyte)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=group), 
                   binwidth = 100, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  


# Histogram on a Continuous (Numeric) Variable
g <- ggplot(karmdata, aes(PreOP_INR)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=group), 
                   binwidth = 0.05, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  



gp <- ggplot(data=karmdata, # 데이터 입력            
             aes(x=PreOP_Hb, y=To_Surg_Ti_min))+   # x,y 축 입력
  geom_point(   # 그래프 종류 입력
    aes(color=group, shape = group ),   # 그래프  모양 입력  
    alpha=1,  # 투명도 입력   
    size=4)  # 크기 입력
gp


gp <- ggplot(data=karmdata, # 데이터 입력            
             aes(x=PreOP_Hb, y=OR_Blood_Loss))+   # x,y 축 입력
  geom_point(   # 그래프 종류 입력
    aes(color=group, shape = group ),   # 그래프  모양 입력  
    alpha=1,  # 투명도 입력   
    size=4)  # 크기 입력
gp



mosaicplot(karmdata$group ~ karmdata$Grinding, col =c("red","blue"))

mosaicplot(karmdata$OP_Code ~ karmdata$group, col =c("red","blue"))

plot(density(karmdata$PreOP_Hb))

hb_roc <- roc(karmdata$group, karmdata$PreOP_Hb)
plot.roc(hb_roc)


plot.roc(hb_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   # 기준치(cut-off value)에 대한 출력, 포인트, 색상을 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 


plot(density(karmdata$To_Surg_Ti_min))

time_roc <- roc(karmdata$group, karmdata$To_Surg_Ti_min)
plot.roc(time_roc)


plot.roc(time_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   # 기준치(cut-off value)에 대한 출력, 포인트, 색상을 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 


tree <- rpart(group ~ PreOP_Hb + To_Surg_Ti_min , data = karmdata,  method = "anova")
rpart.plot(tree)


plot(density(karmdata$OR_Volulyte))

hist(karmdata$OR_Volulyte)

volulite_roc <- roc(karmdata$group, karmdata$OR_Volulyte)
plot.roc(volulite_roc)


plot.roc(volulite_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   # 기준치(cut-off value)에 대한 출력, 포인트, 색상을 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 


tree <- rpart(group ~ OR_Volulyte , data = karmdata,  method = "anova")
rpart.plot(tree)

colnames(karmdata)


plot(density(karmdata$PreOP_INR))

hist(karmdata$PreOP_INR)

INR_roc <- roc(karmdata$group, karmdata$PreOP_INR)
plot.roc(INR_roc)


plot.roc(INR_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   # 기준치(cut-off value)에 대한 출력, 포인트, 색상을 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 


tree <- rpart(group ~ PreOP_INR , data = karmdata,  method = "anova")
rpart.plot(tree)


library(ggplot2)

