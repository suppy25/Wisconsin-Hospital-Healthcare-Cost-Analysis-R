library(readxl)
# First we read the given csv hospital file and mount the table
hosp<-read_xlsx("d:/dataset/HospitalCosts.xlsx")
hosp

#1.To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure.
hist(hosp$AGE,main = "Frequency of patients",col = "aquamarine",xlab =
       "Age")

attach(hosp)
AGE<-as.factor(AGE)
summary(AGE)

# Aggregate function is used to add the expenditure from each age and then max function used to find highest costs.
aggregate(TOTCHG~AGE,FUN=sum,data = hosp)

max(aggregate(TOTCHG~AGE,FUN=sum,data=hosp))

#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
hist(APRDRG,col = "cyan1",main = "Frequency of Treatments",xlab = "Trea
tment Categories")

APRDRG_fact<-as.factor(hosp$APRDRG)
summary(APRDRG_fact)

which.max(summary(APRDRG_fact))

df<-aggregate(TOTCHG~APRDRG,FUN = sum,data=hosp)
df

df[which.max(df$TOTCHG),]

#3. To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
hosp<-na.omit(hosp)#first we remove "NA"values
hosp$RACE<-as.factor(hosp$RACE)
model_aov<-aov(TOTCHG~RACE,data = hosp)
model_aov#ANOVA RESULTS

summary(model_aov)

summary(hosp$RACE)

#4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.
hosp$FEMALE<-as.factor(hosp$FEMALE)
model_lm4<-lm(TOTCHG~AGE+FEMALE,data = hosp)#calling Regression funtion
summary(model_lm4)
summary(hosp$FEMALE)#comparing genders

#5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
hosp$RACE<-as.factor(hosp$RACE)
model_lm5<-lm(LOS~AGE+FEMALE+RACE,data = hosp)
summary(model_lm5)

#6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.
model_lm6<-lm(TOTCHG~AGE+FEMALE+RACE+LOS+APRDRG,data = hosp)
summary(model_lm6)

