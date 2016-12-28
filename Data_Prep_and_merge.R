library(readxl)

#import data

naChars <- c(""," ","NA","na","Na","nA","NA","n/a","N/A")
pha <- read.csv("data/PHA_Detail_2016.csv",skip=6,na=naChars)
bio <- read.csv("data/Biometric_Detail_2016.csv",skip=0,na=naChars)
pw <- read.csv("data/PW_ELIG_Current.csv",skip=0,na=naChars)
historicalAHSN <- read_excel("data/Historical_AHSN.xls", sheet=1)
diabetes <- read_excel("data/Diabetes_Registry.xlsx", sheet=1, skip=13)
hyperlipidemia <- read_excel("data/Hyperlipidemia_Registry.xlsx", sheet=1, skip=13)
hypertension <- read_excel("data/Hypertension_Registry.xlsx", sheet=1, skip=13)


# format biometric data

keep <- c("MemberNumber","Base_Weight","Base_BMI","BaseWaist","BaseBPSystolic",
          "BaseBPDiastolic","BaseBloodGlucose","BaseFastestBloodGlucose","BaseTotal_Chol",
          "BaseLDLChol","BaseHDLChol","BaseTriglycerides","BaseHemoglobin")
bio <- bio[,names(bio) %in% keep]
bio$MemberNumber <- toupper(bio$MemberNumber)

# format PHA

pha <- pha[,c(9,38:69)]
pha$MemberNumber <- toupper(pha$MemberNumber)
pha <- ifelse(is.na(pha) | pha == "N",0,1)
pha <- data.frame(pha)


# format claims data


diabetes <- data.frame("SSN"=substr(diabetes$`Individual ID`,0,9), 
                       "DOB"= as.Date(substr(diabetes$`Individual ID`,10,9999),"%Y%m%d"),
                       "Diabetes"=1,
                       "DiabetesOnset"= as.Date(diabetes$`Onset Date`,"%m/%d/%Y"))


hyperlipidemia <- data.frame("SSN"=substr(hyperlipidemia$`Individual ID`,0,9),
                             "DOB"= as.Date(substr(hyperlipidemia$`Individual ID`,10,9999),"%Y%m%d"),
                             "Hyperlipidemia"=1,
                             "HyperlipidemiaOnset"= as.Date(hyperlipidemia$`Onset Date`,"%m/%d/%Y"))



hypertension <- data.frame("SSN"=substr(hypertension$`Individual ID`,0,9),
                           "DOB"= as.Date(substr(hypertension$`Individual ID`,10,9999),"%Y%m%d"),
                           "Hypertension"=1,
                           "HypertensionOnset"= as.Date(hypertension$`Onset Date`,"%m/%d/%Y"))

claimslist <- c(as.character(diabetes$SSN),as.character(hyperlipidemia$SSN),as.character(hypertension$SSN))
doblist <- c(diabetes$DOB,hyperlipidemia$DOB,hypertension$DOB)
claims <- data.frame("id"=paste0(claimslist,doblist), "SSN"= claimslist,"DOB" = doblist)
claims <- claims[!duplicated(claims$id),]

claims <- merge(claims, diabetes, by=c("SSN","DOB"), all.x=TRUE)
claims <- merge(claims, hyperlipidemia, by=c("SSN","DOB"), all.x=TRUE)
claims <- merge(claims, hypertension, by=c("SSN","DOB"), all.x=TRUE)
claims <- claims[,names(claims) != "id"]

claims[,3] <- ifelse(is.na(claims[,3])==TRUE,0,1)
claims[,5] <- ifelse(is.na(claims[,5])==TRUE,0,1)
claims[,7] <- ifelse(is.na(claims[,7])==TRUE,0,1)

rm(claimslist, hyperlipidemia, hypertension, diabetes)

# Create nested dataframe with bio and pha information

participants <- c(as.character(pha$MemberNumber),as.character(bio$MemberNumber))
participants <- toupper(unique(participants))


dat <- data.frame("MemberNumber"=participants)
dat <- merge(dat, pha, by="MemberNumber", all.x=TRUE)

dat <- dat %>% group_by(MemberNumber) %>% nest()
names(dat) <- c("MemberNumber","PHA")

dat2 <- data.frame("MemberNumber"=participants)
dat2 <- merge(dat2, bio, by="MemberNumber", all.x=TRUE)
dat2 <- dat2 %>% group_by(MemberNumber) %>% nest()
names(dat2) <- c("MemberNumber","BIO")

dat <- merge(dat,dat2, by="MemberNumber", all.x=TRUE)



# Match claims data with MemberNumber


pw <- data.frame("MemberNumber"=toupper(pw$ELIG_AHSN),"SSN"=pw$SSN, "DOB"=as.Date(pw$DOB,"%m/%d/%Y"))

historicalAHSN <- data.frame("MemberNumber"=toupper(historicalAHSN$`AHSN#`),
                             "SSN"=as.character(gsub("-","",historicalAHSN$`Social Number`)),
                             "DOB"= as.Date(historicalAHSN$`Birth Date`,"%m/%d/%Y"))

mem <- rbind(pw,historicalAHSN)
mem <- mem[!duplicated(mem$SSN),]



claims <- merge(claims, mem, by=c("SSN","DOB"), all.x=TRUE)

claims <- claims[is.na(claims$MemberNumber)==FALSE,]

 



dat3 <- data.frame("MemberNumber"=participants)
dat3 <- merge(dat3, claims, by="MemberNumber", all.x=TRUE)
dat3 <- dat3 %>% group_by(MemberNumber) %>% nest()
names(dat3) <- c("MemberNumber","Claims")


dat <- merge(dat,dat3, by="MemberNumber", all.x=TRUE)


save(dat,file="MasterData.Rda")
