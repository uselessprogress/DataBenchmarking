load("MasterData.Rda")

pha_dib <- dat %>% 
           unnest(PHA) %>% 
           mutate(Diabetes = ifelse(Q1_Diabetes=="N",0,1)) %>% 
           select(Diabetes,MemberNumber)

pha_bp <- dat %>% 
           unnest(PHA) %>% 
           mutate(Hypertension = ifelse(Q1_HighBloodPressure=="N",0,1)) %>% 
           select(Hypertension,MemberNumber)


pha_lip <- dat %>% 
           unnest(PHA) %>% 
           mutate(Hyperlipidemia = ifelse(Q1_HighCholesterol=="N",0,1)) %>% 
           select(Hyperlipidemia,MemberNumber)



cla_dib <- dat %>% 
           unnest(Claims) %>% 
           mutate(Diabetes = ifelse(Diabetes=="Yes",1,0)) %>% 
           select(Diabetes,MemberNumber)


cla_bp <- dat %>% 
  unnest(Claims) %>% 
  mutate(Hypertension = ifelse(Hypertension=="Yes",1,0)) %>% 
  select(Hypertension,MemberNumber)



cla_lip <- dat %>% 
  unnest(Claims) %>% 
  mutate(Hyperlipidemia = ifelse(Hyperlipidemia=="Yes",1,0)) %>% 
  select(Hyperlipidemia,MemberNumber)






