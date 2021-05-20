
#For changing village name:
#1. change Deg_ variable
#2. change conn_ variable
#3. Change village name
#4. check the xmm files
#5. check bmi import file #6. Simulations - change line 435,437,499

#library('igraph',lib.loc="/home/ly266/project/conda_pkgs")
## Plotting free_time
xmm<-read.csv('hw3_conn_v3_subset_s_shridhar_2021-03-30.csv')
xmm3<-read.csv('hw3_resp_v3_subset_s_shridhar_2021-03-30.csv')

#xmm<-read.csv('xmm_c.csv') # only for Hacienda San Juan
#xmm3<-read.csv('xmm3_c.csv')

#xmm<-read.csv('xmm_me.csv')#only for Mecatal
#xmm3<-read.csv('xmm3_me.csv')


resp<-xmm3[,1:dim(xmm3)[2]]


conn_i <- xmm[,1:dim(xmm)[2]]
#resp<-xmm3[,2:dim(xmm3)[2]]

## All relationships
conn_t <- subset(conn_i, conn_i$alter_source == 1 | conn_i$alter_source == 3) # subsetting or selecting elements with mathching rows
# Subset conn by relationship = closest_friend, free_time, personal_private
#resp<-xmm3[,2:dim(xmm3)[2]]
ui<-as.data.frame(unique(conn_t$relationship))

conn_t <- subset(conn_t, conn_t$relationship == "free_time" 
                 |conn_t$relationship == "personal_private"
                 |conn_t$relationship == "closest_friend" 
                 |conn_t$relationship == "father"
                 |conn_t$relationship == "partner"
)
conn_temp <- subset(conn_t, conn_t$village_name_w3 == "Hacienda San Juan")

temp <- conn_temp[c("alter")] # select alter column
colnames(temp)[1] <-"ego"  # rename the column as alter
temp <- temp$ego 
temp <- as.data.frame(temp) #still alter stored as ego
colnames(temp)[1] <- "ego"
vtable <- rbind(conn_temp[c("ego")], temp) #rows ot alter (in temp) added to the bottom of ego [so alter and ego in same list]
# Merge with gender data from resp
gender_lookup <- resp[c("respondent_master_id", "gender")]
vtable$gender <- with(gender_lookup, gender[match(vtable$ego, respondent_master_id)])

#branches <- conn_temp[c("ego", "a2701")]
#branches$risk <- ifelse(branches$a2701 == "Rarely/never",2,ifelse(branches$a2701 == "A few days a month" , 3,ifelse(branches$a2701 == "A few days a week", 4,ifelse(branches$a2701 == "Every day" , 5,ifelse(is.na(branches$risk), 1,"NA" )))))

#risk_lookup <- branches[c("ego", "risk")]
#vtable$risk <- with(risk_lookup, risk[match(vtable$ego,ego)])






#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))

#Deg_hacienda_san_juan<- as.data.frame(degree(g_vill, mode = "all"))
Deg_hacienda_san_juan<-as.data.frame(vtable$ego)
#Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(degree(g_vill, mode = "all")))
colnames(Deg_hacienda_san_juan)<-c("name")


conn_hacienda_san_juan<-as.data.frame(conn_temp$ego)
conn_hacienda_san_juan<-cbind(conn_hacienda_san_juan,as.data.frame(conn_temp$alter))
colnames(conn_hacienda_san_juan)<-c("name","friend")



#-----------------------------
## a2703 & a2704 salutation
conn_t <- subset(conn_i, conn_i$alter_source == 1 | conn_i$alter_source == 3) # subsetting or selecting elements with mathching rows
# Subset conn by relationship = closest_friend, free_time, personal_private
conn_t <- subset(conn_t, conn_t$relationship == "free_time" )
#conn_t$relationship == "personal_private"#conn_t$relationship == "closest_friend" |)
conn_temp <- subset(conn_t, conn_t$village_name_w3 == "Hacienda San Juan")
#lkk<-xmm3[,2:dim(xmm3)[2]]

temp <- conn_temp[c("alter")] # select alter column
colnames(temp)[1] <-"ego"  # rename the column as alter
temp <- temp$ego 
temp <- as.data.frame(temp) #still alter stored as ego
colnames(temp)[1] <- "ego"
vtable <- rbind(conn_temp[c("ego")], temp) #rows ot alter (in temp) added to the bottom of ego [so alter and ego in same list]
# Merge with gender data from resp
gender_lookup <- resp[c("respondent_master_id", "gender")]
vtable$gender <- with(gender_lookup, gender[match(vtable$ego, respondent_master_id)])



branches <- conn_temp[c("ego", "a2701", "a2703a", "a2703b", "a2703c",
                        "a2703d", "a2703e", "a2703f", "a2703g",
                        "a2704a", "a2704b", "a2704c",
                        "a2704d", "a2704e", "a2704f", "a2704g")]
# need to line extra condition for NA or else the previously set risks would be set to NA
branches$risk <- ifelse(branches$a2703a == "A smile",2, branches$risk) # dont put NA thing for the first one
branches$risk <- ifelse(branches$a2703b == "A gesture such as a bow, nod or wave" & is.na(branches$risk), 3, branches$risk)
branches$risk <- ifelse(branches$a2703c == "A verbal salute (like saying hello)" & is.na(branches$risk), 4, branches$risk)
branches$risk <- ifelse(branches$a2703d == "A handshake or hi-five" & is.na(branches$risk), 5, branches$risk)
branches$risk <- ifelse(branches$a2703e == "A pat on the back"& is.na(branches$risk), 6, branches$risk)
branches$risk <- ifelse(branches$a2703f == "A hug" & is.na(branches$risk), 7, branches$risk)
branches$risk <- ifelse(branches$a2703g == "A kiss on the cheek" & is.na(branches$risk), 8, branches$risk)
branches$risk <- ifelse(branches$a2704a == "A smile"& is.na(branches$risk),2, branches$risk)
branches$risk <- ifelse(branches$a2704b == "A gesture such as a bow, nod or wave" & is.na(branches$risk), 3, branches$risk)
branches$risk <- ifelse(branches$a2704c == "A verbal salute (like saying hello)" & is.na(branches$risk), 4, branches$risk)
branches$risk <- ifelse(branches$a2704d == "A handshake or hi-five" & is.na(branches$risk), 5, branches$risk)
branches$risk <- ifelse(branches$a2704e == "A pat on the back"& is.na(branches$risk), 6, branches$risk)
branches$risk <- ifelse(branches$a2704f == "A hug" & is.na(branches$risk), 7, branches$risk)
branches$risk <- ifelse(branches$a2704g == "A kiss on the cheek" & is.na(branches$risk), 8, branches$risk)
#branches$risk <- ifelse(is.na(branches$risk), 1, branches$risk)

risk_lookup <- branches[c("ego", "risk")]
vtable$risk <- with(risk_lookup, risk[match(vtable$ego,ego)])


# Merge with village
village_lookup <- resp[c("respondent_master_id", "village_name_w3")]
vtable$village <- with(village_lookup, village_name_w3[match(vtable$ego, respondent_master_id)])


#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))

risk_sal <- vtable[c("ego","risk")]
xkk <- with(risk_sal, risk[match(Deg_hacienda_san_juan$name, ego)])

Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(xkk))
colnames(Deg_hacienda_san_juan)<-c("name","Salutation")

risk_sal2 <- branches[c("ego","risk")]
xkk2 <- with(risk_sal2, risk[match(conn_hacienda_san_juan$name, ego)|match(conn_hacienda_san_juan$friend, ego)])

conn_hacienda_san_juan<-cbind(conn_hacienda_san_juan,as.data.frame(xkk2))
colnames(conn_hacienda_san_juan)<-c("name","friend","Salutation")

#-----------------------------
#x<-E(g_vill)
## a2701 how often do you meet each other?
conn_t <- subset(conn_i, conn_i$alter_source == 1 | conn_i$alter_source == 3) # subsetting or selecting elements with mathching rows
# Subset conn by relationship = closest_friend, free_time, personal_private
conn_t <- subset(conn_t, conn_t$relationship == "free_time" )
#conn_t$relationship == "personal_private"#conn_t$relationship == "closest_friend" |)
conn_temp <- subset(conn_t, conn_t$village_name_w3 == "Hacienda San Juan")

temp <- conn_temp[c("alter")] # select alter column
colnames(temp)[1] <-"ego"  # rename the column as alter
temp <- temp$ego 
temp <- as.data.frame(temp) #still alter stored as ego
colnames(temp)[1] <- "ego"
vtable <- rbind(conn_temp[c("ego")], temp) #rows ot alter (in temp) added to the bottom of ego [so alter and ego in same list]
# Merge with gender data from resp
gender_lookup <- resp[c("respondent_master_id", "gender")]
vtable$gender <- with(gender_lookup, gender[match(vtable$ego, respondent_master_id)])

branches <- conn_temp[c("ego", "a2701")]
branches$risk <- ifelse(branches$a2701 == "Rarely/never",2,ifelse(branches$a2701 == "A few days a month", 3,ifelse(branches$a2701 == "A few days a week", 4,ifelse(branches$a2701 == "Every day" , 5,1))))
branches$risk <- ifelse(is.na(branches$a2701),1,branches$risk)

risk_lookup <- branches[c("ego", "risk")]
vtable$risk <- with(risk_lookup, risk[match(vtable$ego,ego)])


# Merge with village
village_lookup <- resp[c("respondent_master_id", "village_name_w3")]
vtable$village <- with(village_lookup, village_name_w3[match(vtable$ego, respondent_master_id)])

#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))

risk_freq <- vtable[c("ego","risk")]
xkk <- with(risk_freq, risk[match(Deg_hacienda_san_juan$name, ego)])

Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(xkk))
colnames(Deg_hacienda_san_juan)<-c("name","Salutation","Frequency")

risk_sal2 <- branches[c("ego","risk")]
xkk2 <- with(risk_sal2, risk[match(conn_hacienda_san_juan$name, ego)|match(conn_hacienda_san_juan$friend, ego)])

conn_hacienda_san_juan<-cbind(conn_hacienda_san_juan,as.data.frame(xkk2))
colnames(conn_hacienda_san_juan)<-c("name","friend","Salutation","Frequency")

#-----------------------------
## a2702 how often do you share a meal with each other?
conn_t <- subset(conn_i, conn_i$alter_source == 1 | conn_i$alter_source == 3) # subsetting or selecting elements with mathching rows
# Subset conn by relationship = closest_friend, free_time, personal_private
conn_t <- subset(conn_t, conn_t$relationship == "free_time" )
#conn_t$relationship == "personal_private"#conn_t$relationship == "closest_friend" |)
conn_temp <- subset(conn_t, conn_t$village_name_w3 == "Hacienda San Juan")

temp <- conn_temp[c("alter")] # select alter column
colnames(temp)[1] <-"ego"  # rename the column as alter
temp <- temp$ego 
temp <- as.data.frame(temp) #still alter stored as ego
colnames(temp)[1] <- "ego"
vtable <- rbind(conn_temp[c("ego")], temp) #rows ot alter (in temp) added to the bottom of ego [so alter and ego in same list]
# Merge with gender data from resp
gender_lookup <- resp[c("respondent_master_id", "gender")]
vtable$gender <- with(gender_lookup, gender[match(vtable$ego, respondent_master_id)])

branches <- conn_temp[c("ego", "a2702")]
branches$risk <- ifelse(branches$a2702 == "About once a month or less",2,ifelse(branches$a2702 == "A few times a month", 3,ifelse(branches$a2702 == "About once a week", 4,ifelse(branches$a2702 == "Almost every day" , 5,1))))
branches$risk <- ifelse(is.na(branches$a2702),1,branches$risk)

risk_lookup <- branches[c("ego", "risk")]
vtable$risk <- with(risk_lookup, risk[match(vtable$ego,ego)])


# Merge with village
village_lookup <- resp[c("respondent_master_id", "village_name_w3")]
vtable$village <- with(village_lookup, village_name_w3[match(vtable$ego, respondent_master_id)])

#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))

risk_freq <- vtable[c("ego","risk")]
xkk <- with(risk_freq, risk[match(Deg_hacienda_san_juan$name, ego)])

Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(xkk))
colnames(Deg_hacienda_san_juan)<-c("name","Salutation","Frequency","Meal")





Deg_hacienda_san_juan$Salutation <- ifelse(is.na(Deg_hacienda_san_juan$Salutation),0,Deg_hacienda_san_juan$Salutation)
Deg_hacienda_san_juan$Frequency <- ifelse(is.na(Deg_hacienda_san_juan$Frequency),0,Deg_hacienda_san_juan$Frequency)
Deg_hacienda_san_juan$Meal <- ifelse(is.na(Deg_hacienda_san_juan$Meal),0,Deg_hacienda_san_juan$Meal)


ss <- ifelse(Deg_hacienda_san_juan$Salutation>3 & Deg_hacienda_san_juan$Frequency>3 & Deg_hacienda_san_juan$Meal>3,1, 0)
Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(ss))
colnames(Deg_hacienda_san_juan)<-c("name","Salutation","Frequency","Meal","Super_Spreaders")



write.csv(Deg_hacienda_san_juan,"Hacienda San Juan-variables.csv")
write.csv(conn_hacienda_san_juan,"Hacienda San Juan-conn-vars.csv")

## PLotting super spreaders

conn_t <- subset(conn_i, conn_i$alter_source == 1 | conn_i$alter_source == 3) # subsetting or selecting elements with mathching rows
# Subset conn by relationship = closest_friend, free_time, personal_private
resp<-xmm3[,1:dim(xmm3)[2]]
ui<-as.data.frame(unique(conn_t$relationship))

conn_t <- subset(conn_t, conn_t$relationship == "free_time" 
                 |conn_t$relationship == "personal_private"
                 |conn_t$relationship == "closest_friend" 
                 |conn_t$relationship == "father"
                 |conn_t$relationship == "partner"
)
conn_temp <- subset(conn_t, conn_t$village_name_w3 == "Hacienda San Juan")

temp <- conn_temp[c("alter")] # select alter column
colnames(temp)[1] <-"ego"  # rename the column as alter
temp <- temp$ego 
temp <- as.data.frame(temp) #still alter stored as ego
colnames(temp)[1] <- "ego"
vtable <- rbind(conn_temp[c("ego")], temp) #rows ot alter (in temp) added to the bottom of ego [so alter and ego in same list]
# Merge with gender data from resp
gender_lookup <- resp[c("respondent_master_id", "gender")]
vtable$gender <- with(gender_lookup, gender[match(vtable$ego, respondent_master_id)])

#branches <- conn_temp[c("ego", "a2701")]
#branches$risk <- ifelse(branches$a2701 == "Rarely/never",2,ifelse(branches$a2701 == "A few days a month" , 3,ifelse(branches$a2701 == "A few days a week", 4,ifelse(branches$a2701 == "Every day" , 5,ifelse(is.na(branches$risk), 1,"NA" )))))

#risk_lookup <- branches[c("ego", "risk")]
#vtable$risk <- with(risk_lookup, risk[match(vtable$ego,ego)])


# Merge with village
village_lookup <- resp[c("respondent_master_id", "village_name_w3")]
vtable$village <- with(village_lookup, village_name_w3[match(vtable$ego, respondent_master_id)])

# Merge with village
village_lookup <- resp[c("respondent_master_id", "age_at_survey")]
vtable$age <- with(village_lookup, age_at_survey[match(vtable$ego, respondent_master_id)])

#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))
vtable<-cbind(vtable,Deg_hacienda_san_juan$Super_Spreaders)
vtable<-cbind(vtable,Deg_hacienda_san_juan$Salutation)
vtable<-cbind(vtable,Deg_hacienda_san_juan$Frequency)
colnames(vtable)<-c("ego","gender","village","age","ss","Salutation","Frequency")

conn_hacienda_san_juan$Salutation <- ifelse(is.na(conn_hacienda_san_juan$Salutation),1,conn_hacienda_san_juan$Salutation)
conn_hacienda_san_juan$Frequency <- ifelse(is.na(conn_hacienda_san_juan$Frequency),1,conn_hacienda_san_juan$Frequency)

gg<-unique(conn_hacienda_san_juan$Salutation)

conn_temp2<-cbind(conn_temp[,1:2],conn_hacienda_san_juan$Salutation)
conn_temp2<-cbind(conn_temp2,conn_hacienda_san_juan$Frequency)

# Merge with village
village_lookup <- resp[c("respondent_master_id", "age_at_survey")]
conn_temp2$age <- with(village_lookup, age_at_survey[match(conn_temp2$ego, respondent_master_id)])

# Merge with village
village_lookup <- resp[c("respondent_master_id", "gender")]
conn_temp2$gender <- with(village_lookup, gender[match(conn_temp2$ego, respondent_master_id)])

colnames(conn_temp2)<-c("ego","alter","Salutation","Frequency","age","gender")

conn_hacienda_san_juan<-conn_temp2## Risky line 04-06-21

conn_hacienda_san_juan$age <- ifelse(is.na(conn_hacienda_san_juan$age),15,conn_hacienda_san_juan$age)
conn_hacienda_san_juan$gender <- ifelse(is.na(conn_hacienda_san_juan$gender),"female",conn_hacienda_san_juan$gender)

sum(Deg_hacienda_san_juan$Super_Spreaders)

toString(vtable[1,1])==toString(conn_hacienda_san_juan[1,2])

#xyy<-unique(as.data.frame(conn_hacienda_san_juan[,2]))

edge_trans=matrix(0,dim(vtable)[1],dim(vtable)[1])
rownames(edge_trans)<-vtable[,1]
colnames(edge_trans)<-vtable[,1]

edge_trans[1,1]
tk1=matrix(0,dim(conn_hacienda_san_juan)[1],1)
tk2=matrix(0,dim(conn_hacienda_san_juan)[1],1)

for(i in 1:dim(vtable)[1]){
  for(j in 1:dim(conn_hacienda_san_juan)[1]){
    if(toString(vtable[i,1])==toString(conn_hacienda_san_juan[j,1])){
      tk1[j,1]=i
    }
    if(toString(vtable[i,1])==toString(conn_hacienda_san_juan[j,2])){
      tk2[j,1]=i
    }
  }
}
for(i in 1:dim(tk1)[1]){
  edge_trans[tk1[i,1],tk2[i,1]]=1 # its directed, not undirected edge
  edge_trans[tk2[i,1],tk1[i,1]]=1
}
sum(edge_trans)

freq=matrix(0,dim(vtable)[1],dim(vtable)[1])
rownames(freq)<-vtable[,1]
colnames(freq)<-vtable[,1]

for(i in 1:dim(tk1)[1]){
  if(conn_hacienda_san_juan[i,4]==1|conn_hacienda_san_juan[i,4]==0){#Add Salutation condition
    freq[tk1[i,1],tk2[i,1]]=3/25
    freq[tk2[i,1],tk1[i,1]]=3/25
  }else{}
  if(conn_hacienda_san_juan[i,4]==2&conn_hacienda_san_juan[i,3]>1){ #the contact part -see if its okay 02-25-21
    freq[tk1[i,1],tk2[i,1]]=3/25
    freq[tk2[i,1],tk1[i,1]]=3/25
  }
  if(conn_hacienda_san_juan[i,4]==3&conn_hacienda_san_juan[i,3]>1){ # Any contact is okay except Not known
    freq[tk1[i,1],tk2[i,1]]=3/25
    freq[tk2[i,1],tk1[i,1]]=3/25
  }
  if(conn_hacienda_san_juan[i,4]==4&conn_hacienda_san_juan[i,3]>1){
    freq[tk1[i,1],tk2[i,1]]=3/25
    freq[tk2[i,1],tk1[i,1]]=3/25
  }
  if(conn_hacienda_san_juan[i,4]==5&conn_hacienda_san_juan[i,3]>1){
    freq[tk1[i,1],tk2[i,1]]=3/25
    freq[tk2[i,1],tk1[i,1]]=3/25
  }
  if(conn_hacienda_san_juan[i,6]==2){ #Gender-dispersion factor
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*1.3
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*1.3
  }
  if(conn_hacienda_san_juan[i,5]>20&conn_hacienda_san_juan[i,5]<31){ #Age-dispersion factor from Chen.et.al
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*1.245
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*1.245
  }
  if(conn_hacienda_san_juan[i,5]>30&conn_hacienda_san_juan[i,5]<41){#Age-dispersion factor from Chen.et.al
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*0.985
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*0.985
  }
  if(conn_hacienda_san_juan[i,5]>40&conn_hacienda_san_juan[i,5]<51){#Age-dispersion factor from Chen.et.al
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*1.19
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*1.19
  }
  if(conn_hacienda_san_juan[i,5]>50&conn_hacienda_san_juan[i,5]<61){#Age-dispersion factor from Chen.et.al
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*1.34
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*1.34
  }
  if(conn_hacienda_san_juan[i,5]>60&conn_hacienda_san_juan[i,5]<71){#Age-dispersion factor from Chen.et.al
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*0.978
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*0.978
  }
  if(conn_hacienda_san_juan[i,5]>70){#Age-dispersion factor from Chen.et.al
    freq[tk1[i,1],tk2[i,1]]=freq[tk1[i,1],tk2[i,1]]*0.835
    freq[tk2[i,1],tk1[i,1]]=freq[tk2[i,1],tk1[i,1]]*0.835
  }
  
}

#conn_hacienda_san_juan[1,4]

freq[1,4]
# for COVID
#pdf<-c(0.0135608704804066,0.0348223933128218,0.0522994650750815,0.0653315228970143,0.0739005405291876,0.0784135784965912,0.0795075328934619,0.0779004446906258,0.0742924229730756,0.0693086232740543,0.0634732952896427,0.0572043841529478,0.0508202521770477,0.0445524793748991,0.0385607937919468,0.0329477809754459,0.0277721352151112,0.0230599334923272,0.0188138418642664,0.0150203960129056,0.0116556042755924,0.00868915320340797,0.00608748622517829,0.00381599654696059,0.00184053863581980)

# for Diarrhea # from Chen et.al "Shigellosis in Hubei"
pdf<-c(0.2443,0.3087,0.1594,0.0923,0.0547,0.0336,0.0228,0.0842)

pdf<-as.data.frame(pdf)

time=100 # 1 year--for simulation constraints

xd=0
xd2=0
xd3=0
xd4=0
sim_runs=100

# Starts here
#library(foreach)
#library(doParallel)
#unregisterDoParallel
#registerDoSEQ()

#xf_all<-matrix(0,sim_runs*dim(vtable)[1],dim(vtable)[1])
#dim(xf_all)

xly<-1 #starting seeded node

for(tio in xly:30){  #dim(vtable)[1]
  xf<-matrix(0,sim_runs,dim(vtable)[1])
  for(ytu in 1:sim_runs){
    
    inf_nod<-matrix(0,time,dim(vtable)[1]) # Recording which node gets infected
    
    # for loop for different seeded cells
    cho_node=tio #Seeded node
    
    inf_nod[1:time,cho_node]=1 #seeded
    time_trans<-matrix(0,1,dim(vtable)[1])# transmitting time (from characteristic equation) ----only 1 column for storing real time of infection transmission
    time_trans[cho_node]=time_trans[cho_node]+1 #need to differentiate between transmission and getting infected
    
    for(t in 1:time){
      for (i in 1:dim(vtable)[1]){
        if(inf_nod[t,i]==1&time_trans[i]<8){  #&edge_trans[cho_node,i]==1 # for COVID=25, for diarrhea=8---SHIV 04-07-21
          
          xc<-which(edge_trans[i,]==1)
          freq_prob<-freq[i,xc]
          
          for(g in 1:length(xc)){
            p=pdf[time_trans[i]+1,1]*freq_prob[g]*rnorm(1,mean=1,sd=0.01) #including frequency of contact # Add noise 1% sd --Gaussian 04-07-21
            xg3 <- rbinom(1,1,p) # problem because it picks 0
            if(inf_nod[t,xc[g]]<1){
              inf_nod[t:time,xc[g]]=xg3
              
              xd<-rbind(xd,p)#checking
              xd2<-rbind(xd2,xg3)#checking
              xd3<-rbind(xd3,time_trans[i])#checking
              xd4<-rbind(xd4,i)#checking
            }
          }
          
          
          
          time_trans[i]=time_trans[i]+1
          
          
        }
        
        
        
      }
      
    }
    
    inf_new=as.data.frame(which((inf_nod[time,]-inf_nod[1,])==1))#for every simulation run, records new nodes arising
    
    if(dim(inf_new)[1]>0){
      xf[ytu,1:dim(inf_new)[1]]=inf_new[1:dim(inf_new)[1],1]
    }
    
  }
  
  if(tio==xly){xf_all<-xf
  }else{xf_all<-rbind(xf_all,xf)}
  #xf_all[(tio-1)*100+1:tio*100,]<-xf
  
  #sprintf("%i",tio)
  
}

write.csv(as.data.frame(xf_all),"Nocon-med-Hacienda San Juan-sim-4-new-1000-dist1-30.csv") # need to run from 14 - 85

dim(xf_all)


