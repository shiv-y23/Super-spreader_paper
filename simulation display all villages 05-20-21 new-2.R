
#For changing village name:
#1. change Deg_ variable
#2. change conn_ variable
#3. Change village name
#4. check the xmm files
#5. check bmi import file

# For running full file: first read in the wave1,2,3 datasets
# 2. make sure yyu (Sime results) are read in

library('igraph')
## Plotting free_time
#xmm<-read.csv('hw3_conn_v3_subset_s_shridhar_2021-03-30.csv')
#xmm3<-read.csv('hw3_resp_v3_subset_s_shridhar_2021-03-30.csv')

#xmm3_w1<-read.csv('hw1_resp_v8_subset_s_shridhar_2021-03-30.csv')

#xmm3_w2<-read.csv('hw2_resp_v5_subset_s_shridhar_2021-03-30.csv')


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


# Merge with village
village_lookup <- resp[c("respondent_master_id", "village_name_w3")]
vtable$village <- with(village_lookup, village_name_w3[match(vtable$ego, respondent_master_id)])

#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))

g_vill <- graph_from_data_frame(conn_temp, vtable, directed = T)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
V(g_vill)$shape <- ifelse(V(g_vill)$gender == "male", "square", "circle")
V(g_vill)$size <- degree(g_vill, mode = "all") / 2
#V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(6), sequential_pal)
#V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(4), sequential_pal)
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r
plot(g_vill,
     edge.arrow.size = 0.2,
     #     vertex.color = adjustcolor(V(g_vill)$color, alpha.f = 1.2),
     vertex.label = NA,
     edge.curved = 0,
     rescale = T)

#legend(x=-1.5, y=-1.1, c("NA","Rarely/never", "A few days a month", "A few days a week", "Every day"),
#       pch=21, col="#777777",
#       pt.bg=c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", 
#               "#B30000"), pt.cex=2, cex=.8, bty="n", ncol=2)


#Deg_hacienda_san_juan<- as.data.frame(degree(g_vill, mode = "all"))
Deg_hacienda_san_juan<-as.data.frame(V(g_vill)$name)
Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(degree(g_vill, mode = "all")))
colnames(Deg_hacienda_san_juan)<-c("name","conn")

conn_hacienda_san_juan<-as.data.frame(conn_temp$ego)
conn_hacienda_san_juan<-cbind(conn_hacienda_san_juan,as.data.frame(conn_temp$alter))
colnames(conn_hacienda_san_juan)<-c("name","friend")


max(Deg_hacienda_san_juan$conn)

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

g_vill <- graph_from_data_frame(conn_temp, vtable, directed = F)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
V(g_vill)$shape <- ifelse(V(g_vill)$gender == "male", "square", "circle") # This is for gender, no need, 05-02-21
V(g_vill)$size <- degree(g_vill, mode = "all") / 1.3 # This is only for size of node based on degree 05-02-21



V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(7), sequential_pal)
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r
plot(g_vill,
     edge.arrow.size = 0.2,
     vertex.color = adjustcolor(V(g_vill)$color, alpha.f = 1.2),
     vertex.label = NA,
     edge.curved = 0,
     rescale = T)

legend(x=-1.5, y=-1.1, c("NA","A smile", "A gesture such as a bow, nod or wave", "A verbal salute (like saying hello)", "A handshake or hi-five", "A pat on the back", "A hug","A kiss on the cheek"),
       pch=21, col="#777777",
       pt.bg=c("#FFF7EC", 
               "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", 
               "#D7301F", "#990000"), pt.cex=2, cex=.8, bty="n", ncol=2)

risk_sal <- vtable[c("ego","risk")]
xkk <- with(risk_sal, risk[match(Deg_hacienda_san_juan$name, ego)])

Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(xkk))
colnames(Deg_hacienda_san_juan)<-c("name","conn","Salutation")

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

g_vill <- graph_from_data_frame(conn_temp, vtable, directed = T)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
V(g_vill)$shape <- ifelse(V(g_vill)$gender == "male", "square", "circle")
V(g_vill)$size <- degree(g_vill, mode = "all") / 1.3
#V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(6), sequential_pal)
V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(4), sequential_pal)
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r
plot(g_vill,
     edge.arrow.size = 0.2,
     vertex.color = adjustcolor(V(g_vill)$color, alpha.f = 1.2),
     vertex.label = NA,
     edge.curved = 0,
     rescale = T)

legend(x=-1.5, y=-1.1, c("NA","Rarely/never", "A few days a month", "A few days a week", "Every day"),
       pch=21, col="#777777",
       pt.bg=c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", 
               "#B30000"), pt.cex=2, cex=.8, bty="n", ncol=2)

risk_freq <- vtable[c("ego","risk")]
xkk <- with(risk_freq, risk[match(Deg_hacienda_san_juan$name, ego)])

Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(xkk))
colnames(Deg_hacienda_san_juan)<-c("name","conn","Salutation","Frequency")

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

g_vill <- graph_from_data_frame(conn_temp, vtable, directed = T)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
V(g_vill)$shape <- ifelse(V(g_vill)$gender == "male", "square", "circle")
V(g_vill)$size <- degree(g_vill, mode = "all") / 1.3
#V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(6), sequential_pal)
V(g_vill)$color <- scales::dscale(as.numeric(V(g_vill)$risk) %>% cut(4), sequential_pal)
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r
plot(g_vill,
     edge.arrow.size = 0.2,
     vertex.color = adjustcolor(V(g_vill)$color, alpha.f = 1.2),
     vertex.label = NA,
     edge.curved = 0,
     rescale = T)

legend(x=-1.5, y=-1.1, c("NA","About once a month or less", "A few times a month", "About once a week", "Almost every day"),
       pch=21, col="#777777",
       pt.bg=c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", 
               "#B30000"), pt.cex=2, cex=.8, bty="n", ncol=2)

risk_freq <- vtable[c("ego","risk")]
xkk <- with(risk_freq, risk[match(Deg_hacienda_san_juan$name, ego)])

Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(xkk))
colnames(Deg_hacienda_san_juan)<-c("name","conn","Salutation","Frequency","Meal")




Deg_hacienda_san_juan$Salutation <- ifelse(is.na(Deg_hacienda_san_juan$Salutation),0,Deg_hacienda_san_juan$Salutation)
Deg_hacienda_san_juan$Frequency <- ifelse(is.na(Deg_hacienda_san_juan$Frequency),0,Deg_hacienda_san_juan$Frequency)
Deg_hacienda_san_juan$Meal <- ifelse(is.na(Deg_hacienda_san_juan$Meal),0,Deg_hacienda_san_juan$Meal)


ss <- ifelse(Deg_hacienda_san_juan$conn>10&Deg_hacienda_san_juan$Salutation>3 & Deg_hacienda_san_juan$Frequency>3 & Deg_hacienda_san_juan$Meal>3,1, 0)
Deg_hacienda_san_juan<-cbind(Deg_hacienda_san_juan,as.data.frame(ss))
colnames(Deg_hacienda_san_juan)<-c("name","conn","Salutation","Frequency","Meal","Super_Spreaders")


################
# Dirarrhea positive

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

dia_pos <- resp[c("respondent_master_id", "c0500")]
vtable$dia_pos <- with(dia_pos, c0500[match(vtable$ego, respondent_master_id)])

dia_pos <- resp[c("respondent_master_id", "c0500")]
Deg_hacienda_san_juan$dia_pos <- with(dia_pos, c0500[match(Deg_hacienda_san_juan$name, respondent_master_id)])

xg_rct<-which(dia_pos=="Yes") # 1.2%

623/48723

xg<-which(Deg_hacienda_san_juan$dia_pos=="Yes")

xg2<-which(vtable$dia_pos=="Yes")

Deg_hacienda_san_juan$dia_pos <- ifelse(Deg_hacienda_san_juan$dia_pos == "Yes",1,0)
Deg_hacienda_san_juan$dia_pos <- ifelse(is.na(Deg_hacienda_san_juan$dia_pos),0,Deg_hacienda_san_juan$dia_pos)

dia_pos_w1 <- xmm3_w1[c("respondent_master_id", "c0500")]
Deg_hacienda_san_juan$dia_pos_w1 <- with(dia_pos_w1, c0500[match(Deg_hacienda_san_juan$name, respondent_master_id)])

dia_pos_w2 <- xmm3_w2[c("respondent_master_id", "c0500")]
Deg_hacienda_san_juan$dia_pos_w2 <- with(dia_pos_w2, c0500[match(Deg_hacienda_san_juan$name, respondent_master_id)])

Deg_hacienda_san_juan$dia_pos_w1 <- ifelse(Deg_hacienda_san_juan$dia_pos_w1 == "Yes",1,0)
Deg_hacienda_san_juan$dia_pos_w1 <- ifelse(is.na(Deg_hacienda_san_juan$dia_pos_w1),0,Deg_hacienda_san_juan$dia_pos_w1)

Deg_hacienda_san_juan$dia_pos_w2 <- ifelse(Deg_hacienda_san_juan$dia_pos_w2 == "Yes",1,0)
Deg_hacienda_san_juan$dia_pos_w2 <- ifelse(is.na(Deg_hacienda_san_juan$dia_pos_w2),0,Deg_hacienda_san_juan$dia_pos_w2)

Deg_hacienda_san_juan$dia_tot<-Deg_hacienda_san_juan$dia_pos+Deg_hacienda_san_juan$dia_pos_w1+Deg_hacienda_san_juan$dia_pos_w2

Deg_hacienda_san_juan$dia_tot<-ifelse(Deg_hacienda_san_juan$dia_tot>1,1,Deg_hacienda_san_juan$dia_tot)

Deg_hacienda_san_juan<-Deg_hacienda_san_juan[,c(1,2,3,4,5,6,10)]

#####

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

#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))
vtable<-cbind(vtable,Deg_hacienda_san_juan$Super_Spreaders)
vtable<-cbind(vtable,Deg_hacienda_san_juan$Salutation)
vtable<-cbind(vtable,Deg_hacienda_san_juan$Frequency)
vtable<-cbind(vtable,Deg_hacienda_san_juan$dia_tot)
colnames(vtable)<-c("ego","gender","village","ss","Salutation","Frequency","dia_pos")

conn_hacienda_san_juan$Salutation <- ifelse(is.na(conn_hacienda_san_juan$Salutation),1,conn_hacienda_san_juan$Salutation)
conn_hacienda_san_juan$Frequency <- ifelse(is.na(conn_hacienda_san_juan$Frequency),1,conn_hacienda_san_juan$Frequency)

gg<-unique(conn_hacienda_san_juan$Salutation)

conn_temp2<-cbind(conn_temp[,1:2],conn_hacienda_san_juan$Salutation)
conn_temp2<-cbind(conn_temp2,conn_hacienda_san_juan$Frequency)
colnames(conn_temp2)<-c("ego","alter","Salutation","Frequency")

g_vill <- graph_from_data_frame(conn_temp2, vtable, directed = F)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
#V(g_vill)$shape <- ifelse(V(g_vill)$gender == "male", "square", "circle")
#V(g_vill)$size <- degree(g_vill, mode = "all") / 2

edge.start <- ends(g_vill, es=E(g_vill), names=F)[,1]

label_distance <- ifelse(vtable$Frequency == 5,0.1, vtable$Frequency)
label_distance <- ifelse(vtable$Frequency == 4, 0.2, label_distance)
label_distance <- ifelse(vtable$Frequency == 3, 0.3, label_distance)
label_distance <- ifelse(vtable$Frequency == 2, 0.4, label_distance)
label_distance <- ifelse(vtable$Frequency == 1, 0.55, label_distance)


gv<-E(g_vill)$color
gv2<-unique(edge.start)

df<-E(g_vill)$Salutation


V(g_vill)$color <- ifelse(V(g_vill)$dia_pos==1,"#B30000","#56B4E9")

V(g_vill)$frame.color <- ifelse(V(g_vill)$dia_pos==1,"#B30000","#56B4E9")

edge.col <- ifelse(V(g_vill)$Salutation == 1,"#FFF7EC",ifelse(V(g_vill)$Salutation == 2, "#FEE8C8",ifelse(V(g_vill)$Salutation == 3, "#FDD49E",ifelse(V(g_vill)$Salutation == 4, "#FDBB84",ifelse(V(g_vill)$Salutation == 5, "#FC8D59",ifelse(V(g_vill)$Salutation == 6, "#EF6548",ifelse(V(g_vill)$Salutation == 7, "#D7301F","#B30000")))))))


edge.start <- ends(g_vill, es=E(g_vill), names=F)[,2] # if it originates from that node, then he/she's behavior highlighted


edge.start3<-ifelse(vtable$Salutation[edge.start] == 1,"#d73027",ifelse(vtable$Salutation[edge.start] == 2, "#f46d43",ifelse(vtable$Salutation[edge.start] == 3, "#fdae61",ifelse(vtable$Salutation[edge.start] == 4, "#fee08b",ifelse(vtable$Salutation[edge.start] == 5, "#d9ef8b",ifelse(vtable$Salutation[edge.start] == 6, "#a6d96a",ifelse(vtable$Salutation[edge.start] == 7, "#66bd63","#1a9850")))))))


# pta paper graph

#edge.start4 <- ifelse(vtable$Frequency[edge.start] >= 4,"#d73027", vtable$Salutation[edge.start])
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 6, "#f46d43", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 5, "#fdae61", edge.start4)
edge.start4 <- ifelse(vtable$Frequency[edge.start] == 3, "#fee08b", vtable$Salutation[edge.start])
edge.start4 <- ifelse(vtable$Frequency[edge.start] < 3, "#1a9850", edge.start4)
edge.start4 <- ifelse(vtable$Frequency[edge.start] > 3, "#d73027", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 3, "#d9ef8b", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 2, "#a6d96a", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 1, "#66bd63", edge.start4)
#edge.start4 <- ifelse(vtable$Frequency[edge.start] <= 2, "#1a9850", edge.start4)

#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 5,5/2, vtable$Frequency[edge.start])
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 4, 4/2, edge.width4)
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 3, 3/2, edge.width4)
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 2, 2/2, edge.width4)
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 1, 1/2, edge.width4)

edge.width4 <- ifelse(vtable$Frequency[edge.start] >= 1,3, vtable$Frequency[edge.start])
edge.width4 <- ifelse(vtable$Frequency[edge.start] == 2.5, 2, edge.width4)
edge.width4 <- ifelse(vtable$Frequency[edge.start] <= 2, 1, edge.width4)

lk<-as.data.frame(edge.width4)


xy3<-as.data.frame(edge.start4)
xy4<-as.data.frame(which(vtable$Salutation[edge.start] == 7))
xy5<-as.data.frame(which(conn_temp2 == 7))

#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r
pdf('Hacienda San Juan dia_pos.pdf',height=8.5,width=14)
set.seed(1)
plot(g_vill,
     edge.arrow.size = 0.2,edge.color=edge.start4,#edge.width=edge.width4,#edge.arrow.size=edge.width4,#edge.color=edge.col,
     vertex.color = adjustcolor(V(g_vill)$color, alpha.f = 0.7),edge.width=2,#edge.color="#737373",#"#fee08b",
     vertex.size=7, #vertex.frame.color="#000000",
     vertex.label = NA,vertex.label.dist=0,
     edge.curved = 0,
     rescale = T)

#legend(x=-1.8, y=-0.8, c("Male"),
#       pch=0, col="#777777",
#       pt.bg=c("#f7f7f7"), pt.cex=2, cex=.8, bty="n", ncol=1)
#legend(x=-1.3, y=-0.8, c("Female"),
#       pch=1, col="#777777",
#       pt.bg=c("#f7f7f7"), pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=-1.8, y=-0.9, c("Regular"),
       pch=21, col="#56B4E9",
       pt.bg=c("#56B4E9"), pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=-1.3, y=-0.9, c( "Diarrhea positive"),
       pch=21, col="#B30000",
       pt.bg=c("#B30000"), pt.cex=2, cex=.8, bty="n", ncol=1)

legend(x=-1.8, y=-1, c("About once a month or less"),
       pch="-", col="#1a9850",
       pt.bg=c("#1a9850"), pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=-1.8, y=-1.1, c("A few times a month"),
       pch="-", col="#fee08b",
       pt.bg=c("#fee08b"), pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=-1.8, y=-1.2, c("More than once a week"),
       pch="-", col="#d73027",
       pt.bg=c("#d73027"), pt.cex=2, cex=.8, bty="n", ncol=1)
#legend(x=-1.8, y=-1, c("NA","A smile", "A gesture such as a bow, nod or wave", "A verbal salute (like saying hello)", "A handshake or hi-five", "A pat on the back", "A hug","A kiss on the cheek"),
#       pch="-", col=c("#1a9850",
#                      "#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61",
#                      "#f46d43","#d73027"),
#       pt.bg=c("#1a9850",
#               "#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61",
#               "#f46d43","#d73027"), pt.cex=2, cex=.8, bty="n", ncol=3)
dev.off()


x<-sequential_pal

sum(Deg_hacienda_san_juan$Super_Spreaders)

#xf<-read.csv('116-0001-05_profile.csv')


mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/100 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
# clips as a circle
add_shape("triangle", clip=shapes("circle")$clip,
          plot=mytriangle)

##########################################################################
# Heatmap
#load('Hacienda San Juan 1-85.RData')



yyu<-read.csv('Nocon-med-Hacienda San Juan-sim-4-new-1000-dist1-30.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
yyu<-yyu[,2:dim(yyu)[2]]

xf_all2<-yyu

yyu<-read.csv('Nocon-med-Hacienda San Juan-sim-4-new-1000-dist30-60.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
yyu<-yyu[,2:dim(yyu)[2]]

xf_all2<-rbind(xf_all2,yyu,check.names = FALSE)

#yyu<-read.csv('Nocon-med-Hacienda San Juan-sim-4-new-1000-dist60-90.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu,check.names = FALSE)

#yyu<-read.csv('Flu-Hacienda San Juan-sim-4-new-1000-dist1-15.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-yyu

#yyu<-read.csv('Flu-Hacienda San Juan-sim-4-new-1000-dist15-30.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu,check.names = FALSE)

#yyu<-read.csv('Flu-Hacienda San Juan-sim-4-new-1000-dist30-60.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu,check.names = FALSE)


##
#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30.csv') #diarrhea-Hacienda San Juan-sim-4-new-1000-dist1-30
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-yyu


#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist30-60.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu,check.names = FALSE)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist50-75.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist75-100.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist100-125.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist125-150.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist300-350.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist350-400.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)

#yyu<-read.csv('diarrhea-Hacienda San Juan-sim-4-new-1000-dist400-450.csv')
#yyu<-yyu[,2:dim(yyu)[2]]

#xf_all2<-rbind(xf_all2,yyu)


#

gss<-matrix(0,dim(xf_all2)[2],1) # Super spreading capability 04-15-21


for(i in 1:dim(xf_all2)[2]){
  temp<-xf_all2[(((i-1)*100)+1):(i*100),]
  ct<-length(which(temp[1,]>0))
  for(j in 2:100){
    ct<-rbind(ct,length(which(temp[j,]>0)))
  }
  gss[i,1]<-mean(ct[which(ct>0),1])
}



gmg<-matrix(0,dim(xf_all2)[2],1) # infection likelihood

for(i in 1:dim(xf_all2)[2]){
  gmg[i,1]<-length(which(xf_all2==i))
}

gms<-as.data.frame(sort(gmg))

###### Plotting heatmap gmg

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

#etable<-vtable$risk[1:517]
# Make unique
vtable <- (unique(vtable[ , 1:dim(vtable)[2]]))
vtable<-cbind(vtable,Deg_hacienda_san_juan$Salutation)
vtable<-cbind(vtable,Deg_hacienda_san_juan$Frequency)
vtable<-cbind(vtable,Deg_hacienda_san_juan$dia_tot) # or dia_pos
colnames(vtable)<-c("ego","gender","village","Salutation","Frequency","dia_pos")

#vtable <- Deg_hacienda_san_juan
gender <- resp[c("respondent_master_id", "gender")]
vtable$gender <- with(gender, gender[match(Deg_hacienda_san_juan$name, respondent_master_id)])

conn_hacienda_san_juan$Salutation <- ifelse(is.na(conn_hacienda_san_juan$Salutation),1,conn_hacienda_san_juan$Salutation)
conn_hacienda_san_juan$Frequency <- ifelse(is.na(conn_hacienda_san_juan$Frequency),1,conn_hacienda_san_juan$Frequency)

gg<-unique(conn_hacienda_san_juan$Salutation)

conn_temp2<-cbind(conn_temp[,1:2],conn_hacienda_san_juan$Salutation)
conn_temp2<-cbind(conn_temp2,conn_hacienda_san_juan$Frequency)
colnames(conn_temp2)<-c("ego","alter","Salutation","Frequency")



g_vill <- graph_from_data_frame(conn_temp2, vtable, directed = F)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
V(g_vill)$shape <- ifelse(V(g_vill)$dia_pos == "1", "square", "circle")


#with_fr(g_vill2)

#V(g_vill)$shape <- ifelse(V(g_vill)$dia_pos == "1", "triangle", V(g_vill)$shape)
#V(g_vill)$size <- degree(g_vill, mode = "all") / 2

#edge.col <- ifelse(V(g_vill)$Salutation == 1,"#FFF7EC",ifelse(V(g_vill)$Salutation == 2, "#FEE8C8",ifelse(V(g_vill)$Salutation == 3, "#FDD49E",ifelse(V(g_vill)$Salutation == 4, "#FDBB84",ifelse(V(g_vill)$Salutation == 5, "#FC8D59",ifelse(V(g_vill)$Salutation == 6, "#EF6548",ifelse(V(g_vill)$Salutation == 7, "#D7301F","#B30000")))))))

#xy2<-vtable[edge.start]$Salutation

#edge.start <- ends(g_vill, es=E(g_vill), names=F)[,2] # if it originates from that node, then he/she's behavior highlighted

#edge.start3<-ifelse(vtable$Salutation[edge.start] == 1,"#d73027",ifelse(vtable$Salutation[edge.start] == 2, "#f46d43",ifelse(vtable$Salutation[edge.start] == 3, "#fdae61",ifelse(vtable$Salutation[edge.start] == 4, "#fee08b",ifelse(vtable$Salutation[edge.start] == 5, "#d9ef8b",ifelse(vtable$Salutation[edge.start] == 6, "#a6d96a",ifelse(vtable$Salutation[edge.start] == 7, "#66bd63","#1a9850")))))))
#edge.col <- V(g_vill)$color[edge.start]

#edge.start4 <- ifelse(vtable$Frequency[edge.start] >= 4,"#d73027", vtable$Salutation[edge.start])
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 6, "#f46d43", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 5, "#fdae61", edge.start4)
#edge.start4 <- ifelse(vtable$Frequency[edge.start] == 3, "#df65b0", vtable$Salutation[edge.start])
edge.start4 <- ifelse(vtable$Frequency[edge.start] >=0 , "#d4b9da", vtable$Salutation[edge.start])
edge.start4 <- ifelse(vtable$Frequency[edge.start] > 3&vtable$Salutation[edge.start] > 3, "#980043", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 3, "#d9ef8b", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 2, "#a6d96a", edge.start4)
#edge.start4 <- ifelse(vtable$Salutation[edge.start] == 1, "#66bd63", edge.start4)
#edge.start4 <- ifelse(vtable$Frequency[edge.start] <= 2, "#1a9850", edge.start4)

#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 5,5/2, vtable$Frequency[edge.start])
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 4, 4/2, edge.width4)
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 3, 3/2, edge.width4)
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 2, 2/2, edge.width4)
#edge.width4 <- ifelse(vtable$Frequency[edge.start] == 1, 1/2, edge.width4)

# Need to split data into octiles

#Deg_hacienda_san_juan<-read.csv('Hacienda San Juan-all-param-04-22-2021.csv')
#gmg<-Deg_hacienda_san_juan$inf_lik
#gss<-Deg_hacienda_san_juan$SS_cap

gms<-as.data.frame(sort(gmg))

mn=gms[dim(gms)[1],1]

mn=rbind(mn,gms[floor(dim(gms)[1]*4/5),1])
mn=rbind(mn,gms[floor(dim(gms)[1]*3/5),1])
mn=rbind(mn,gms[floor(dim(gms)[1]*2/5),1])
mn=rbind(mn,gms[floor(dim(gms)[1]*1/5),1])
mn=rbind(mn,gms[1,1])
max(gmg)

# As octile

vert_col <- ifelse(gmg >=mn[2,1],"#d7191c", gmg)
vert_col <- ifelse(gmg >=mn[3,1]&gmg<mn[2,1], "#fdae61", vert_col)
vert_col <- ifelse(gmg >=mn[4,1]&gmg<mn[3,1], "#ffffbf", vert_col)
vert_col <- ifelse(gmg >mn[5,1]&gmg<mn[4,1], "#a6d96a", vert_col)
vert_col <- ifelse(gmg <= mn[5,1], "#1a9641", vert_col)

vert_col2<-vert_col

vert_col2_ind<-which(vtable$dia_pos==1)

vert_col2[vert_col2_ind,1]<-"#000000"

xy3<-as.data.frame(edge.start4)
xy4<-as.data.frame(which(vtable$Salutation[edge.start] == 7))
xy5<-as.data.frame(which(conn_temp2 == 7))

#add.vertex.shape("triangle", clip=vertex.shapes("circle")$clip,
#                 plot=mytriangle)

#dev.off()
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r
pdf('No con Hacienda San Juan flu inf_lik 4.pdf',height=8.5,width=14)
g_vill2 <- layout_with_kk(g_vill)
g_vill2 <- norm_coords(g_vill2, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(1)
plot(g_vill,#layout=g_vill2, #was g_vill was using regular clustering- Kamada kawai (new), new one is Freuchterman-Reingold algo (default) - for g_vill2 
     edge.arrow.size = 0.2,edge.color=edge.start4,#edge.width=edge.width4,#edge.arrow.size=edge.width4,#edge.color=edge.col,
     vertex.color = adjustcolor(vert_col, alpha.f = 0.7),edge.width=2,#edge.color="#df65b0",
     vertex.label = NA,vertex.label.dist=0,vertex.size=7,vertex.frame.color = adjustcolor(vert_col2, alpha.f = 1),vertex.frame.width=vtable$dia_pos+0.5,
     edge.curved = 0,
     rescale = T)

#get.seed

legend(x=-1, y=-1, c("Regular"),
       pch=1, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=-0.48, y=-1, c("Diarrhea positive"),
       pch=0, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=0.4, y=-0.8, c("Lesser frequency and safer salutation"),
       pch="-", col="#d4b9da", #1a9850
       pt.bg=c("#d4b9da"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.4, y=-0.9, c("Higher frequency and risker salutation"),
       pch="-", col="#980043", #d73027
       pt.bg=c("#980043"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=-1, y=-1.1, c(  "1st quintile","2nd quintile", "3rd quintile", "4th quintile","5th quintile"),
       pch=16, col=c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"),
       pt.bg=c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"), pt.cex=2, cex=1.3, bty="n", ncol=3)
#col=c("#1a9850",
#      "#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61",
#      "#f46d43","#d73027"),

dev.off()




## Super spreading capability 04-15-21

gss<-ifelse(is.nan(gss),0,gss)

gss2<-as.data.frame(sort(gss))

#gss<-log(gss) for log scale

mn=gss2[dim(gss2)[1],1]

mn=rbind(mn,gss2[floor(dim(gss2)[1]*4/5),1])
mn=rbind(mn,gss2[floor(dim(gss2)[1]*3/5),1])
mn=rbind(mn,gss2[floor(dim(gss2)[1]*2/5),1])
mn=rbind(mn,gss2[floor(dim(gss2)[1]*1/5),1])
mn=rbind(mn,gss2[1,1])


vert_col <- ifelse(gss >=mn[2,1],"#d7191c", gss)
vert_col <- ifelse(gss >=mn[3,1]&gss<mn[2,1], "#fdae61", vert_col)
vert_col <- ifelse(gss >=mn[4,1]&gss<mn[3,1], "#ffffbf", vert_col)
vert_col <- ifelse(gss >mn[5,1]&gss<mn[4,1], "#a6d96a", vert_col)
vert_col <- ifelse(gss <= mn[5,1], "#1a9641", vert_col)

vert_col2<-vert_col

vert_col2_ind<-which(vtable$dia_pos==1)

vert_col2[vert_col2_ind,1]<-"#000000"

xy3<-as.data.frame(edge.start4)
xy4<-as.data.frame(which(vtable$Salutation[edge.start] == 7))
xy5<-as.data.frame(which(conn_temp2 == 7))

#add.vertex.shape("triangle", clip=vertex.shapes("circle")$clip,
#                 plot=mytriangle)

#dev.off()
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r

pdf('No con Hacienda San Juan flu ss_cap 4.pdf',height=8.5,width=14)
set.seed(1)
plot(g_vill,#layout=g_vill2,
     edge.arrow.size = 0.2,edge.color=edge.start4,#edge.width=edge.width4,#edge.arrow.size=edge.width4,#edge.color=edge.col,
     vertex.color = adjustcolor(vert_col, alpha.f = 0.7),edge.width=2,#edge.color="#df65b0",
     vertex.label = NA,vertex.label.dist=0,vertex.size=7,vertex.frame.color = vert_col2,
     edge.curved = 0,
     rescale = T)

legend(x=-1, y=-1, c("Regular"),
       pch=1, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=-0.48, y=-1, c("Diarrhea positive"),
       pch=0, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=0.4, y=-0.8, c("Lesser frequency and safer salutation"),
       pch="-", col="#d4b9da", #1a9850
       pt.bg=c("#d4b9da"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.4, y=-0.9, c("Higher frequency and risker salutation"),
       pch="-", col="#980043", #d73027
       pt.bg=c("#980043"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=-1, y=-1.1, c(  "1st quintile","2nd quintile", "3rd quintile", "4th quintile","5th quintile"),
       pch=16, col=c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"),
       pt.bg=c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"), pt.cex=2, cex=1.3, bty="n", ncol=3)
dev.off()

# This node is responsible for how much percentage of the entire transmission?

#edge.start4 <- ifelse(vtable$Frequency[edge.start] == 3, "#2171b5", vtable$Salutation[edge.start])
#edge.start4 <- ifelse(vtable$Frequency[edge.start] < 3, "#6baed6", edge.start4)
#edge.start4 <- ifelse(vtable$Frequency[edge.start] > 3, "#08306b", edge.start4)

dim(Deg_hacienda_san_juan)[1]

gmk<-gss/dim(Deg_hacienda_san_juan)[1]*100

#dim(Deg_hacienda_san_juan)[1]
vert_col <- ifelse(gmk >=90,"#a50026", gmk)
vert_col <- ifelse(gmk >=80&gmk<90, "#d73027", vert_col)
vert_col <- ifelse(gmk >=70&gmk<80, "#f46d43", vert_col)
vert_col <- ifelse(gmk >=60&gmk<70, "#fdae61", vert_col)
vert_col <- ifelse(gmk >=50&gmk<60, "#fee08b", vert_col)
vert_col <- ifelse(gmk >=40&gmk<50, "#d9ef8b", vert_col)
vert_col <- ifelse(gmk >=30&gmk<40, "#a6d96a", vert_col)
vert_col <- ifelse(gmk >=20&gmk<30, "#66bd63", vert_col)
vert_col <- ifelse(gmk >=10&gmk<20, "#1a9850", vert_col)
vert_col <- ifelse(gmk <= 10, "#006837", vert_col)

vert_col2<-vert_col

vert_col2_ind<-which(vtable$dia_pos==1)

vert_col2[vert_col2_ind,1]<-"#000000"

pdf('No con Hacienda San Juan flu ss_cap_perc 4.pdf',height=8.5,width=14)
set.seed(1)
plot(g_vill,#layout=g_vill2,
     edge.arrow.size = 0.2,edge.color=edge.start4,#edge.width=edge.width4,#edge.arrow.size=edge.width4,#edge.color=edge.col,
     vertex.color = adjustcolor(vert_col, alpha.f = 0.7),edge.width=2,#edge.color="#df65b0",
     vertex.label = NA,vertex.label.dist=0,vertex.size=7,vertex.frame.color = adjustcolor(vert_col2, alpha.f = 1),
     edge.curved = 0,
     rescale = T)

legend(x=-1, y=-0.9, c("Regular"),
       pch=1, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=-0.48, y=-0.9, c("Diarrhea positive"),
       pch=0, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=0.4, y=-0.8, c("Lesser frequency and safer salutation"),
       pch="-", col="#d4b9da", #1a9850
       pt.bg=c("#d4b9da"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.4, y=-0.9, c("Higher frequency and risker salutation"),
       pch="-", col="#980043", #d73027
       pt.bg=c("#980043"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=-1, y=-1, c(">90% of transmission","80-90% of transmission", "70-80% of transmission", "60-70% of transmission", "50-60% of transmission", "40-50% of transmission", "30-40% of transmission","20-30% of transmission","10-20% of transmission","<10% of transmission"),
       pch=16, col=c("#a50026","#d73027","#f46d43",
                     "#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63",
                     "#1a9850","#006837"),
       pt.bg=c("#a50026","#d73027","#f46d43",
               "#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63",
               "#1a9850","#006837"), pt.cex=2, cex=1.3, bty="n", ncol=3)
dev.off()

library(reldist,quietly = TRUE)

gini(gmg)

gini(gmk)

#######################

edge.start4 <- ifelse(vtable$Frequency[edge.start] >=0 , "#cccccc", vtable$Salutation[edge.start])
edge.start4 <- ifelse(vtable$Frequency[edge.start] > 3&vtable$Salutation[edge.start] > 3, "#636363", edge.start4)

vert_col[,1]<-"#2b8cbe"


vert_col2[,1]<-"#2b8cbe"
vert_col2[vert_col2_ind,1]<-"#000000"

dist_ind<-40
dist_row<-3911

vert_col[dist_ind,1]<-"#d7191c"

vert_col2[dist_ind,1]<-"#d7191c"

jk<-xf_all2[dist_row,which(xf_all2[dist_row,]>0)]

vert_col[t(jk),1]<-"#fdae61"
vert_col2[t(jk),1]<-"#fdae61"

pdf('Hacienda San Juan sim 11.pdf',height=8.5,width=14)
set.seed(1)
plot(g_vill,#layout=g_vill2,
     edge.arrow.size = 0.2,edge.color=edge.start4,#edge.width=edge.width4,#edge.arrow.size=edge.width4,#edge.color=edge.col,
     vertex.color = adjustcolor(vert_col, alpha.f = 0.7),edge.width=2,#edge.color="#2171b5",
     vertex.label = NA,vertex.label.dist=0,vertex.size=7,vertex.frame.color = vert_col2,
     edge.curved = 0,
     rescale = T)




legend(x=0.5, y=-0.6, c("Regular"),
       pch=16, col="#2b8cbe", #1a9850
       pt.bg=c("#2b8cbe"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=1.1, y=-0.6, c("Diarrhea positive"),
       pch=0, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.5, y=-0.7, c("Seeded node"),
       pch=16, col="#d7191c",
       pt.bg=c("#d7191c"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.5, y=-0.8, c("New Infection"),
       pch=16, col="#fdae61", #d73027
       pt.bg=c("#fdae61"), pt.cex=2, cex=1.3, bty="n", ncol=1)

legend(x=0.5, y=-0.9, c("Lesser frequency and safer salutation"),
       pch="-", col="#cccccc", #1a9850
       pt.bg=c("#cccccc"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.5, y=-1, c("Higher frequency and risker salutation"),
       pch="-", col="#636363", #d73027
       pt.bg=c("#636363"), pt.cex=2, cex=1.3, bty="n", ncol=1)

dev.off()


#gmg_san_juan<-gmg
#gss_san_juan<-gss
#gmk_san_juan<-gmk

#gmg_san_juan_nocon<-gmg
#gss_san_juan_nocon<-gss
#gmk_san_juan_nocon<-gmk

#gmg_san_juan_flu<-gmg
#gss_san_juan_flu<-gss
#gmk_san_juan_flu<-gmk

gmk_dif<-gmk_san_juan-gmk_san_juan_nocon
gmg_dif<-gmg_san_juan-gmg_san_juan_nocon
gss_dif<-gss_san_juan-gss_san_juan_nocon

library(ggplot2)

x=c(1:58)

#write.csv(gmk_dif,'ss_perc.csv')
#write.csv(gmg_dif,'ss.csv')
#write.csv(gss_dif,'inf_lik.csv')

write.csv(cbind(cbind(gmk_san_juan,gmk_san_juan_nocon),gmk_san_juan_flu),'ss_perc3.csv')
write.csv(cbind(cbind(gss_san_juan,gss_san_juan_nocon),gss_san_juan_flu),'ss3.csv')
write.csv(cbind(cbind(gmg_san_juan,gmg_san_juan_nocon),gmg_san_juan_flu),'inf_lik3.csv')

#lk1<-cbind(gmg_san_juan,gmg_san_juan_nocon)
#lk2<-cbind(lk1,gss_san_juan)
#lk2<-cbind(lk2,gss_san_juan_nocon)
#lk2<-cbind(lk2,gmk_san_juan)
#lk2<-cbind(lk2,gmk_san_juan_nocon)

#cols <- c("#d73027","#fdae61")#,"#1a9850","#4575b4"
#boxplot(cbind(gmg_san_juan,gmg_san_juan_nocon), las=2,names = c("Regular","Without quality of interaction"), at =c(1,2), col=cols,box.width=c(0.1,0.1))

#ggplot(as.data.frame(t(cbind(gmg_san_juan,gmg_san_juan_nocon))), aes(x="Social network quality", y=c("Infection likelihood"))) +
#  geom_point(aes(fill=as.data.frame(t(cbind(gmg_san_juan,gmg_san_juan_nocon)))), size=5, shape=21, colour="grey20",
#             position=position_jitter(width=0.2, height=0.1)) +
#  geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") +
#labs(title="Vulnerability")


tr<-read.csv('Rank_dif_hacienda_san_juan.csv',header = F)

tr<-as.matrix(tr)

tr2<-as.data.frame(sort(tr[,1]))

#gss<-log(gss) for log scale

g_vill <- graph_from_data_frame(conn_temp2, vtable, directed = F)
g_vill <- simplify(g_vill, remove.multiple = T, remove.loops = T)
V(g_vill)$shape <- ifelse(V(g_vill)$dia_pos == "1", "square", "circle")



edge.start4 <- ifelse(vtable$Frequency[edge.start] >=0 , "#d4b9da", vtable$Salutation[edge.start])
edge.start4 <- ifelse(vtable$Frequency[edge.start] > 3&vtable$Salutation[edge.start] > 3, "#980043", edge.start4)

mn=tr2[dim(tr2)[1],1]

mn=rbind(mn,tr2[floor(dim(tr2)[1]*4/5),1])
mn=rbind(mn,tr2[floor(dim(tr2)[1]*3/5),1])
mn=rbind(mn,tr2[floor(dim(tr2)[1]*2/5),1])
mn=rbind(mn,tr2[floor(dim(tr2)[1]*1/5),1])
mn=rbind(mn,tr2[1,1])


vert_col <- ifelse(tr >=25,"#d7191c", tr)
vert_col <- ifelse(tr >0&tr<25, "#fdae61", vert_col)
vert_col <- ifelse(tr ==0, "#ffffbf", vert_col)
vert_col <- ifelse(tr >-25&tr<0, "#a6d96a", vert_col)
vert_col <- ifelse(tr <= -25, "#1a9641", vert_col)

vert_col2<-vert_col

vert_col2_ind<-which(vtable$dia_pos==1)

vert_col2[vert_col2_ind]<-"#000000"

xy3<-as.data.frame(edge.start4)
xy4<-as.data.frame(which(vtable$Salutation[edge.start] == 7))
xy5<-as.data.frame(which(conn_temp2 == 7))

#add.vertex.shape("triangle", clip=vertex.shapes("circle")$clip,
#                 plot=mytriangle)

#dev.off()
#E(g_vill)$color <- scales::dscale(as.numeric(etable) %>% cut(7), diverging_pal) #color scheme - diverging_pal, sequential or r

pdf('Hacienda San Juan rank_dif 4.pdf',height=8.5,width=14)
set.seed(1)
plot(g_vill,#layout=g_vill2,
     edge.arrow.size = 0.2,edge.color=edge.start4,#edge.width=edge.width4,#edge.arrow.size=edge.width4,#edge.color=edge.col,
     vertex.color = adjustcolor(vert_col, alpha.f = 0.7),edge.width=2,#edge.color="#df65b0",
     vertex.label = NA,vertex.label.dist=0,vertex.size=7,vertex.frame.color = vert_col2,
     edge.curved = 0,
     rescale = T)

legend(x=-1, y=-1, c("Regular"),
       pch=1, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=-0.48, y=-1, c("Diarrhea positive"),
       pch=0, col="#777777",
       pt.bg=c("#f7f7f7"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=0.4, y=-0.8, c("Lesser frequency and safer salutation"),
       pch="-", col="#d4b9da", #1a9850
       pt.bg=c("#d4b9da"), pt.cex=2, cex=1.3, bty="n", ncol=1)
legend(x=0.4, y=-0.9, c("Higher frequency and risker salutation"),
       pch="-", col="#980043", #d73027
       pt.bg=c("#980043"), pt.cex=2, cex=1.3, bty="n", ncol=1)


legend(x=-1, y=-1.1, c(  "Negative(high)","Negative(low)", "No change", "Positive(low)","Positive(high)"),
       pch=16, col=c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"),
       pt.bg=c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"), pt.cex=2, cex=1.3, bty="n", ncol=3)
dev.off()



