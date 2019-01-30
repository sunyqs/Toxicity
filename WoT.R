library(dplyr)
library(tidyr)

#increase the max print, and turn off scientific notation
options(scipen=999)
options(max.print=99999999)

#reading three tables
table_1=read.table(file="table_1.tsv", sep='\t', header=FALSE)
table_2=read.csv("table_2.csv")
table_3=read.csv("table_3 (2).csv")
AutoReport=table_2[table_2[,1]=="game automatic",]
names(table_1)=c("date","spa_id","user_skill_at_this_battle_pr","user_battle_count_at_time_battle","user_team_identifier","user_clan_identifier","user_days_in_clan_at_the_time_battle","battle_identifier","user_platoon_identifier","whether_user_is_legionnaire","whether_user_died_in_battle","amount_time_user_was_in_battle","vehicle_id","tank_type","tank_name","tank_level","distance_traveled_in_meter","damage_dealt","damage_taken","shots_fired","shots_hit","xp_gained","assisted_damage","damage_blocked","average_pr_team_1","average_pr_team_2","team_size","battle_type","battle_result","battle_start_time","battle_end_time","battle_time","a_chat_entries_in_battle","b_unique_players_with_entry_in_chat","c_users_chat_entries","e_users_orginal_chat_entries")
table_1=table_1[!(table_1$battle_type=="BOOTCAMP" | table_1$battle_type=="SANDBOX" | table_1$battle_type=="TRAINING" | table_1$battle_type=="RATED_SANDBOX" | table_1$battle_type == "EPIC_RANDOM_TRAINING"),]

attach(table_1)
#select column of team size, battle type, battle duration, has legionnaire or not, team average PR, and battle result
average_selfteam_pr=average_pr_team_1+average_pr_team_2
team=select(table_1, battle_identifier, user_team_identifier, team_size, battle_type, battle_time, user_clan_identifier, whether_user_is_legionnaire, battle_result)
team=cbind(team, average_selfteam_pr)
team$clan_count=ave(team$user_clan_identifier, team$battle_identifier, team$user_team_identifier, FUN = function(x) sum(x!=0))
team$legionnaire_count=ave(team$whether_user_is_legionnaire,team$battle_identifier,team$user_team_identifier, FUN=function(x) sum(x!=0))
#delete duplicated data
team=team[!duplicated(team[,1:2]),]
bnext <- data.frame(c(tail(team$battle_identifier,-1),0),c(tail(team$average_selfteam_pr,-1),0))
team$average_enemy_pr=ifelse(team$battle_identifier==bnext$c.tail.team.battle_identifier...1...0., bnext$c.tail.team.average_selfteam_pr...1...0., 0)
bprior=data.frame(c(0,head(team$battle_identifier,-1)),c(0,head(team$average_selfteam_pr,-1)))
average_enemy_pr2=ifelse(team$battle_identifier==bprior$c.0..head.team.battle_identifier...1..,bprior$c.0..head.team.average_selfteam_pr...1.., 0)
team$average_enemy_pr=team$average_enemy_pr+average_enemy_pr2


team2=select(table_1,battle_identifier, user_team_identifier, user_skill_at_this_battle_pr)
team2$team_pr_sd=ave(team2$user_skill_at_this_battle_pr,team2$battle_identifier,team2$user_team_identifier,FUN=function(x) sd(x))
team2$team_pr_max=ave(team2$user_skill_at_this_battle_pr,team2$battle_identifier,team2$user_team_identifier,FUN=function(x) max(x))
team2$team_pr_min=ave(team2$user_skill_at_this_battle_pr,team2$battle_identifier,team2$user_team_identifier,FUN=function(x) min(x))
team2=team2[!duplicated(team2[,1:2]),]
toxictable=left_join(toxictable,team2,by=c("battle_identifier","user_team_identifier"))


detach(table_1)

#From table_2 find toxicity 
#Get automatic reported toxicity and set data type
AutoReport=transform(AutoReport, battle_id=as.numeric(as.character(battle_id)))
AutoReportTeam=left_join(AutoReport, table_1, by= c("battle_id"="battle_identifier","reported_user_id"="spa_id"))
AutoReportTeam=select(AutoReportTeam, behavior_type, battle_id, user_team_identifier,reported_user_id,reporter_id)
AutoReportTeam=na.omit(AutoReportTeam)

#get unique reported users and reproters in each team
AutoReportTeam$unique_reported_user=ave(AutoReportTeam$reported_user_id,AutoReportTeam$battle_id,AutoReportTeam$user_team_identifier,FUN=function(x) length(unique(x)))
AutoReportTeam$unique_reporter=ave(AutoReportTeam$reporter_id,AutoReportTeam$battle_id,AutoReportTeam$user_team_identifier,FUN=function(x) length(unique(x)))

#create a table with battle_id, team id, count of Inaction / Bot, 
#count of Unsportsmanlike Conduct, count of Inappropriate Behavior, 
#count of Offensive nickname, whether being reported, and the number of toxic behavior in the team

#get frequency based on battle_id, team id and behavior type
toxictable=as.data.frame(table(AutoReportTeam[,1:3]))
toxictable=toxictable[toxictable[,4]!=0,]
toxictable=toxictable[order(toxictable$battle_id),]

#split the data frame by bahvior type 
toxiclist=split(toxictable, as.factor(toxictable$behavior_type))

#make tables for each behavior type
Inaction_Bot=toxiclist$`Inaction / Bot`[,2:4]
names(Inaction_Bot)=c("battle_id","user_team_identifier", "Inaction/Bot")
Unsportsmanlike=toxiclist$`Unsportsmanlike Conduct`[,2:4]
names(Unsportsmanlike)=c("battle_id","user_team_identifier", "Unsportsmanlike")
Inapp=toxiclist$`Inappropriate Behavior`[,2:4]
names(Inapp)=c("battle_id","user_team_identifier", "Inappropriate")
Nickname=toxiclist$`Offensive nickname`[,2:4]
names(Nickname)=c("battle_id","user_team_identifier", "Nickname")

#combine 4 behavior type tables
toxictable=full_join(Inaction_Bot,Unsportsmanlike, by = c("battle_id","user_team_identifier"))
toxictable=full_join(toxictable,Inapp,by = c("battle_id","user_team_identifier"))
toxictable=full_join(toxictable,Nickname,by = c("battle_id","user_team_identifier"))
toxictable[is.na(toxictable)]<-0

#Add two columns, total number of toxic behavior in the team and binary varible of toxic behavior
toxictable$toxic<-rowSums(toxictable[,3:6])
toxictable$report[toxictable$toxic>0]<-1

AutoReportTeam=AutoReportTeam[,-1]
AutoReportTeam2=AutoReportTeam[!duplicated(AutoReportTeam[,3:4]),]
AutoReportTeam=AutoReportTeam[!duplicated(AutoReportTeam[,1:2]),]
AutoReportTeam=AutoReportTeam[,-c(3:4)]


#combine toxic table with AutoReportTeam table
toxictable=transform(toxictable, battle_id = as.numeric(as.character(battle_id)))
toxictable=transform(toxictable, user_team_identifier = as.numeric(as.character(user_team_identifier)))
toxictable=full_join(toxictable,AutoReportTeam,by=c("battle_id","user_team_identifier") )

#combine toxic table with team table
toxictable=full_join(team, toxictable, by= c("battle_identifier"="battle_id", "user_team_identifier"="user_team_identifier"))
toxictable[is.na(toxictable)]<-0
#toxictable is the table for H1 (except there is no clan involved data)
write.csv(toxictable,"toxictable.csv")

###get reciprocal 
AutoReport=AutoReport[order(AutoReport$battle_id),]
teamid=select(table_1, spa_id,battle_identifier, user_team_identifier)
AutoReport=transform(AutoReport, battle_id=as.numeric(as.character(battle_id)))
AutoReport=left_join(AutoReport,teamid,by=c("battle_id"="battle_identifier","reported_user_id"="spa_id"))
AutoReport=left_join(AutoReport,teamid,by=c("battle_id"="battle_identifier","reporter_id"="spa_id"))
AutoSwitch=data.frame(cbind(AutoReport$battle_id,AutoReport$reporter_id,AutoReport$reported_user_id))
names(AutoSwitch)=c("battle_id","reporter_id","reported_user_id")
reciprocal=semi_join(AutoReport,AutoSwitch,by=c("battle_id"="battle_id","reported_user_id"="reporter_id","reporter_id"="reported_user_id"))
reciprocal$selfteamreport=ifelse(reciprocal$user_team_identifier.x==reciprocal$user_team_identifier.y,1,0)
reciprocal$enemyreport=ifelse(reciprocal$user_team_identifier.x==reciprocal$user_team_identifier.y,0,1)
reciprocal$selfteamreport=ave(reciprocal$selfteamreport,reciprocal$battle_id,FUN=sum)
reciprocal$enemyreport=ave(reciprocal$enemyreport,reciprocal$battle_id,FUN=sum)
reciprocal=select(reciprocal,battle_id,user_team_identifier.x,selfteamreport, enemyreport)
reciprocal=reciprocal[!duplicated(reciprocal[,1:2]),]
toxictable=left_join(toxictable,reciprocal, by=c("battle_identifier"="battle_id","user_team_identifier"="user_team_identifier.x"))
toxictable[is.na(toxictable)]<-0


#get behavior type
ToxicPlayer=select(table_2,reported_user_id, time_stamp)
ToxicBeh=as.data.frame.matrix(table(ToxicPlayer[,1:2]))
ToxicBeh=data.frame(t(ToxicBeh))
ToxicBeh=tibble::rownames_to_column(ToxicBeh,var="user_id")
ToxicBeh=transform(ToxicBeh, user_id = as.numeric(as.character(user_id)))
ToxicBeh=left_join(table_1,ToxicBeh, by=c("spa_id"="user_id"))
ToxicBeh[is.na(ToxicBeh)]<-0

###
reported=semi_join(table_3, AutoReport, by=c("spa_key"="reported_user_id"))
nonreported=anti_join(table_3, AutoReport, by=c("spa_key"="reported_user_id"))
nonreported=nonreported[sample(1:nrow(nonreported),50918,replace=FALSE),]
reported$reported=rep(1,50918)
reported$sample=rep(0,50918)
nonreported$reported=rep(0,50918)
nonreported$sample=rep(1,50918)
reported$reporter=ifelse(reported$spa_key %in% AutoReport$reporter_id,1,0)
nonreported$reporter=ifelse(nonreported$spa_key %in% AutoReport$reporter_id,1,0)
sample=rbind(reported,nonreported)
ToxicBeh=left_join(sample,table_1,by=c("spa_key"="spa_id"))
##Table for RQ
write.csv(ToxicBeh,"ToxicBeh2.csv")

survey=read.csv("survey_base_NA_2018_4.4.18.csv")
surveytable=inner_join(table_1,survey, by=c("spa_id"="﻿spa_id"))
surveytable=left_join(surveytable,toxictable, by=c("battle_identifier"="battle_identifier","user_team_identifier"="user_team_identifier"))
write.csv(surveytable,"surveytable.csv")

toxictable=read.csv("toxictable.csv")
##get selfteam exposure
ToxicExp2=left_join(table_1, toxictable, by=c("battle_identifier","user_team_identifier"))
ToxicExp2=select(ToxicExp2,spa_id,battle_identifier, battle_type, battle_start_time,toxic)
ToxicExp2=transform(ToxicExp2,battle_start_time=as.character(battle_start_time))

#dtparts=as.data.frame(t(as.data.frame(strsplit(ToxicExp$battle_start_time,' '))))

ToxicExp=select(table_1,spa_id,user_skill_at_this_battle_pr,user_battle_count_at_time_battle,user_team_identifier,battle_identifier,battle_type,battle_start_time)

ToxicExp2$battle_start_time=as.Date(as.character(as.POSIXct(ToxicExp2$battle_start_time)))
ToxicExp=cbind(ToxicExp,table_1$user_battle_count_at_time_battle)
colnames(ToxicExp)[7]="battle_count"
ToxicExp=cbind(ToxicExp,table_1$user_skill_at_this_battle_pr)
colnames(ToxicExp)[8]="user_skill"





Day1=data.frame(ToxicExp2$spa_id)
Day1$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
Day1$selfteam=ave(Day1$selfteam,Day1$ToxicExp.spa_id,FUN=sum)
Day1$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day1$enemy=ave(Day1$enemy,Day1$ToxicExp.spa_id,FUN=sum)
Day1$battle=Day1$selfteam+Day1$enemy
Day1$before=ifelse(ToxicExp$time_stamp<=as.Date("03/01/2018",format="%m/%d/%Y"),0,1)

Day1$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day1$Ranked=ave(Day1$Ranked,Day1$ToxicExp2.spa_id,FUN=sum)
Day1$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day1$Regular=ave(Day1$Regular,Day1$ToxicExp2.spa_id,FUN=sum)
Day1$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day1$Sortie_2=ave(Day1$Sortie_2,Day1$ToxicExp2.spa_id,FUN=sum)
Day1$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day1$Fort=ave(Day1$Fort,Day1$ToxicExp2.spa_id,FUN=sum)
Day1$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day1$Epic=ave(Day1$Epic,Day1$ToxicExp2.spa_id,FUN=sum)
Day1$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day1$Cybersport=ave(Day1$Cybersport,Day1$ToxicExp2.spa_id,FUN=sum)

Day1$battle_count1=0
Day1$battle_count1=ifelse(ToxicExp$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),ToxicExp$battle_count,Day1$battle_count1)
Day1$battle_count1=ave(Day1$battle_count1,Day1$ToxicExp.spa_id,FUN=min)

Day1=Day1[!duplicated(Day1$ToxicExp2.spa_id),]
Day1[is.na(Day1)]<-1
#write.csv(Day1,"Day1.csv")

Day2=data.frame(ToxicExp2$spa_id)
Day2$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
Day2$selfteam=ave(Day2$selfteam,Day2$ToxicExp.spa_id,FUN=sum)
Day2$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day2$enemy=ave(Day2$enemy,Day2$ToxicExp.spa_id,FUN=sum)
Day2$battle=Day2$selfteam+Day2$enemy
Day2$before=ifelse(ToxicExp$time_stamp<=as.Date("03/02/2018",format="%m/%d/%Y"),0,1)

Day2$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day2$Ranked=ave(Day2$Ranked,Day2$ToxicExp2.spa_id,FUN=sum)
Day2$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day2$Regular=ave(Day2$Regular,Day2$ToxicExp2.spa_id,FUN=sum)
Day2$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day2$Sortie_2=ave(Day2$Sortie_2,Day2$ToxicExp2.spa_id,FUN=sum)
Day2$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day2$Fort=ave(Day2$Fort,Day2$ToxicExp2.spa_id,FUN=sum)
Day2$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day2$Epic=ave(Day2$Epic,Day2$ToxicExp2.spa_id,FUN=sum)
Day2$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day2$Cybersport=ave(Day2$Cybersport,Day2$ToxicExp2.spa_id,FUN=sum)

Day2$battle_count2=0
Day2$battle_count2=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),ToxicExp$battle_count,Day2$battle_count2)
Day2$battle_count2=ave(Day2$battle_count2,Day2$ToxicExp.spa_id,FUN=min)

Day2=Day2[!duplicated(Day2$ToxicExp2.spa_id),]
Day2[is.na(Day2)]<-1
#write.csv(Day2,"Day2.csv")

Day3=data.frame(ToxicExp2$spa_id)
Day3$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day3$selfteam=ave(Day1$selfteam,Day3$ToxicExp.spa_id,FUN=sum)
Day3$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day3$enemy=ave(Day3$enemy,Day3$ToxicExp.spa_id,FUN=sum)
Day3$battle=Day3$selfteam+Day3$enemy
Day3$before=ifelse(ToxicExp$time_stamp<=as.Date("03/03/2018",format="%m/%d/%Y"),0,1)

Day3$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day3$Ranked=ave(Day3$Ranked,Day3$ToxicExp2.spa_id,FUN=sum)
Day3$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day3$Regular=ave(Day3$Regular,Day3$ToxicExp2.spa_id,FUN=sum)
Day3$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day3$Sortie_2=ave(Day3$Sortie_2,Day3$ToxicExp2.spa_id,FUN=sum)
Day3$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day3$Fort=ave(Day3$Fort,Day3$ToxicExp2.spa_id,FUN=sum)
Day3$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day3$Epic=ave(Day3$Epic,Day3$ToxicExp2.spa_id,FUN=sum)
Day3$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day3$Cybersport=ave(Day3$Cybersport,Day3$ToxicExp2.spa_id,FUN=sum)

Day3$battle_count3=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day3$battle_count3=ave(Day3$battle_count3,Day3$ToxicExp.spa_id,FUN=min)

Day3=Day3[!duplicated(Day3$ToxicExp2.spa_id),]
Day3[is.na(Day3)]<-1
#write.csv(Day3,"Day3.csv")

Day4=data.frame(ToxicExp2$spa_id)
Day4$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day4$selfteam=ave(Day4$selfteam,Day4$ToxicExp.spa_id,FUN=sum)
Day4$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day4$enemy=ave(Day4$enemy,Day4$ToxicExp.spa_id,FUN=sum)
Day4$battle=Day4$selfteam+Day4$enemy
Day4$before=ifelse(ToxicExp$time_stamp<=as.Date("03/04/2018",format="%m/%d/%Y"),0,1)

Day4$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day4$Ranked=ave(Day4$Ranked,Day4$ToxicExp2.spa_id,FUN=sum)
Day4$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day4$Regular=ave(Day4$Regular,Day4$ToxicExp2.spa_id,FUN=sum)
Day4$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day4$Sortie_2=ave(Day4$Sortie_2,Day4$ToxicExp2.spa_id,FUN=sum)
Day4$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day4$Fort=ave(Day4$Fort,Day4$ToxicExp2.spa_id,FUN=sum)
Day4$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day4$Epic=ave(Day4$Epic,Day4$ToxicExp2.spa_id,FUN=sum)
Day4$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day4$Cybersport=ave(Day4$Cybersport,Day4$ToxicExp2.spa_id,FUN=sum)

Day4$battle_count4=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day4$battle_count4=ave(Day4$battle_count4,Day4$ToxicExp.spa_id,FUN=min)

Day4=Day4[!duplicated(Day4$ToxicExp2.spa_id),]
Day4[is.na(Day4)]<-1
#write.csv(Day4,"Day4.csv")

Day5=data.frame(ToxicExp2$spa_id)
Day5$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day5$selfteam=ave(Day5$selfteam,Day5$ToxicExp.spa_id,FUN=sum)
Day5$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day5$enemy=ave(Day5$enemy,Day5$ToxicExp.spa_id,FUN=sum)
Day5$battle=Day5$selfteam+Day5$enemy
Day5$before=ifelse(ToxicExp$time_stamp<=as.Date("03/05/2018",format="%m/%d/%Y"),0,1)

Day5$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day5$Ranked=ave(Day5$Ranked,Day5$ToxicExp2.spa_id,FUN=sum)
Day5$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day5$Regular=ave(Day5$Regular,Day5$ToxicExp2.spa_id,FUN=sum)
Day5$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day5$Sortie_2=ave(Day5$Sortie_2,Day5$ToxicExp2.spa_id,FUN=sum)
Day5$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day5$Fort=ave(Day5$Fort,Day5$ToxicExp2.spa_id,FUN=sum)
Day5$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day5$Epic=ave(Day5$Epic,Day5$ToxicExp2.spa_id,FUN=sum)
Day5$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day5$Cybersport=ave(Day5$Cybersport,Day5$ToxicExp2.spa_id,FUN=sum)

Day5$battle_count5=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day5$battle_count5=ave(Day5$battle_count5,Day5$ToxicExp.spa_id,FUN=min)

Day5=Day5[!duplicated(Day5$ToxicExp2.spa_id),]
Day5[is.na(Day5)]<-1
#write.csv(Day5,"Day5.csv")

Day6=data.frame(ToxicExp2$spa_id)
Day6$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day6$selfteam=ave(Day6$selfteam,Day6$ToxicExp.spa_id,FUN=sum)
Day6$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day6$enemy=ave(Day6$enemy,Day6$ToxicExp.spa_id,FUN=sum)
Day6$battle=Day6$selfteam+Day6$enemy
Day6$before=ifelse(ToxicExp$time_stamp<=as.Date("03/06/2018",format="%m/%d/%Y"),0,1)

Day6$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day6$Ranked=ave(Day6$Ranked,Day6$ToxicExp2.spa_id,FUN=sum)
Day6$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day6$Regular=ave(Day6$Regular,Day6$ToxicExp2.spa_id,FUN=sum)
Day6$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day6$Sortie_2=ave(Day6$Sortie_2,Day6$ToxicExp2.spa_id,FUN=sum)
Day6$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day6$Fort=ave(Day6$Fort,Day6$ToxicExp2.spa_id,FUN=sum)
Day6$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day6$Epic=ave(Day6$Epic,Day6$ToxicExp2.spa_id,FUN=sum)
Day6$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day6$Cybersport=ave(Day6$Cybersport,Day6$ToxicExp2.spa_id,FUN=sum)

Day6$battle_count6=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day6$battle_count6=ave(Day6$battle_count6,Day6$ToxicExp.spa_id,FUN=min)

Day6=Day6[!duplicated(Day6$ToxicExp2.spa_id),]
Day6[is.na(Day6)]<-1
#write.csv(Day6,"Day6.csv")

Day7=data.frame(ToxicExp2$spa_id)
Day7$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day7$selfteam=ave(Day7$selfteam,Day7$ToxicExp.spa_id,FUN=sum)
Day7$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day7$enemy=ave(Day7$enemy,Day7$ToxicExp.spa_id,FUN=sum)
Day7$battle=Day7$selfteam+Day7$enemy
Day7$before=ifelse(ToxicExp$time_stamp<=as.Date("03/07/2018",format="%m/%d/%Y"),0,1)

Day7$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day7$Ranked=ave(Day7$Ranked,Day7$ToxicExp2.spa_id,FUN=sum)
Day7$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day7$Regular=ave(Day7$Regular,Day7$ToxicExp2.spa_id,FUN=sum)
Day7$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day7$Sortie_2=ave(Day7$Sortie_2,Day7$ToxicExp2.spa_id,FUN=sum)
Day7$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day7$Fort=ave(Day7$Fort,Day7$ToxicExp2.spa_id,FUN=sum)
Day7$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day7$Epic=ave(Day7$Epic,Day7$ToxicExp2.spa_id,FUN=sum)
Day7$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day7$Cybersport=ave(Day7$Cybersport,Day7$ToxicExp2.spa_id,FUN=sum)

Day7$battle_count7=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day7$battle_count7=ave(Day7$battle_count7,Day7$ToxicExp.spa_id,FUN=min)

Day7[is.na(Day7)]<-1
Day7=Day7[!duplicated(Day7$ToxicExp2.spa_id),]
#write.csv(Day7,"Day7.csv")

Day8=data.frame(ToxicExp2$spa_id)
Day8$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day8$selfteam=ave(Day8$selfteam,Day8$ToxicExp.spa_id,FUN=sum)
Day8$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day8$enemy=ave(Day8$enemy,Day8$ToxicExp.spa_id,FUN=sum)
Day8$battle=Day8$selfteam+Day8$enemy
Day8$before=ifelse(ToxicExp$time_stamp<=as.Date("03/08/2018",format="%m/%d/%Y"),0,1)

Day8$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day8$Ranked=ave(Day8$Ranked,Day8$ToxicExp2.spa_id,FUN=sum)
Day8$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day8$Regular=ave(Day8$Regular,Day8$ToxicExp2.spa_id,FUN=sum)
Day8$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day8$Sortie_2=ave(Day8$Sortie_2,Day8$ToxicExp2.spa_id,FUN=sum)
Day8$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day8$Fort=ave(Day8$Fort,Day8$ToxicExp2.spa_id,FUN=sum)
Day8$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day8$Epic=ave(Day8$Epic,Day8$ToxicExp2.spa_id,FUN=sum)
Day8$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day8$Cybersport=ave(Day8$Cybersport,Day8$ToxicExp2.spa_id,FUN=sum)

Day8$battle_count8=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day8$battle_count8=ave(Day8$battle_count8,Day8$ToxicExp.spa_id,FUN=min)

Day8=Day8[!duplicated(Day8$ToxicExp2.spa_id),]
Day8[is.na(Day8)]<-1
#write.csv(Day8,"Day8.csv")

Day9=data.frame(ToxicExp2$spa_id)
Day9$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day9$selfteam=ave(Day9$selfteam,Day9$ToxicExp.spa_id,FUN=sum)
Day9$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day9$enemy=ave(Day9$enemy,Day9$ToxicExp.spa_id,FUN=sum)
Day9$battle=Day9$selfteam+Day9$enemy
Day9$before=ifelse(ToxicExp$time_stamp<=as.Date("03/09/2018",format="%m/%d/%Y"),0,1)

Day9$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day9$Ranked=ave(Day9$Ranked,Day9$ToxicExp2.spa_id,FUN=sum)
Day9$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day9$Regular=ave(Day9$Regular,Day9$ToxicExp2.spa_id,FUN=sum)
Day9$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day9$Sortie_2=ave(Day9$Sortie_2,Day9$ToxicExp2.spa_id,FUN=sum)
Day9$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day9$Fort=ave(Day9$Fort,Day9$ToxicExp2.spa_id,FUN=sum)
Day9$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day9$Epic=ave(Day9$Epic,Day9$ToxicExp2.spa_id,FUN=sum)
Day9$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day9$Cybersport=ave(Day9$Cybersport,Day9$ToxicExp2.spa_id,FUN=sum)
Day9$Cybersport=ave(Day9$Cybersport,Day9$ToxicExp.spa_id,FUN=sum)

Day9$battle_count9=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day9$battle_count9=ave(Day9$battle_count9,Day9$ToxicExp.spa_id,FUN=min)

Day9=Day9[!duplicated(Day9$ToxicExp2.spa_id),]
Day9[is.na(Day9)]<-1
#write.csv(Day9,"Day9.csv")

Day10=data.frame(ToxicExp2$spa_id)
Day10$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day10$selfteam=ave(Day10$selfteam,Day10$ToxicExp.spa_id,FUN=sum)
Day10$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day10$enemy=ave(Day10$enemy,Day10$ToxicExp.spa_id,FUN=sum)
Day10$battle=Day10$selfteam+Day10$enemy
Day10$before=ifelse(ToxicExp$time_stamp<=as.Date("03/10/2018",format="%m/%d/%Y"),0,1)

Day10$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day10$Ranked=ave(Day10$Ranked,Day10$ToxicExp2.spa_id,FUN=sum)
Day10$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day10$Regular=ave(Day10$Regular,Day10$ToxicExp2.spa_id,FUN=sum)
Day10$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day10$Sortie_2=ave(Day10$Sortie_2,Day10$ToxicExp2.spa_id,FUN=sum)
Day10$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day10$Fort=ave(Day10$Fort,Day10$ToxicExp2.spa_id,FUN=sum)
Day10$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day10$Epic=ave(Day10$Epic,Day10$ToxicExp2.spa_id,FUN=sum)
Day10$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day10$Cybersport=ave(Day10$Cybersport,Day10$ToxicExp2.spa_id,FUN=sum)


Day10$battle_count10=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day10$battle_count10=ave(Day10$battle_count10,Day10$ToxicExp.spa_id,FUN=min)

Day10=Day10[!duplicated(Day10$ToxicExp2.spa_id),]
Day10[is.na(Day10)]<-1
#write.csv(Day10,"Day10.csv")

Day11=data.frame(ToxicExp2$spa_id)
Day11$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day11$selfteam=ave(Day11$selfteam,Day11$ToxicExp.spa_id,FUN=sum)
Day11$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day11$enemy=ave(Day11$enemy,Day11$ToxicExp.spa_id,FUN=sum)
Day11$battle=Day11$selfteam+Day11$enemy
Day11$before=ifelse(ToxicExp$time_stamp<=as.Date("03/11/2018",format="%m/%d/%Y"),0,1)

Day11$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day11$Ranked=ave(Day11$Ranked,Day11$ToxicExp2.spa_id,FUN=sum)
Day11$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day11$Regular=ave(Day11$Regular,Day11$ToxicExp2.spa_id,FUN=sum)
Day11$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day11$Sortie_2=ave(Day11$Sortie_2,Day11$ToxicExp2.spa_id,FUN=sum)
Day11$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day11$Fort=ave(Day11$Fort,Day11$ToxicExp2.spa_id,FUN=sum)
Day11$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day11$Epic=ave(Day11$Epic,Day11$ToxicExp2.spa_id,FUN=sum)
Day11$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day11$Cybersport=ave(Day11$Cybersport,Day11$ToxicExp2.spa_id,FUN=sum)

Day11$battle_count11=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day11$battle_count11=ave(Day11$battle_count11,Day11$ToxicExp.spa_id,FUN=min)

Day11=Day11[!duplicated(Day11$ToxicExp2.spa_id),]
Day11[is.na(Day11)]<-1
#write.csv(Day11,"Day11.csv")

Day12=data.frame(ToxicExp2$spa_id)
Day12$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day12$selfteam=ave(Day12$selfteam,Day12$ToxicExp.spa_id,FUN=sum)
Day12$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day12$enemy=ave(Day12$enemy,Day12$ToxicExp.spa_id,FUN=sum)
Day12$battle=Day12$selfteam+Day12$enemy
Day12$before=ifelse(ToxicExp$time_stamp<=as.Date("03/12/2018",format="%m/%d/%Y"),0,1)

Day12$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day12$Ranked=ave(Day12$Ranked,Day12$ToxicExp2.spa_id,FUN=sum)
Day12$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day12$Regular=ave(Day12$Regular,Day12$ToxicExp2.spa_id,FUN=sum)
Day12$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day12$Sortie_2=ave(Day12$Sortie_2,Day12$ToxicExp2.spa_id,FUN=sum)
Day12$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day12$Fort=ave(Day12$Fort,Day12$ToxicExp2.spa_id,FUN=sum)
Day12$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day12$Epic=ave(Day12$Epic,Day12$ToxicExp2.spa_id,FUN=sum)
Day12$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day12$Cybersport=ave(Day12$Cybersport,Day12$ToxicExp2.spa_id,FUN=sum)

Day12$battle_count12=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day12$battle_count12=ave(Day12$battle_count12,Day12$ToxicExp.spa_id,FUN=min)

Day12=Day12[!duplicated(Day12$ToxicExp2.spa_id),]
Day12[is.na(Day12)]<-1
#write.csv(Day12,"Day12.csv")

Day13=data.frame(ToxicExp2$spa_id)
Day13$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day13$selfteam=ave(Day13$selfteam,Day13$ToxicExp.spa_id,FUN=sum)
Day13$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day13$enemy=ave(Day13$enemy,Day13$ToxicExp.spa_id,FUN=sum)
Day13$battle=Day13$selfteam+Day13$enemy
Day13$before=ifelse(ToxicExp$time_stamp<=as.Date("03/13/2018",format="%m/%d/%Y"),0,1)

Day13$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day13$Ranked=ave(Day13$Ranked,Day13$ToxicExp2.spa_id,FUN=sum)
Day13$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day13$Regular=ave(Day13$Regular,Day13$ToxicExp2.spa_id,FUN=sum)
Day13$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day13$Sortie_2=ave(Day13$Sortie_2,Day13$ToxicExp2.spa_id,FUN=sum)
Day13$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day13$Fort=ave(Day13$Fort,Day13$ToxicExp2.spa_id,FUN=sum)
Day13$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day13$Epic=ave(Day13$Epic,Day13$ToxicExp2.spa_id,FUN=sum)
Day13$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day13$Cybersport=ave(Day13$Cybersport,Day13$ToxicExp2.spa_id,FUN=sum)

Day13$battle_count13=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day13$battle_count13=ave(Day13$battle_count13,Day13$ToxicExp.spa_id,FUN=min)

Day13=Day13[!duplicated(Day13$ToxicExp2.spa_id),]
Day13[is.na(Day13)]<-1
#write.csv(Day13,"Day13.csv")

Day14=data.frame(ToxicExp2$spa_id)
Day14$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day14$selfteam=ave(Day14$selfteam,Day14$ToxicExp.spa_id,FUN=sum)
Day14$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day14$enemy=ave(Day14$enemy,Day14$ToxicExp.spa_id,FUN=sum)
Day14$battle=Day14$selfteam+Day14$enemy
Day14$before=ifelse(ToxicExp$time_stamp<=as.Date("03/14/2018",format="%m/%d/%Y"),0,1)

Day14$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day14$Ranked=ave(Day14$Ranked,Day14$ToxicExp2.spa_id,FUN=sum)
Day14$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day14$Regular=ave(Day14$Regular,Day14$ToxicExp2.spa_id,FUN=sum)
Day14$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day14$Sortie_2=ave(Day14$Sortie_2,Day14$ToxicExp2.spa_id,FUN=sum)
Day14$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day14$Fort=ave(Day14$Fort,Day14$ToxicExp2.spa_id,FUN=sum)
Day14$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day14$Epic=ave(Day14$Epic,Day14$ToxicExp2.spa_id,FUN=sum)
Day14$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day14$Cybersport=ave(Day14$Cybersport,Day14$ToxicExp2.spa_id,FUN=sum)

Day14$battle_count14=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day14$battle_count14=ave(Day14$battle_count14,Day14$ToxicExp.spa_id,FUN=min)

Day14=Day14[!duplicated(Day14$ToxicExp2.spa_id),]
Day14[is.na(Day14)]<-1
#write.csv(Day14,"Day14.csv")

Day15=data.frame(ToxicExp2$spa_id)
Day15$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day15$selfteam=ave(Day15$selfteam,Day15$ToxicExp.spa_id,FUN=sum)
Day15$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day15$enemy=ave(Day15$enemy,Day15$ToxicExp.spa_id,FUN=sum)
Day15$battle=Day15$selfteam+Day15$enemy
Day15$before=ifelse(ToxicExp$time_stamp<=as.Date("03/15/2018",format="%m/%d/%Y"),0,1)

Day15$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day15$Ranked=ave(Day15$Ranked,Day15$ToxicExp2.spa_id,FUN=sum)
Day15$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day15$Regular=ave(Day15$Regular,Day15$ToxicExp2.spa_id,FUN=sum)
Day15$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day15$Sortie_2=ave(Day15$Sortie_2,Day15$ToxicExp2.spa_id,FUN=sum)
Day15$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day15$Fort=ave(Day15$Fort,Day15$ToxicExp2.spa_id,FUN=sum)
Day15$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day15$Epic=ave(Day15$Epic,Day15$ToxicExp2.spa_id,FUN=sum)
Day15$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day15$Cybersport=ave(Day15$Cybersport,Day15$ToxicExp2.spa_id,FUN=sum)

Day15$battle_count15=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day15$battle_count15=ave(Day15$battle_count15,Day15$ToxicExp.spa_id,FUN=min)

Day15=Day15[!duplicated(Day15$ToxicExp2.spa_id),]
Day15[is.na(Day15)]<-1
#write.csv(Day15,"Day15.csv")

Day16=data.frame(ToxicExp2$spa_id)
Day16$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day16$selfteam=ave(Day16$selfteam,Day16$ToxicExp.spa_id,FUN=sum)
Day16$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day16$enemy=ave(Day16$enemy,Day16$ToxicExp.spa_id,FUN=sum)
Day16$battle=Day16$selfteam+Day16$enemy
Day16$before=ifelse(ToxicExp$time_stamp<=as.Date("03/16/2018",format="%m/%d/%Y"),0,1)

Day16$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day16$Ranked=ave(Day16$Ranked,Day16$ToxicExp2.spa_id,FUN=sum)
Day16$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day16$Regular=ave(Day16$Regular,Day16$ToxicExp2.spa_id,FUN=sum)
Day16$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day16$Sortie_2=ave(Day16$Sortie_2,Day16$ToxicExp2.spa_id,FUN=sum)
Day16$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day16$Fort=ave(Day16$Fort,Day16$ToxicExp2.spa_id,FUN=sum)
Day16$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day16$Epic=ave(Day16$Epic,Day16$ToxicExp2.spa_id,FUN=sum)
Day16$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day16$Cybersport=ave(Day16$Cybersport,Day16$ToxicExp2.spa_id,FUN=sum)

Day16$battle_count16=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day16$battle_count16=ave(Day16$battle_count16,Day16$ToxicExp.spa_id,FUN=min)

Day16=Day16[!duplicated(Day16$ToxicExp2.spa_id),]
Day16[is.na(Day16)]<-1
#write.csv(Day16,"Day16.csv")

Day17=data.frame(ToxicExp2$spa_id)
Day17$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day17$selfteam=ave(Day17$selfteam,Day17$ToxicExp.spa_id,FUN=sum)
Day17$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day17$enemy=ave(Day17$enemy,Day17$ToxicExp.spa_id,FUN=sum)
Day17$battle=Day17$selfteam+Day17$enemy
Day17$before=ifelse(ToxicExp$time_stamp<=as.Date("03/17/2018",format="%m/%d/%Y"),0,1)

Day17$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day17$Ranked=ave(Day17$Ranked,Day17$ToxicExp2.spa_id,FUN=sum)
Day17$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day17$Regular=ave(Day17$Regular,Day17$ToxicExp2.spa_id,FUN=sum)
Day17$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day17$Sortie_2=ave(Day17$Sortie_2,Day17$ToxicExp2.spa_id,FUN=sum)
Day17$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day17$Fort=ave(Day17$Fort,Day17$ToxicExp2.spa_id,FUN=sum)
Day17$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day17$Epic=ave(Day17$Epic,Day17$ToxicExp2.spa_id,FUN=sum)
Day17$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day17$Cybersport=ave(Day17$Cybersport,Day17$ToxicExp2.spa_id,FUN=sum)

Day17$battle_count17=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day17$battle_count17=ave(Day17$battle_count17,Day17$ToxicExp.spa_id,FUN=min)

Day17=Day17[!duplicated(Day17$ToxicExp2.spa_id),]
Day17[is.na(Day17)]<-1
#write.csv(Day17,"Day17.csv")

Day18=data.frame(ToxicExp2$spa_id)
Day18$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day18$selfteam=ave(Day18$selfteam,Day18$ToxicExp.spa_id,FUN=sum)
Day18$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day18$enemy=ave(Day18$enemy,Day18$ToxicExp.spa_id,FUN=sum)
Day18$battle=Day18$selfteam+Day18$enemy
Day18$before=ifelse(ToxicExp$time_stamp<=as.Date("03/18/2018",format="%m/%d/%Y"),0,1)

Day18$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day18$Ranked=ave(Day18$Ranked,Day18$ToxicExp2.spa_id,FUN=sum)
Day18$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day18$Regular=ave(Day18$Regular,Day18$ToxicExp2.spa_id,FUN=sum)
Day18$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/81/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day18$Sortie_2=ave(Day18$Sortie_2,Day18$ToxicExp2.spa_id,FUN=sum)
Day18$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day18$Fort=ave(Day18$Fort,Day18$ToxicExp2.spa_id,FUN=sum)
Day18$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day18$Epic=ave(Day18$Epic,Day18$ToxicExp2.spa_id,FUN=sum)
Day18$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day18$Cybersport=ave(Day18$Cybersport,Day18$ToxicExp2.spa_id,FUN=sum)

Day18$battle_count18=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day18$battle_count18=ave(Day18$battle_count18,Day18$ToxicExp.spa_id,FUN=min)

Day18=Day18[!duplicated(Day18$ToxicExp2.spa_id),]
Day18[is.na(Day18)]<-1
#write.csv(Day18,"Day18.csv")

Day19=data.frame(ToxicExp2$spa_id)
Day19$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day19$selfteam=ave(Day19$selfteam,Day19$ToxicExp.spa_id,FUN=sum)
Day19$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day19$enemy=ave(Day19$enemy,Day19$ToxicExp.spa_id,FUN=sum)
Day19$battle=Day19$selfteam+Day19$enemy
Day19$before=ifelse(ToxicExp$time_stamp<=as.Date("03/19/2018",format="%m/%d/%Y"),0,1)

Day19$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day19$Ranked=ave(Day19$Ranked,Day19$ToxicExp2.spa_id,FUN=sum)
Day19$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day19$Regular=ave(Day19$Regular,Day19$ToxicExp2.spa_id,FUN=sum)
Day19$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day19$Sortie_2=ave(Day19$Sortie_2,Day19$ToxicExp2.spa_id,FUN=sum)
Day19$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day19$Fort=ave(Day19$Fort,Day19$ToxicExp2.spa_id,FUN=sum)
Day19$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day19$Epic=ave(Day19$Epic,Day19$ToxicExp2.spa_id,FUN=sum)
Day19$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day19$Cybersport=ave(Day19$Cybersport,Day19$ToxicExp2.spa_id,FUN=sum)

Day19$battle_count19=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day19$battle_count19=ave(Day19$battle_count19,Day19$ToxicExp.spa_id,FUN=min)

Day19=Day19[!duplicated(Day19$ToxicExp2.spa_id),]
Day19[is.na(Day19)]<-1
#write.csv(Day19,"Day19.csv")

Day20=data.frame(ToxicExp2$spa_id)
Day20$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day20$selfteam=ave(Day20$selfteam,Day20$ToxicExp.spa_id,FUN=sum)
Day20$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day20$enemy=ave(Day20$enemy,Day20$ToxicExp.spa_id,FUN=sum)
Day20$battle=Day20$selfteam+Day20$enemy
Day20$before=ifelse(ToxicExp$time_stamp<=as.Date("03/20/2018",format="%m/%d/%Y"),0,1)

Day20$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day20$Ranked=ave(Day20$Ranked,Day20$ToxicExp2.spa_id,FUN=sum)
Day20$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day20$Regular=ave(Day20$Regular,Day20$ToxicExp2.spa_id,FUN=sum)
Day20$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day20$Sortie_2=ave(Day20$Sortie_2,Day20$ToxicExp2.spa_id,FUN=sum)
Day20$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day20$Fort=ave(Day20$Fort,Day20$ToxicExp2.spa_id,FUN=sum)
Day20$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day20$Epic=ave(Day20$Epic,Day20$ToxicExp2.spa_id,FUN=sum)
Day20$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day20$Cybersport=ave(Day20$Cybersport,Day20$ToxicExp2.spa_id,FUN=sum)

Day20$battle_count20=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day20$battle_count20=ave(Day20$battle_count20,Day20$ToxicExp.spa_id,FUN=min)

Day20=Day20[!duplicated(Day20$ToxicExp2.spa_id),]
Day20[is.na(Day20)]<-1
#write.csv(Day20,"Day20.csv")

Day21=data.frame(ToxicExp2$spa_id)
Day21$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day21$selfteam=ave(Day21$selfteam,Day21$ToxicExp.spa_id,FUN=sum)
Day21$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day21$enemy=ave(Day21$enemy,Day21$ToxicExp.spa_id,FUN=sum)
Day21$battle=Day21$selfteam+Day21$enemy
Day21$before=ifelse(ToxicExp$time_stamp<=as.Date("03/21/2018",format="%m/%d/%Y"),0,1)

Day21$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day21$Ranked=ave(Day21$Ranked,Day21$ToxicExp2.spa_id,FUN=sum)
Day21$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day21$Regular=ave(Day21$Regular,Day21$ToxicExp2.spa_id,FUN=sum)
Day21$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day21$Sortie_2=ave(Day21$Sortie_2,Day21$ToxicExp2.spa_id,FUN=sum)
Day21$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day21$Fort=ave(Day21$Fort,Day21$ToxicExp2.spa_id,FUN=sum)
Day21$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day21$Epic=ave(Day21$Epic,Day21$ToxicExp2.spa_id,FUN=sum)
Day21$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day21$Cybersport=ave(Day21$Cybersport,Day21$ToxicExp2.spa_id,FUN=sum)

Day21$battle_count21=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day21$battle_count21=ave(Day21$battle_count21,Day21$ToxicExp.spa_id,FUN=min)

Day21=Day21[!duplicated(Day21$ToxicExp2.spa_id),]
Day21[is.na(Day21)]<-1
#write.csv(Day21,"Day21.csv")

Day22=data.frame(ToxicExp2$spa_id)
Day22$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day22$selfteam=ave(Day22$selfteam,Day22$ToxicExp.spa_id,FUN=sum)
Day22$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day22$enemy=ave(Day22$enemy,Day22$ToxicExp.spa_id,FUN=sum)
Day22$battle=Day22$selfteam+Day22$enemy
Day22$before=ifelse(ToxicExp$time_stamp<=as.Date("03/22/2018",format="%m/%d/%Y"),0,1)


Day22$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day22$Ranked=ave(Day22$Ranked,Day22$ToxicExp2.spa_id,FUN=sum)
Day22$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day22$Regular=ave(Day22$Regular,Day22$ToxicExp2.spa_id,FUN=sum)
Day22$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day22$Sortie_2=ave(Day22$Sortie_2,Day22$ToxicExp2.spa_id,FUN=sum)
Day22$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day22$Fort=ave(Day22$Fort,Day22$ToxicExp2.spa_id,FUN=sum)
Day22$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day22$Epic=ave(Day22$Epic,Day22$ToxicExp2.spa_id,FUN=sum)
Day22$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day22$Cybersport=ave(Day22$Cybersport,Day22$ToxicExp2.spa_id,FUN=sum)

Day22$battle_count22=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day22$battle_count22=ave(Day22$battle_count22,Day22$ToxicExp.spa_id,FUN=min)

Day22=Day22[!duplicated(Day22$ToxicExp2.spa_id),]
Day22[is.na(Day22)]<-1
#write.csv(Day22,"Day22.csv")

Day23=data.frame(ToxicExp2$spa_id)
Day23$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day23$selfteam=ave(Day23$selfteam,Day23$ToxicExp.spa_id,FUN=sum)
Day23$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day23$enemy=ave(Day23$enemy,Day23$ToxicExp.spa_id,FUN=sum)
Day23$battle=Day23$selfteam+Day23$enemy
Day23$before=ifelse(ToxicExp$time_stamp<=as.Date("03/23/2018",format="%m/%d/%Y"),0,1)

Day23$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day23$Ranked=ave(Day23$Ranked,Day23$ToxicExp2.spa_id,FUN=sum)
Day23$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day23$Regular=ave(Day23$Regular,Day23$ToxicExp2.spa_id,FUN=sum)
Day23$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day23$Sortie_2=ave(Day23$Sortie_2,Day23$ToxicExp2.spa_id,FUN=sum)
Day23$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day23$Fort=ave(Day23$Fort,Day23$ToxicExp2.spa_id,FUN=sum)
Day23$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day23$Epic=ave(Day23$Epic,Day23$ToxicExp2.spa_id,FUN=sum)
Day23$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day23$Cybersport=ave(Day23$Cybersport,Day23$ToxicExp2.spa_id,FUN=sum)

Day23$battle_count23=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day23$battle_count23=ave(Day23$battle_count23,Day23$ToxicExp.spa_id,FUN=min)

Day23=Day23[!duplicated(Day23$ToxicExp2.spa_id),]
Day23[is.na(Day23)]<-1
#write.csv(Day23,"Day23.csv")

Day24=data.frame(ToxicExp2$spa_id)
Day24$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day24$selfteam=ave(Day24$selfteam,Day24$ToxicExp.spa_id,FUN=sum)
Day24$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day24$enemy=ave(Day24$enemy,Day24$ToxicExp.spa_id,FUN=sum)
Day24$battle=Day24$selfteam+Day24$enemy
Day24$before=ifelse(ToxicExp$time_stamp<=as.Date("03/24/2018",format="%m/%d/%Y"),0,1)

Day24$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day24$Ranked=ave(Day24$Ranked,Day24$ToxicExp2.spa_id,FUN=sum)
Day24$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day24$Regular=ave(Day24$Regular,Day24$ToxicExp2.spa_id,FUN=sum)
Day24$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day24$Sortie_2=ave(Day24$Sortie_2,Day24$ToxicExp2.spa_id,FUN=sum)
Day24$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day24$Fort=ave(Day24$Fort,Day24$ToxicExp2.spa_id,FUN=sum)
Day24$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day24$Epic=ave(Day24$Epic,Day24$ToxicExp2.spa_id,FUN=sum)
Day24$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day24$Cybersport=ave(Day24$Cybersport,Day24$ToxicExp2.spa_id,FUN=sum)

Day24$battle_count24=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day24$battle_count24=ave(Day24$battle_count24,Day24$ToxicExp.spa_id,FUN=min)

Day24=Day24[!duplicated(Day24$ToxicExp2.spa_id),]
Day24[is.na(Day24)]<-1
#write.csv(Day24,"Day24.csv")

Day25=data.frame(ToxicExp2$spa_id)
Day25$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day25$selfteam=ave(Day25$selfteam,Day25$ToxicExp.spa_id,FUN=sum)
Day25$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day25$enemy=ave(Day25$enemy,Day25$ToxicExp.spa_id,FUN=sum)
Day25$battle=Day25$selfteam+Day25$enemy
Day25$before=ifelse(ToxicExp$time_stamp<=as.Date("03/25/2018",format="%m/%d/%Y"),0,1)

Day25$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day25$Ranked=ave(Day25$Ranked,Day25$ToxicExp2.spa_id,FUN=sum)
Day25$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day25$Regular=ave(Day25$Regular,Day25$ToxicExp2.spa_id,FUN=sum)
Day25$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day25$Sortie_2=ave(Day25$Sortie_2,Day25$ToxicExp2.spa_id,FUN=sum)
Day25$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day25$Fort=ave(Day25$Fort,Day25$ToxicExp2.spa_id,FUN=sum)
Day25$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day25$Epic=ave(Day25$Epic,Day25$ToxicExp2.spa_id,FUN=sum)
Day25$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day25$Cybersport=ave(Day25$Cybersport,Day25$ToxicExp2.spa_id,FUN=sum)

Day25$battle_count25=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day25$battle_count25=ave(Day25$battle_count25,Day25$ToxicExp.spa_id,FUN=min)

Day25=Day25[!duplicated(Day25$ToxicExp2.spa_id),]
Day25[is.na(Day25)]<-1
#write.csv(Day25,"Day25.csv")

Day26=data.frame(ToxicExp2$spa_id)
Day26$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day26$selfteam=ave(Day26$selfteam,Day26$ToxicExp.spa_id,FUN=sum)
Day26$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day26$enemy=ave(Day26$enemy,Day26$ToxicExp.spa_id,FUN=sum)
Day26$battle=Day26$selfteam+Day26$enemy
Day26$before=ifelse(ToxicExp$time_stamp<=as.Date("03/26/2018",format="%m/%d/%Y"),0,1)

Day26$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day26$Ranked=ave(Day26$Ranked,Day26$ToxicExp2.spa_id,FUN=sum)
Day26$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day26$Regular=ave(Day26$Regular,Day26$ToxicExp2.spa_id,FUN=sum)
Day26$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day26$Sortie_2=ave(Day26$Sortie_2,Day26$ToxicExp2.spa_id,FUN=sum)
Day26$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day26$Fort=ave(Day26$Fort,Day26$ToxicExp2.spa_id,FUN=sum)
Day26$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day26$Epic=ave(Day26$Epic,Day26$ToxicExp2.spa_id,FUN=sum)
Day26$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day26$Cybersport=ave(Day26$Cybersport,Day26$ToxicExp2.spa_id,FUN=sum)

Day26$battle_count26=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day26$battle_count26=ave(Day26$battle_count26,Day26$ToxicExp.spa_id,FUN=min)

Day26=Day26[!duplicated(Day26$ToxicExp2.spa_id),]
Day26[is.na(Day26)]<-1
#write.csv(Day26,"Day26.csv")

Day27=data.frame(ToxicExp2$spa_id)
Day27$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day27$selfteam=ave(Day27$selfteam,Day27$ToxicExp.spa_id,FUN=sum)
Day27$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day27$enemy=ave(Day27$enemy,Day27$ToxicExp.spa_id,FUN=sum)
Day27$battle=Day27$selfteam+Day27$enemy
Day27$before=ifelse(ToxicExp$time_stamp<=as.Date("03/27/2018",format="%m/%d/%Y"),0,1)

Day27$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day27$Ranked=ave(Day27$Ranked,Day27$ToxicExp2.spa_id,FUN=sum)
Day27$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day27$Regular=ave(Day27$Regular,Day27$ToxicExp2.spa_id,FUN=sum)
Day27$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day27$Sortie_2=ave(Day27$Sortie_2,Day27$ToxicExp2.spa_id,FUN=sum)
Day27$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day27$Fort=ave(Day27$Fort,Day27$ToxicExp2.spa_id,FUN=sum)
Day27$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day27$Epic=ave(Day27$Epic,Day27$ToxicExp2.spa_id,FUN=sum)
Day27$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day27$Cybersport=ave(Day27$Cybersport,Day27$ToxicExp2.spa_id,FUN=sum)

Day27$battle_count27=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day27$battle_count27=ave(Day27$battle_count27,Day27$ToxicExp.spa_id,FUN=min)

Day27=Day27[!duplicated(Day27$ToxicExp2.spa_id),]
Day27[is.na(Day27)]<-1
#write.csv(Day27,"Day27.csv")

Day28=data.frame(ToxicExp2$spa_id)
Day28$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day28$selfteam=ave(Day28$selfteam,Day28$ToxicExp.spa_id,FUN=sum)
Day28$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day28$enemy=ave(Day28$enemy,Day28$ToxicExp.spa_id,FUN=sum)
Day28$battle=Day28$selfteam+Day28$enemy
Day28$before=ifelse(ToxicExp$time_stamp<=as.Date("03/28/2018",format="%m/%d/%Y"),0,1)

Day28$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day28$Ranked=ave(Day28$Ranked,Day28$ToxicExp2.spa_id,FUN=sum)
Day28$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day28$Regular=ave(Day28$Regular,Day28$ToxicExp2.spa_id,FUN=sum)
Day28$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day28$Sortie_2=ave(Day28$Sortie_2,Day28$ToxicExp2.spa_id,FUN=sum)
Day28$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day28$Fort=ave(Day28$Fort,Day28$ToxicExp2.spa_id,FUN=sum)
Day28$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day28$Epic=ave(Day28$Epic,Day28$ToxicExp2.spa_id,FUN=sum)
Day28$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day28$Cybersport=ave(Day28$Cybersport,Day28$ToxicExp2.spa_id,FUN=sum)

Day28$battle_count28=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day28$battle_count28=ave(Day28$battle_count28,Day28$ToxicExp.spa_id,FUN=min)

Day28=Day28[!duplicated(Day28$ToxicExp2.spa_id),]
Day28[is.na(Day28)]<-1
#write.csv(Day28,"Day28.csv")

Day29=data.frame(ToxicExp2$spa_id)
Day29$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day29$selfteam=ave(Day29$selfteam,Day29$ToxicExp.spa_id,FUN=sum)
Day29$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day29$enemy=ave(Day29$enemy,Day29$ToxicExp.spa_id,FUN=sum)
Day29$battle=Day29$selfteam+Day29$enemy
Day29$before=ifelse(ToxicExp$time_stamp<=as.Date("03/29/2018",format="%m/%d/%Y"),0,1)

Day29$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day29$Ranked=ave(Day29$Ranked,Day29$ToxicExp2.spa_id,FUN=sum)
Day29$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day29$Regular=ave(Day29$Regular,Day29$ToxicExp2.spa_id,FUN=sum)
Day29$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day29$Sortie_2=ave(Day29$Sortie_2,Day29$ToxicExp2.spa_id,FUN=sum)
Day29$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day29$Fort=ave(Day29$Fort,Day29$ToxicExp2.spa_id,FUN=sum)
Day29$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day29$Epic=ave(Day29$Epic,Day29$ToxicExp2.spa_id,FUN=sum)
Day29$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day29$Cybersport=ave(Day29$Cybersport,Day29$ToxicExp2.spa_id,FUN=sum)

Day29$battle_count29=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day29$battle_count29=ave(Day29$battle_count29,Day29$ToxicExp.spa_id,FUN=min)

Day29=Day29[!duplicated(Day29$ToxicExp2.spa_id),]
Day29[is.na(Day29)]<-1
#write.csv(Day29,"Day29.csv")

Day30=data.frame(ToxicExp2$spa_id)
Day30$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day30$selfteam=ave(Day30$selfteam,Day30$ToxicExp.spa_id,FUN=sum)
Day30$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day30$enemy=ave(Day30$enemy,Day30$ToxicExp.spa_id,FUN=sum)
Day30$battle=Day30$selfteam+Day30$enemy
Day30$before=ifelse(ToxicExp$time_stamp<=as.Date("03/30/2018",format="%m/%d/%Y"),0,1)

Day30$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day30$Ranked=ave(Day30$Ranked,Day30$ToxicExp2.spa_id,FUN=sum)
Day30$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day30$Regular=ave(Day30$Regular,Day30$ToxicExp2.spa_id,FUN=sum)
Day30$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day30$Sortie_2=ave(Day30$Sortie_2,Day30$ToxicExp2.spa_id,FUN=sum)
Day30$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day30$Fort=ave(Day30$Fort,Day30$ToxicExp2.spa_id,FUN=sum)
Day30$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day30$Epic=ave(Day30$Epic,Day30$ToxicExp2.spa_id,FUN=sum)
Day30$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day30$Cybersport=ave(Day30$Cybersport,Day30$ToxicExp2.spa_id,FUN=sum)

Day30$battle_count30=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day30$battle_count30=ave(Day30$battle_count30,Day30$ToxicExp.spa_id,FUN=min)

Day30=Day30[!duplicated(Day30$ToxicExp2.spa_id),]
Day30[is.na(Day30)]<-1
#write.csv(Day30,"Day30.csv")

Day31=data.frame(ToxicExp2$spa_id)
Day31$selfteam=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),ToxicExp$toxic.x,0)
Day31$selfteam=ave(Day31$selfteam,Day31$ToxicExp.spa_id,FUN=sum)
Day31$enemy=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
Day31$enemy=ave(Day31$enemy,Day31$ToxicExp.spa_id,FUN=sum)
Day31$battle=Day31$selfteam+Day31$enemy
Day31$before=ifelse(ToxicExp$time_stamp<=as.Date("03/31/2018",format="%m/%d/%Y"),0,1)

Day31$Ranked=ifelse(ToxicExp2$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="RANKED",ToxicExp2$toxic,0)
Day31$Ranked=ave(Day31$Ranked,Day31$ToxicExp2.spa_id,FUN=sum)
Day31$Regular=ifelse(ToxicExp2$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="REGULAR",ToxicExp2$toxic,0)
Day31$Regular=ave(Day31$Regular,Day31$ToxicExp2.spa_id,FUN=sum)
Day31$Sortie_2=ifelse(ToxicExp2$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="SORTIE_2",ToxicExp2$toxic,0)
Day31$Sortie_2=ave(Day31$Sortie_2,Day31$ToxicExp2.spa_id,FUN=sum)
Day31$Fort=ifelse(ToxicExp2$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="FORT_BATTLE_2",ToxicExp2$toxic,0)
Day31$Fort=ave(Day31$Fort,Day31$ToxicExp2.spa_id,FUN=sum)
Day31$Epic=ifelse(ToxicExp2$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="EPIC_RANDOM",ToxicExp2$toxic,0)
Day31$Epic=ave(Day31$Epic,Day31$ToxicExp2.spa_id,FUN=sum)
Day31$Cybersport=ifelse(ToxicExp2$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y") & ToxicExp2$battle_type=="CYBERSPORT",ToxicExp2$toxic,0)
Day31$Cybersport=ave(Day31$Cybersport,Day31$ToxicExp2.spa_id,FUN=sum)

Day31$battle_count31=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),ToxicExp$battle_count,0)
Day31$battle_count31=ave(Day31$battle_count31,Day31$ToxicExp.spa_id,FUN=min)

Day31=Day31[!duplicated(Day31$ToxicExp2.spa_id),]
Day31[is.na(Day31)]<-1
#write.csv(Day31,"Day31.csv")

names(Day1)=c("spa_id","selfteam1","enemy1","battle1","before1")
names(Day2)=c("spa_id","selfteam2","enemy2","battle2","before2")
names(Day3)=c("spa_id","selfteam3","enemy3","battle3","before3")
names(Day4)=c("spa_id","selfteam4","enemy4","battle4","before4")
names(Day5)=c("spa_id","selfteam5","enemy5","battle5","before5")
names(Day6)=c("spa_id","selfteam6","enemy6","battle6","before6")
names(Day7)=c("spa_id","selfteam7","enemy7","battle7","before7")
names(Day8)=c("spa_id","selfteam8","enemy8","battle8","before8")
names(Day9)=c("spa_id","selfteam9","enemy9","battle9","before9")
names(Day10)=c("spa_id","selfteam10","enemy10","battle10","before10")
names(Day11)=c("spa_id","selfteam11","enemy11","battle11","before11")
names(Day12)=c("spa_id","selfteam12","enemy12","battle12","before12")
names(Day13)=c("spa_id","selfteam13","enemy13","battle13","before13")
names(Day14)=c("spa_id","selfteam14","enemy14","battle14","before14")
names(Day15)=c("spa_id","selfteam15","enemy15","battle15","before15")
names(Day16)=c("spa_id","selfteam16","enemy16","battle16","before16")
names(Day17)=c("spa_id","selfteam17","enemy17","battle17","before17")
names(Day18)=c("spa_id","selfteam18","enemy18","battle18","before18")
names(Day19)=c("spa_id","selfteam19","enemy19","battle19","before19")
names(Day20)=c("spa_id","selfteam20","enemy20","battle20","before20")
names(Day21)=c("spa_id","selfteam21","enemy21","battle21","before21")
names(Day22)=c("spa_id","selfteam22","enemy22","battle22","before22")
names(Day23)=c("spa_id","selfteam23","enemy23","battle23","before23")
names(Day24)=c("spa_id","selfteam24","enemy24","battle24","before24")
names(Day25)=c("spa_id","selfteam25","enemy25","battle25","before25")
names(Day26)=c("spa_id","selfteam26","enemy26","battle26","before26")
names(Day27)=c("spa_id","selfteam27","enemy27","battle27","before27")
names(Day28)=c("spa_id","selfteam28","enemy28","battle28","before28")
names(Day29)=c("spa_id","selfteam29","enemy29","battle29","before29")
names(Day30)=c("spa_id","selfteam30","enemy30","battle30","before30")
names(Day31)=c("spa_id","selfteam31","enemy31","battle31","before31")

names(Day1)=c("spa_id","Ranked1","Regular1","Sortie1","Fort1","Epic1","Cybersport1")
names(Day2)=c("spa_id","Ranked2","Regular2","Sortie2","Fort2","Epic2","Cybersport2")
names(Day3)=c("spa_id","Ranked3","Regular3","Sortie3","Fort3","Epic3","Cybersport3")
names(Day4)=c("spa_id","Ranked4","Regular4","Sortie4","Fort4","Epic4","Cybersport4")
names(Day5)=c("spa_id","Ranked5","Regular5","Sortie5","Fort5","Epic5","Cybersport5")
names(Day6)=c("spa_id","Ranked6","Regular6","Sortie6","Fort6","Epic6","Cybersport6")
names(Day7)=c("spa_id","Ranked7","Regular7","Sortie7","Fort7","Epic7","Cybersport7")
names(Day8)=c("spa_id","Ranked8","Regular8","Sortie8","Fort8","Epic8","Cybersport8")
names(Day9)=c("spa_id","Ranked9","Regular9","Sortie9","Fort9","Epic9","Cybersport9")
names(Day10)=c("spa_id","Ranked10","Regular10","Sortie10","Fort10","Epic10","Cybersport10")
names(Day11)=c("spa_id","Ranked11","Regular11","Sortie11","Fort11","Epic11","Cybersport11")
names(Day12)=c("spa_id","Ranked12","Regular12","Sortie12","Fort12","Epic12","Cybersport12")
names(Day13)=c("spa_id","Ranked13","Regular13","Sortie13","Fort13","Epic13","Cybersport13")
names(Day14)=c("spa_id","Ranked14","Regular14","Sortie14","Fort14","Epic14","Cybersport14")
names(Day15)=c("spa_id","Ranked15","Regular15","Sortie15","Fort15","Epic15","Cybersport15")
names(Day16)=c("spa_id","Ranked16","Regular16","Sortie16","Fort16","Epic16","Cybersport16")
names(Day17)=c("spa_id","Ranked17","Regular17","Sortie17","Fort17","Epic17","Cybersport17")
names(Day18)=c("spa_id","Ranked18","Regular18","Sortie18","Fort18","Epic18","Cybersport18")
names(Day19)=c("spa_id","Ranked19","Regular19","Sortie19","Fort19","Epic19","Cybersport19")
names(Day20)=c("spa_id","Ranked20","Regular20","Sortie20","Fort20","Epic20","Cybersport20")
names(Day21)=c("spa_id","Ranked21","Regular21","Sortie21","Fort21","Epic21","Cybersport21")
names(Day22)=c("spa_id","Ranked22","Regular22","Sortie22","Fort22","Epic22","Cybersport22")
names(Day23)=c("spa_id","Ranked23","Regular23","Sortie23","Fort23","Epic23","Cybersport23")
names(Day24)=c("spa_id","Ranked24","Regular24","Sortie24","Fort24","Epic24","Cybersport24")
names(Day25)=c("spa_id","Ranked25","Regular25","Sortie25","Fort25","Epic25","Cybersport25")
names(Day26)=c("spa_id","Ranked26","Regular26","Sortie26","Fort26","Epic26","Cybersport26")
names(Day27)=c("spa_id","Ranked27","Regular27","Sortie27","Fort27","Epic27","Cybersport27")
names(Day28)=c("spa_id","Ranked28","Regular28","Sortie28","Epic28","Cybersport28","Fort28")
names(Day29)=c("spa_id","Ranked29","Regular29","Sortie29","Fort29","Epic29","Cybersport29")
names(Day30)=c("spa_id","Ranked30","Regular30","Sortie30","Fort30","Epic30","Cybersport30")
names(Day31)=c("spa_id","Ranked31","Regular31","Sortie31","Fort31","Epic31","Cybersport31")

names(Day1)=c("spa_id","battle_count1")
names(Day2)=c("spa_id","battle_count2")
names(Day3)=c("spa_id","battle_count3")
names(Day4)=c("spa_id","battle_count4")
names(Day5)=c("spa_id","battle_count5")
names(Day6)=c("spa_id","battle_count6")
names(Day7)=c("spa_id","battle_count7")
names(Day8)=c("spa_id","battle_count8")
names(Day9)=c("spa_id","battle_count9")
names(Day10)=c("spa_id","battle_count10")
names(Day11)=c("spa_id","battle_count11")
names(Day12)=c("spa_id","battle_count12")
names(Day13)=c("spa_id","battle_count13")
names(Day14)=c("spa_id","battle_count14")
names(Day15)=c("spa_id","battle_count15")
names(Day16)=c("spa_id","battle_count16")
names(Day17)=c("spa_id","battle_count17")
names(Day18)=c("spa_id","battle_count18")
names(Day19)=c("spa_id","battle_count19")
names(Day20)=c("spa_id","battle_count20")
names(Day21)=c("spa_id","battle_count21")
names(Day22)=c("spa_id","battle_count22")
names(Day23)=c("spa_id","battle_count23")
names(Day24)=c("spa_id","battle_count24")
names(Day25)=c("spa_id","battle_count25")
names(Day26)=c("spa_id","battle_count26")
names(Day27)=c("spa_id","battle_count27")
names(Day28)=c("spa_id","battle_count28")
names(Day29)=c("spa_id","battle_count29")
names(Day30)=c("spa_id","battle_count30")
names(Day31)=c("spa_id","battle_count31")









ToxicExposure3=Reduce(function(...) merge(...,by="spa_id"),list(Day1,Day2,Day3,Day4,Day5,Day6,Day7,Day8,Day9,Day10,Day11,Day12,Day13,Day14,Day15,Day16,Day17,Day18,Day19,Day20,Day21,Day22,Day23,Day24,Day25,Day26,Day27,Day28,Day29,Day30,Day31))
battle_count=apply(ToxicExposure3[2:32], 1, FUN=function(x) {min(x[x > 0])})
ToxicExposure3=cbind(ToxicExposure3,battle_count)





write.csv(ToxicExposure,"ToxicExposure.csv")

table_3=read.csv("table_3 (2).csv")
ToxicExposure=left_join(ToxicExposure, table_3, by=c("spa_id"="spa_key"))




ToxicExp$sDay2=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay2=ave(ToxicExp$sDay2,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay3=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay3=ave(ToxicExp$sDay3,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay4=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay4=ave(ToxicExp$sDay4,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay5=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay5=ave(ToxicExp$sDay5,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay6=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay6=ave(ToxicExp$sDay6,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay7=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay7=ave(ToxicExp$sDay7,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay8=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay8=ave(ToxicExp$sDay8,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay9=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay9=ave(ToxicExp$sDay9,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay10=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay10=ave(ToxicExp$sDay10,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay11=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay11=ave(ToxicExp$sDay11,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay12=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay12=ave(ToxicExp$sDay12,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay13=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay13=ave(ToxicExp$sDay13,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay14=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay14=ave(ToxicExp$sDay14,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay15=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay15=ave(ToxicExp$sDay15,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay16=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay16=ave(ToxicExp$sDay16,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay17=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay17=ave(ToxicExp$sDay17,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay18=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay18=ave(ToxicExp$sDay18,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay19=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay19=ave(ToxicExp$sDay19,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay20=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay20=ave(ToxicExp$sDay20,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay21=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay21=ave(ToxicExp$sDay21,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay22=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay22=ave(ToxicExp$sDay22,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay23=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay23=ave(ToxicExp$sDay23,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay24=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay24=ave(ToxicExp$sDay24,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay25=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay25=ave(ToxicExp$sDay25,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay26=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay26=ave(ToxicExp$sDay26,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay27=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay27=ave(ToxicExp$sDay27,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay28=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay28=ave(ToxicExp$sDay28,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay29=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay29=ave(ToxicExp$sDay29,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay30=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay30=ave(ToxicExp$sDay30,ToxicExp$spa_id,FUN=sum)
ToxicExp$sDay31=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$sDay31=ave(ToxicExp$sDay31,ToxicExp$spa_id,FUN=sum)


#exposure to enemy toxicity
ToxicExp$enemy_team_id=ifelse(table_1$user_team_identifier==1,2,1)
ToxicExp=left_join(ToxicExp,toxictable,by=c("battle_identifier"="battle_identifier","enemy_team_id"="user_team_identifier"))
ToxicExp=select(ToxicExp,spa_id, user_team_identifier, battle_identifier, battle_start_time,toxic.x,enemy_team_id, toxic.y)


ToxicExp$eDay1=ifelse(ToxicExp$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay1=ave(ToxicExp$eDay1,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay2=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay2=ave(ToxicExp$eDay2,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay3=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay3=ave(ToxicExp$eDay3,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay4=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay4=ave(ToxicExp$eDay4,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay5=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay5=ave(ToxicExp$eDay5,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay6=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay6=ave(ToxicExp$eDay6,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay7=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay7=ave(ToxicExp$eDay7,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay8=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay8=ave(ToxicExp$eDay8,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay9=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay9=ave(ToxicExp$eDay9,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay10=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay10=ave(ToxicExp$eDay10,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay11=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay11=ave(ToxicExp$eDay11,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay12=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay12=ave(ToxicExp$eDay12,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay13=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay13=ave(ToxicExp$eDay13,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay14=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay14=ave(ToxicExp$eDay14,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay15=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay15=ave(ToxicExp$eDay15,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay16=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay16=ave(ToxicExp$eDay16,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay17=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay17=ave(ToxicExp$eDay17,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay18=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay18=ave(ToxicExp$eDay18,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay19=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay19=ave(ToxicExp$eDay19,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay20=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay20=ave(ToxicExp$eDay20,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay21=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay21=ave(ToxicExp$eDay21,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay22=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay22=ave(ToxicExp$eDay22,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay23=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay23=ave(ToxicExp$eDay23,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay23=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay23=ave(ToxicExp$eDay23,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay24=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay24=ave(ToxicExp$eDay24,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay25=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay25=ave(ToxicExp$eDay25,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay26=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay26=ave(ToxicExp$eDay26,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay27=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay27=ave(ToxicExp$eDay27,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay28=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay28=ave(ToxicExp$eDay28,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay29=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay29=ave(ToxicExp$eDay29,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay30=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay30=ave(ToxicExp$eDay30,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay31=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay31=ave(ToxicExp$eDay31,ToxicExp$spa_id,FUN=sum)

#exposure to battle toxicity
ToxicExp$bDay1=ToxicExp$Day1+ToxicExp$eDay1
ToxicExp$bDay2=ToxicExp$Day2+ToxicExp$eDay2
ToxicExp$bDay3=ToxicExp$Day3+ToxicExp$eDay3
ToxicExp$bDay4=ToxicExp$Day4+ToxicExp$eDay4
ToxicExp$bDay5=ToxicExp$Day5+ToxicExp$eDay5
ToxicExp$bDay6=ToxicExp$Day6+ToxicExp$eDay6
ToxicExp$bDay7=ToxicExp$Day7+ToxicExp$eDay7
ToxicExp$bDay8=ToxicExp$Day8+ToxicExp$eDay8
ToxicExp$bDay9=ToxicExp$Day9+ToxicExp$eDay9
ToxicExp$bDay10=ToxicExp$Day10+ToxicExp$eDay10
ToxicExp$bDay11=ToxicExp$Day11+ToxicExp$eDay11
ToxicExp$bDay12=ToxicExp$Day12+ToxicExp$eDay12
ToxicExp$bDay13=ToxicExp$Day13+ToxicExp$eDay13
ToxicExp$bDay14=ToxicExp$Day14+ToxicExp$eDay14
ToxicExp$bDay15=ToxicExp$Day15+ToxicExp$eDay15
ToxicExp$bDay16=ToxicExp$Day16+ToxicExp$eDay16
ToxicExp$bDay17=ToxicExp$Day17+ToxicExp$eDay17
ToxicExp$bDay18=ToxicExp$Day18+ToxicExp$eDay18
ToxicExp$bDay19=ToxicExp$Day19+ToxicExp$eDay19
ToxicExp$bDay20=ToxicExp$Day20+ToxicExp$eDay20
ToxicExp$bDay21=ToxicExp$Day21+ToxicExp$eDay21
ToxicExp$bDay22=ToxicExp$Day22+ToxicExp$eDay22
ToxicExp$bDay23=ToxicExp$Day23+ToxicExp$eDay23
ToxicExp$bDay24=ToxicExp$Day24+ToxicExp$eDay24
ToxicExp$bDay25=ToxicExp$Day25+ToxicExp$eDay25
ToxicExp$bDay26=ToxicExp$Day26+ToxicExp$eDay26
ToxicExp$bDay27=ToxicExp$Day27+ToxicExp$eDay27
ToxicExp$bDay28=ToxicExp$Day28+ToxicExp$eDay28
ToxicExp$bDay29=ToxicExp$Day29+ToxicExp$eDay29
ToxicExp$bDay30=ToxicExp$Day30+ToxicExp$eDay30
ToxicExp$bDay31=ToxicExp$Day31+ToxicExp$eDay31

##
ToxicExp=left_join(ToxicExp,ToxicPlayer,by=c("spa_id"="reported_user_id"))
sum(is.na(ToxicExp$time_stamp.y))
ToxicExp$time_stamp.y=as.Date(as.character(as.POSIXct(ToxicExp$time_stamp.y)))
ToxicExp$first_toxicity=ave(ToxicExp$time_stamp.y,ToxicExp$spa_id, FUN=min)
ToxicExp=left_join(ToxicExp,ToxicPlayer, by=c("spa_id"="reported_user_id"))


##get the first toxic behavior time
ToxicPlayer=select(table_2,reported_user_id, time_stamp)
ToxicPlayer$time_stamp=as.Date(as.character(as.POSIXct(ToxicPlayer$time_stamp)))
ToxicPlayer=ToxicPlayer[order(ToxicPlayer$reported_user_id,ToxicPlayer$time_stamp),]
ToxicPlayer=ToxicPlayer[!duplicated(ToxicPlayer$reported_user_id),]



###
active=select(ToxicExposure3,spa_id,X1_active,X2_active,X3_active,X4_active,X5_active,X6_active,X7_active,X8_active,X9_active,X10_active,X11_active,X12_active,X13_active,X14_active,X15_active,X16_active,X17_active,X18_active,X19_active,X20_active,X21_active,X22_active,X23_active,X24_active,X25_active,X26_active,X27_active,X28_active,X29_active,X30_active,X31_active)
names(active)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
active=gather(active,"day","active",2:32)
active=transform(active,day=as.numeric(day))

purchase=select(ToxicExposure3,spa_id,X1_purchase,X2_purchase,X3_purchase,X4_purchase,X5_purchase,X6_purchase,X7_purchase,X8_purchase,X9_purchase,X10_purchase,X11_purchase,X12_purchase,X13_purchase,X14_purchase,X15_purchase,X16_purchase,X17_purchase,X18_purchase,X19_purchase,X20_purchase,X21_purchase,X22_purchase,X23_purchase,X24_purchase,X25_purchase,X26_purchase,X27_purchase,X28_purchase,X29_purchase,X30_purchase,X31_purchase)
names(purchase)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
purchase=gather(purchase,"day","purchase",2:32)
purchase=transform(purchase,day=as.numeric(day))

selfteam=select(ToxicExposure3,spa_id,selfteam1,selfteam2,selfteam3,selfteam4,selfteam5,selfteam6,selfteam7,selfteam8,selfteam9,selfteam9,selfteam10,selfteam11,selfteam12,selfteam13,selfteam14,selfteam15,selfteam16,selfteam17,selfteam18,selfteam19,selfteam20,selfteam21,selfteam22,selfteam23,selfteam24,selfteam25,selfteam26,selfteam27,selfteam28,selfteam29,selfteam30,selfteam31)
names(selfteam)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
selfteam=gather(selfteam,"day","selfteam",2:32)
selfteam=transform(selfteam,day=as.numeric(day))

enemy=select(ToxicExposure3,spa_id,enemy1,enemy2,enemy3,enemy4,enemy5,enemy6,enemy7,enemy8,enemy9,enemy10,enemy11,enemy12,enemy13,enemy14,enemy15,enemy16,enemy17,enemy18,enemy18,enemy19,enemy20,enemy21,enemy22,enemy23,enemy24,enemy25,enemy26,enemy27,enemy28,enemy29,enemy30,enemy31)
names(enemy)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
enemy=gather(enemy,"day","enemy",2:32)
enemy=transform(enemy,day=as.numeric(day))

battle=select(ToxicExposure,spa_id,battle1,battle2,battle3,battle4,battle5,battle6,battle7,battle8,battle9,battle10,battle11,battle12,battle13,battle14,battle15,battle16,battle17,battle18,battle19,battle20,battle21,battle22,battle23,battle24,battle25,battle26,battle27,battle28,battle29,battle30,battle31)
names(battle)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
battle=gather(battle,"day","battle",2:32)
battle=transform(battle,day=as.numeric(day))

before=select(ToxicExposure3, spa_id, before1,before2,before3,before4,before5,before6,before7,before8,before9,before10,before11,before12,before13,before14,before15,before16,before17,before18,before19,before20,before21,before22,before23,before24,before25,before26,before27,before28,before29,before30,before31)
names(before)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
before=gather(before,"day","before",2:32)
before=transform(before,day=as.numeric((day)))


Ranked=select(ToxicExposure3,spa_id,Ranked1,Ranked2,Ranked3,Ranked4,Ranked5,Ranked6,Ranked7,Ranked8,Ranked9,Ranked10,Ranked11,Ranked12,Ranked13,Ranked14,Ranked15,Ranked16,Ranked17,Ranked18,Ranked19,Ranked20,Ranked21,Ranked22,Ranked23,Ranked24,Ranked25,Ranked26,Ranked27,Ranked28,Ranked29,Ranked30,Ranked31)
names(Ranked)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
Ranked=gather(Ranked,"day","Ranked",2:32)
Ranked=transform(Ranked,day=as.numeric(day))

Regular=select(ToxicExposure3,spa_id,Regular1,Regular2,Regular3,Regular4,Regular5,Regular6,Regular7,Regular8,Regular9,Regular10,Regular11,Regular12,Regular13,Regular14,Regular15,Regular16,Regular17,Regular18,Regular19,Regular20,Regular21,Regular22,Regular23,Regular24,Regular25,Regular26,Regular27,Regular28,Regular29,Regular30,Regular31)
names(Regular)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
Regular=gather(Regular,"day","Regular",2:32)
Regular=transform(Regular,day=as.numeric(day))

Sortie=select(ToxicExposure3,spa_id,Sortie1,Sortie2,Sortie3,Sortie4,Sortie5,Sortie6,Sortie7,Sortie8,Sortie9,Sortie10,Sortie11,Sortie12,Sortie13,Sortie14,Sortie15,Sortie16,Sortie17,Sortie18,Sortie19,Sortie20,Sortie21,Sortie22,Sortie23,Sortie24,Sortie25,Sortie26,Sortie27,Sortie28,Sortie29,Sortie30,Sortie31)
names(Sortie)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
Sortie=gather(Sortie,"day","Sortie",2:32)
Sortie=transform(Sortie,day=as.numeric(day))

Fort=select(ToxicExposure3,spa_id,Fort1,Fort2,Fort3,Fort4,Fort5,Fort6,Fort7,Fort8,Fort9,Fort10,Fort11,Fort12,Fort13,Fort14,Fort15,Fort16,Fort17,Fort18,Fort19,Fort20,Fort21,Fort22,Fort23,Fort24,Fort25,Fort26,Fort27,Fort28,Fort29,Fort30,Fort31)
names(Fort)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
Fort=gather(Fort,"day","Fort",2:32)
Fort=transform(Fort,day=as.numeric(day))

Epic=select(ToxicExposure3,spa_id,Epic1,Epic2,Epic3,Epic4,Epic5,Epic6,Epic7,Epic8,Epic9,Epic10,Epic11,Epic12,Epic13,Epic14,Epic15,Epic16,Epic17,Epic18,Epic19,Epic20,Epic21,Epic22,Epic23,Epic24,Epic25,Epic26,Epic26,Epic27,Epic28,Epic29,Epic30,Epic31)
names(Epic)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
Epic=gather(Epic,"day","Epic",2:32)
Epic=transform(Epic,day=as.numeric(day))

Cybersport=select(ToxicExposure3,spa_id,Cybersport1,Cybersport2,Cybersport3,Cybersport4,Cybersport5,Cybersport6,Cybersport7,Cybersport8,Cybersport9,Cybersport10,Cybersport11,Cybersport12,Cybersport13,Cybersport14,Cybersport15,Cybersport16,Cybersport17,Cybersport18,Cybersport19,Cybersport20,Cybersport21,Cybersport22,Cybersport23,Cybersport24,Cybersport25,Cybersport26,Cybersport27,Cybersport28,Cybersport29,Cybersport30,Cybersport31)
names(Cybersport)=c("spa_id","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
Cybersport=gather(Cybersport,"day","Cybersport",2:32)
Cybersport=transform(Cybersport,day=as.numeric(day))

ToxicExposure4=full_join(Ranked,Regular, by=c("spa_id","day"))

ToxicExposure4=full_join(ToxicExposure4,Sortie, by=c("spa_id","day"))
ToxicExposure4=left_join(ToxicExposure4,Fort, by=c("spa_id","day"))
ToxicExposure4=left_join(ToxicExposure4,Epic, by=c("spa_id","day"))
ToxicExposure4=left_join(ToxicExposure4,Cybersport, by=c("spa_id","day"))


#compare date with first toxic behavior date
ToxicExp$before=ifelse(ToxicExp$V1<ToxicExp$first_toxicity,1,0)
ToxicExp$Day1=ave(ToxicExp$Day1,ToxicExp$spa_id,FUN=sum)
ToxicExp$eDay1=ave(ToxicExp$eDay1,ToxicExp$spa_id,FUN=sum)
#exposure to battle toxicity
ToxicExp$bDay1=ToxicExp$Day1+ToxicExp$eDay1
#compare date with first toxic behavior date
ToxicExp$bsDay1=ifelse(as.Date("03/01/2018",format="%m/%d/%Y")<ToxicExp$first_toxicity,ToxicExp$Day1,0)
ToxicExp$beDay1=ifelse(as.Date("03/01/2018",format="%m/%d/%Y")<ToxicExp$first_toxicity,ToxicExp$eDay1,0)
ToxicExp$bbDay1=ifelse(as.Date("03/01/2018",format="%m/%d/%Y")<ToxicExp$first_toxicity,ToxicExp$bDay1,0)
ToxicExp=ToxicExp[!duplicated(ToxicExp[,1]),]


ToxicExp[is.na(ToxicExp)]<-0
ToxicExp=ToxicExp[!duplicated(ToxicExp[,1]),]




##user history
history=select(table_3,spa_key, first_battle_dt)
history$first_battle_dt=as.Date(as.character(as.POSIXct(history$first_battle_dt)))
ToxicExp=left_join(ToxicExp,history,by=c("spa_id"="spa_key"))
ToxicExp$first_battle_d=0

ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),as.Date("03/01/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),as.Date("03/02/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),as.Date("03/03/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),as.Date("03/04/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),as.Date("03/05/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),as.Date("03/06/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),as.Date("03/07/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),as.Date("03/08/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),as.Date("03/09/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),as.Date("03/10/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),as.Date("03/11/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),as.Date("03/12/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),as.Date("03/13/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),as.Date("03/14/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),as.Date("03/15/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),as.Date("03/16/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),as.Date("03/17/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),as.Date("03/18/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),as.Date("03/19/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),as.Date("03/20/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),as.Date("03/21/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),as.Date("03/22/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),as.Date("03/23/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),as.Date("03/24/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),as.Date("03/25/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),as.Date("03/26/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),as.Date("03/27/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),as.Date("03/28/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),as.Date("03/29/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),as.Date("03/30/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)
ToxicExp$first_battle_d=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),as.Date("03/31/2018",format="%m/%d/%Y")-ToxicExp$first_battle_dt,ToxicExp$first_battle_d)

ToxicExp$day=0
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),1,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),2,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),3,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),4,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),5,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),6,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),7,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),8,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),9,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),10,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),11,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),12,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),13,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),14,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),15,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),16,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),17,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),18,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),19,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),20,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),21,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),22,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),23,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),24,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),25,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),26,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),27,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),28,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),29,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),30,ToxicExp$day)
ToxicExp$day=ifelse(ToxicExp$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),31,ToxicExp$day)

# history battle count & pr
battle_count=select(table_1,spa_id,battle_start_time,user_battle_count_at_time_battle,user_skill_at_this_battle_pr)
battle_count$battle_start_time=as.Date(as.character(as.POSIXct(battle_count$battle_start_time)))
ToxicExp$history_battle_count=ave(ToxicExp$user_battle_count_at_time_battle,battle_count$spa_id,FUN=min)
battle_count$history_pr=ave(battle_count$user_skill_at_this_battle_pr,battle_count$spa_id,FUN=min)

battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/01/2018",format="%m/%d/%Y"),1,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/02/2018",format="%m/%d/%Y"),2,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/03/2018",format="%m/%d/%Y"),3,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/04/2018",format="%m/%d/%Y"),4,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/05/2018",format="%m/%d/%Y"),5,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/06/2018",format="%m/%d/%Y"),6,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/07/2018",format="%m/%d/%Y"),7,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/08/2018",format="%m/%d/%Y"),8,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/09/2018",format="%m/%d/%Y"),9,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/10/2018",format="%m/%d/%Y"),10,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/11/2018",format="%m/%d/%Y"),11,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/12/2018",format="%m/%d/%Y"),12,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/13/2018",format="%m/%d/%Y"),13,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/14/2018",format="%m/%d/%Y"),14,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/15/2018",format="%m/%d/%Y"),15,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/16/2018",format="%m/%d/%Y"),16,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/17/2018",format="%m/%d/%Y"),17,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/18/2018",format="%m/%d/%Y"),18,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/19/2018",format="%m/%d/%Y"),19,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/20/2018",format="%m/%d/%Y"),20,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/21/2018",format="%m/%d/%Y"),21,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/22/2018",format="%m/%d/%Y"),22,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/23/2018",format="%m/%d/%Y"),23,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/24/2018",format="%m/%d/%Y"),24,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/25/2018",format="%m/%d/%Y"),25,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/26/2018",format="%m/%d/%Y"),26,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/27/2018",format="%m/%d/%Y"),27,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/28/2018",format="%m/%d/%Y"),28,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/29/2018",format="%m/%d/%Y"),29,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/30/2018",format="%m/%d/%Y"),30,battle_count$battle_start_time)
battle_count$battle_start_time=ifelse(battle_count$battle_start_time==as.Date("03/31/2018",format="%m/%d/%Y"),31,battle_count$battle_start_time)

ToxicExp$battle_count=ave(ToxicExp$user_battle_count_at_time_battle,ToxicExp$spa_id,ToxicExp$day, FUN=length)
battle_count=battle_count[!duplicated(battle_count[,1:2]),]
ToxicExposure1=left_join(ToxicExposure1,battle_count,by=c("spa_id"="spa_id","day"="battle_start_time"))

ToxicExposure1$history_battle_count_t=ave(ToxicExposure1$history_battle_count,ToxicExposure1[,1:2],FUN=function(x) min(x,na.rm=TRUE))
ToxicExposure1$history_battle_count_t[is.infinite(ToxicExposure1$history_battle_count_t)]=NA
ToxicExposure1=ToxicExposure1[complete.cases(ToxicExposure1$history_battle_count_t), ]
ToxicExposure1$history_pr_t=ave(ToxicExposure1$history_pr,ToxicExposure1$spa_id,FUN=function(x) min(x,na.rm=TRUE))

##random &nonrandom battle played per day
ToxicExp$random=0
ToxicExp$random=ifelse(ToxicExp$battle_type=="REGULAR",1,ToxicExp$random)
ToxicExp$random=ifelse(ToxicExp$battle_type=="RANKED",1,ToxicExp$random)
ToxicExp$random=ifelse(ToxicExp$battle_type=="EPIC",1,ToxicExp$random)
ToxicExp$random=ave(ToxicExp$random,ToxicExp$spa_id,ToxicExp$day,FUN=function(x) sum(x))
ToxicExp$nonrandom=0
ToxicExp$nonrandom=ifelse(ToxicExp$battle_type=="SORTIE_2",1,ToxicExp$nonrandom)
ToxicExp$nonrandom=ifelse(ToxicExp$battle_type=="FORT_BATTLE_2",1,ToxicExp$nonrandom)
ToxicExp$nonrandom=ifelse(ToxicExp$battle_type=="CYBERSPORT",1,ToxicExp$nonrandom)
ToxicExp$nonrandom=ave(ToxicExp$nonrandom,ToxicExp$spa_id,ToxicExp$day,FUN=function(x) sum(x))

##random battle type && Non-random battle type
ToxicExposure4$randomExp=ToxicExposure4$Regular+ToxicExposure4$Epic+ToxicExposure4$Ranked
ToxicExposure4$nonrandomExp=ToxicExposure4$Sortie+ToxicExposure4$Fort+ToxicExposure4$Cybersport

ToxicExposure2$randomExp[is.na(ToxicExposure2$randomExp)]=0
ToxicExposure2$nonrandomExp[is.na(ToxicExposure2$nonrandomExp)]=0

write.csv(ToxicExposure1,"ToxicExposure1_inter.csv")
write.csv(battle_count,"battle_count_inter.csv")


####survival analysis
Toxicd=subset(ToxicExposure2, before==0)
Toxicd=Toxicd[!duplicated(Toxicd$spa_id),]
Toxicd$before=1
Toxicsurv=subset(ToxicExposure2,before==1)
Toxicsurv$before=0
Toxicsurv=rbind(Toxicsurv,Toxicd)
write.csv(Toxicsurv,"Toxicsurv.csv")
surv_subject=Surv(time=Toxicsurv$day,event=Toxicsurv$before)
fit1=survfit(surv_subject~selfteam,data=Toxicsurv)
summary(fit1)
ggsurvplot(fit1,data=Toxicsurv,pval=T)
fit=survfit(surv_subject~1,data=Toxicsurv)
fit2=survfit(surv_subject~enemy+selfteam,data=Toxicsurv)
fit3=survfit(surv_subject~battle,data=Toxicsurv)

history_battle_count_std=scale(Toxicsurv$history_battle_count)
history_pr_std=scale(Toxicsurv$history_pr)

cox0=coxph(surv_subject~battle+history_battle_count_std+history_pr_std+battle_count_std,data=Toxicsurv)
cox=coxph(surv_subject~selfteam+enemy+history_battle_count_std+history_pr_std+battle_count_std,data=Toxicsurv)
cox1=coxph(surv_subject~randomExp+nonrandomExp+history_pr_std+history_battle_count_std+battle_count_std,data=Toxicsurv)
fit4=survfit(cox)

+history_battle_count_std+history_pr_std+battle_count_std
cox2=coxph(surv_subject~randomExp+history_battle_count_std+history_pr_std+battle_count_std,data=Toxicsurv)
fit5=survfit(cox2)
cox3=coxph(surv_subject~nonrandomExp+history_battle_count_std+history_pr_std+battle_count_std,data=Toxicsurv)
fit6=survfit(cox3)
cox4=coxph(surv_subject~selfteam,data=Toxicsurv)
fit7=survfit(cox4)
cox5=coxph(surv_subject~enemy,data=Toxicsurv)
fit8=survfit(cox5)
plot(fit5$time,fit5$cumhaz,type="l",xlab="Days",ylab="Hazard Ratio")
lines(fit6$time,fit6$cumhaz, type="l",col="red")
lines(fit7$time,fit7$cumhaz, type="l",col="blue")
lines(fit8$time,fit8$cumhaz, type="l",col="green")
legend("topleft", legend=c("Random battles", "Nonrandom battles","Selfteam","Enemy team"),col=c('black',"red","blue","green"),cex=0.8,lty = 1)

