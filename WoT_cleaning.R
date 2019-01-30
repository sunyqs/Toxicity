library(dplyr)
library(tidyr)
library(readxl)

#increase the max print, and turn off scientific notation
options(max.print=9999999)
options(scipen=999)

#read table 1
table_1=read.csv("table_1.csv")
table_1=table_1[!(table_1$battle_type=="BOOTCAMP" | table_1$battle_type=="SANDBOX" | table_1$battle_type=="TRAINING" | table_1$battle_type=="RATED_SANDBOX"),]
attach(table_1)
#select column of team size, battle type, battle duration, has legionnaire or not, team average PR, and battle result
average_selfteam_pr=average_pr_team_1+average_pr_team_2
team=select(table_1, battle_identifier, user_team_identifier, team_size, battle_type, battle_time_duration, user_clan_identifier, whether_user_is_legionnaire, battle_result)
team=cbind(team, average_selfteam_pr)
team$clan_count=ave(team$user_clan_identifier, team$battle_identifier, team$user_team_identifier, FUN = function(x) sum(x!=0))
team$legionnaire_count=ave(team$whether_user_is_legionnaire,team$battle_identifier,team$user_team_identifier, FUN=function(x) sum(x!=0))
#delete duplicated data
team=team[!duplicated(team[,1:2]),]
bnext <- data.frame(c(tail(team$battle_identifier,-1),0),c(tail(team$average_selfteam_pr,-1),0))
team$average_enemy_pr=ifelse(team$battle_identifier==bnext$c.tail.team.battle_identifier...1...0., bnext$c.tail.team.average_selfteam_pr...1...0., 0)
bprior=data.frame(c(0,head(team$battle_identifier,-1)),c(0,head(team$average_selfteam_pr,-1)))
team$average_enemy_pr2=ifelse(team$battle_identifier==bprior$c.0..head.team.battle_identifier...1..,bprior$c.0..head.team.average_selfteam_pr...1.., 0)
team$average_enemy_pr=team$average_enemy_pr+team$average_enemy_pr2

detach(table_1)




#Read table_2
table_2 <- read_excel("table_2.xlsx")
#Get automatic reported toxicity and set data type
AutoReport=table_2[table_2[,1]=="game automatic",]
AutoReport=transform(AutoReport, battle_id = as.numeric(battle_id))

#get team id for each toxic behavior 
#Found a problem here, only about 500 battle id found in table 1. 
AutoReportTeam=left_join(AutoReport, table_1,by=c("reported_user_id"="spa_id", "battle_id"="battle_identifier"))
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






#whether report others
report=data.frame(table(table_2$reporter_id))
names(report)=c("user_id","report_freq")

#get behavior type
ToxicPlayer=select(table_2,behavior_type, battle_id, reported_user_id, time_stamp)
ToxicBeh=as.data.frame.matrix(table(ToxicPlayer[,1:2]))
ToxicBeh=data.frame(t(ToxicBeh))
ToxicBeh=tibble::rownames_to_column(ToxicBeh,var="user_id")
ToxicBeh=transform(ToxicBeh, user_id = as.numeric(as.character(user_id)))
ToxicBeh=left_join(table_1,ToxicBeh, by=c("spa_id"="user_id"))
ToxicBeh[is.na(ToxicBeh)]<-0
##Table for RQ


##get exposure to selfteam 
ToxicExp=left_join(table_1, toxictable, by=c("battle_identifier","user_team_identifier"))
ToxicExp=transform(ToxicExp,battle_start_time=as.character(battle_start_time))
dtparts=as.data.frame(t(as.data.frame(strsplit(ToxicExp$battle_start_time,' '))))
ToxicExp=as.data.frame(c(ToxicExp,dtparts))
ToxicExp=transform(ToxicExp, V1 = as.character(V1))
ToxicExp$V1=as.Date(ToxicExp$V1,format="%m/%d/%y")
ToxicExp$Day1=ifelse(ToxicExp$V1==as.Date("03/01/2018",format="%m/%d/%Y"),ToxicExp$toxic,0)
ToxicExp$Day1=ave(ToxicExp$Day1,ToxicExp$spa_id,FUN=sum)

#exposure to enemy toxicity
ToxicExp$enemy_team_id=ifelse(table_1$user_team_identifier==1,2,1)
ToxicExp=left_join(ToxicExp,toxictable,by=c("battle_identifier"="battle_identifier","enemy_team_id"="user_team_identifier"))
ToxicExp$eDay1=ifelse(ToxicExp$V1==as.Date("03/01/2018",format="%m/%d/%Y"),ToxicExp$toxic.y,0)
ToxicExp$eDay1=ave(ToxicExp$eDay1,ToxicExp$spa_id,FUN=sum)

#exposure to battle toxicity
ToxicExp$bDay1=ToxicExp$Day1+ToxicExp$eDay1


#get first toxic behavior date
ToxicExp=select(ToxicExp, spa_id,user_team_identifier,battle_identifier, team_size.x,battle_type.x,battle_result.x,battle_time_duration.x,Inaction.Bot.x, Unsportsmanlike.x,Inappropriate.x,Nickname.x,toxic.x,V1,Day1, enemy_team_id,Inaction.Bot.y,Unsportsmanlike.y,Inappropriate.y,Nickname.y, toxic.y, eDay1,bDay1)
ToxicPlayer=transform(ToxicPlayer,battle_id=as.numeric(battle_id))
ToxicExp=left_join(ToxicExp,ToxicPlayer,by=c("spa_id"="reported_user_id"))
ToxicExp$time_stamp.y=as.Date(as.character(as.POSIXct(ToxicExp$time_stamp.y)))
ToxicExp$first_toxicity=ave(ToxicExp$time_stamp.y,ToxicExp$spa_id, FUN=min)


#compare date with first toxic behavior date
ToxicExp$bsDay1=ifelse(as.Date("03/01/2018",format="%m/%d/%Y")<ToxicExp$first_toxicity,ToxicExp$Day1,0)
ToxicExp$beDay1=ifelse(as.Date("03/01/2018",format="%m/%d/%Y")<ToxicExp$first_toxicity,ToxicExp$eDay1,0)
ToxicExp$bbDay1=ifelse(as.Date("03/01/2018",format="%m/%d/%Y")<ToxicExp$first_toxicity,ToxicExp$bDay1,0)

ToxicExp[is.na(ToxicExp)]<-0
ToxicExp=ToxicExp[!duplicated(ToxicExp[,1]),]


3222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222ToxicPlayer=ToxicPlayer[with(ToxicPlayer,order(reported_user_id,time_stamp)),]
ToxicPlayer = ToxicPlayer[!duplicated(ToxicPlayer$reported_user_id),]

#read table_3
table_3 <- read_excel("table_3.xlsx")

#get all reported battle information
battletoxic=inner_join(table_1, toxictable, by=c("battle_identifier"="battle_id"))
###select(battletoxic, reported_user_id,  )

#get the exposure of toxic battles and the exposure of toxic teams
toxicbattleteam=aggregate(report~spa_id+user_team_identifier.x+user_team_identifier.y,battletoxic,sum)
#the exposure of toxic teams
toxicteam=toxicbattleteam[!(toxicbattleteam$user_team_identifier.x!=toxicbattleteam$user_team_identifier.y),]
#the exposure of toxic battle
toxicbattle=aggregate(report~spa_id, toxicbattleteam, sum)

#Get all palyers has a clan
clan=subset(table_1,user_clan_identifier>0)
###clan=select(clan, spa_id,)
#get all clan with toxic player
###toxicclan=_join(clan, ToxicPlayer, by=c("spa_id"="reported_user_id"))





