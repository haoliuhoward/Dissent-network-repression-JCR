#### replication of the Appendix 
rm(list=ls(all=TRUE))
cat("\014")

# Load packages
source('R/loadPkg.R')
packs = c('tidyverse', 
          'lmtest', 'lme4', 'multiwayvcov','ggeffects',
          'stargazer', 'lubridate','Hmisc','xtable',
          'ggplot2',"RColorBrewer")
loadPkg(packs)

# Load Data 
load('replication_taiwanTerror/data/modData_dedup_noNA.rda')

dv = "charge_death"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")


### Table A1 A2: confirmed leaders  --------------------------------------------------
load('replication_taiwanTerror/data/modData_dedup_noNA_leadership.rda') # add in confirmed leadership
m1 <- glm(charge_death ~ leadership_conf + outDegree + turn_in + 
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander,
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m1_yr <- glm(charge_death ~ leadership_conf + outDegree + turn_in + 
               reason_intel + reason_advert + reason_support + reason_part +
               gender +
               student + doctor + military + inmate + islander +
               factor(sentence_year),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m1_yr
m1_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


m2 <- glm(charge_death ~ leadership_conf + outDegree + turn_in + 
            reason_intel + reason_advert + reason_support + reason_part +
            gender +
            student + doctor + military + inmate + islander +
            SW_leadership,
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_yr <- glm(charge_death ~ leadership_conf + outDegree + turn_in + 
               reason_intel + reason_advert + reason_support + reason_part +
               gender +
               student + doctor + military + inmate + islander +
               SW_leadership +
               factor(sentence_year),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m2_yr
m2_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_rec <- glm(charge_death ~ leadership_conf + outDegree + turn_in + 
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander +
                SW_centrality,
              data = modData_dedup_noNA,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_rec_yr <- glm(charge_death ~ leadership_conf + outDegree + turn_in + 
                   reason_intel + reason_advert + reason_support + reason_part +
                   gender +
                   student + doctor + military + inmate + islander +
                   SW_centrality+ 
                   factor(sentence_year),
                 data = modData_dedup_noNA,
                 family=binomial)
basemodel <- m2_rec_yr
m2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

dv = "charge_death"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(m1.se, m1_yr.se,
          m2.se, m2_yr.se, m2_rec.se, m2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main_robust',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression (Confirmed Leadership)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations', 
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main_confirmed_leaders.tex',
          notes = "Standard errors clustered at the trial case level"
          )

# Table A2
SW2_leader <- glm(charge_death ~ 
                    leadership_conf + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in,
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_leader_yr <- glm(charge_death ~
                       leadership_conf + outDegree + turn_in + 
                       reason_intel + reason_advert + reason_support + reason_part + 
                       gender +
                       student + doctor + military + inmate + islander +
                       SW_leadership*turn_in + factor(sentence_year),
                     data = modData_dedup_noNA,
                     family=binomial)
basemodel <- SW2_leader_yr
SW2_leader_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec <- glm(charge_death ~ 
                 leadership_conf + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in,
               data = modData_dedup_noNA,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec_yr <- glm(charge_death ~
                    leadership_conf + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_centrality*turn_in + factor(sentence_year),
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_rec_yr
SW2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

labs = c("Operation leaders", "Active recruiters", "Defection",
         "Leaking military intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, SW2_leader_yr.se, 
          SW2_rec.se, SW2_rec_yr.se,
          covariate.labels = labs,
          label = 'main2_robust',
          type ="text",
          include.intercept = F,
          title = 'The Conditional Effects of Defection (Confirmed Leadership)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main2_confirmed_leaders.tex',
          notes = "Standard errors clustered at the trial case level")

rm(basemodel, m1, m1.se, m1_yr, m1_yr.se,
   m2, m2.se, m2_rec, m2_rec.se,m2_yr, m2_yr.se, m2, m2_rec_yr, m2_rec_yr.se,
   SW2_leader, SW2_leader_yr, SW2_leader_yr.se, SW2_leader.se,
   SW2_rec, SW2_rec_yr, SW2_rec_yr.se, SW2_rec.se)

### Table A3 A4: including released and then recaptured -----------------------------------------------------------------
load("replication/data/modData_noNA.rda") # include duplicated observations

m1 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + # 犯罪類型 crime types
            gender +
            student + doctor + military + inmate + islander,
          data = modData_noNA,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

m1_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander +
               factor(sentence_year),
             data = modData_noNA,
             family=binomial)
basemodel <- m1_yr
m1_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

m2 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part +
            gender +
            student + doctor + military + inmate + islander+
            SW_leadership ,
          data = modData_noNA,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

m2_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part +
               gender +
               student + doctor + military + inmate + islander +
               SW_leadership +
               factor(sentence_year),
             data = modData_noNA,
             family=binomial)
basemodel <- m2_yr
m2_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

m2_rec <- glm(charge_death ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander+
                SW_centrality,
              data = modData_noNA,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

m2_rec_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
                   reason_intel + reason_advert + reason_support + reason_part +
                   gender +
                   student + doctor + military + inmate + islander +
                   SW_centrality+
                   factor(sentence_year),
                 data = modData_noNA,
                 family=binomial)
basemodel <- m2_rec_yr
m2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))


dv = "charge_death"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")


stargazer(m1.se, m1_yr.se, m2.se, m2_yr.se, m2_rec.se, m2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main_death_multi',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression (Including the Released and Recaptured)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations', 
              nrow(na.omit(modData_noNA[,c(dv, idvs)])), nrow(na.omit(modData_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_noNA[,c(dv, idvs)])), nrow(na.omit(modData_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_noNA[,c(dv, idvs)])), nrow(na.omit(modData_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main_death_multi.tex',
          notes = "Standard errors clustered at the trial case level"
          )


# Table A4
SW2_leader <- glm(charge_death ~ 
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in,
                  data = modData_noNA,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

SW2_leader_yr <- glm(charge_death ~
                       leadership + outDegree + turn_in + 
                       reason_intel + reason_advert + reason_support + reason_part + 
                       gender +
                       student + doctor + military + inmate + islander +
                       SW_leadership*turn_in + factor(sentence_year),
                     data = modData_noNA,
                     family=binomial)
basemodel <- SW2_leader_yr
SW2_leader_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

SW2_rec <- glm(charge_death ~ 
                 leadership + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in,
               data = modData_noNA,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

SW2_rec_yr <- glm(charge_death ~
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_centrality*turn_in + factor(sentence_year),
                  data = modData_noNA,
                  family=binomial)
basemodel <- SW2_rec_yr
SW2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_noNA$caseNum))

labs = c("Operation leaders", "Active recruiters", "Defection",
         "Leaking military intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, SW2_leader_yr.se, 
          SW2_rec.se, SW2_rec_yr.se,
          covariate.labels = labs,
          label = 'main2_death_multi',
          type ="text",
          include.intercept = F,
          # style = 'io',
          title = 'The Conditional Effects of Defection (Including the Released and Recaptured)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          # align=T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData_noNA[,c(dv, idvs)])), nrow(na.omit(modData_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_noNA[,c(dv, idvs)])), nrow(na.omit(modData_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main2_death_multi.tex',
          notes = "Standard errors clustered at the trial case level"
          )

rm(basemodel, m1, m1.se, m1_yr, m1_yr.se,
   m2, m2.se, m2_rec, m2_rec.se,m2_yr, m2_yr.se, m2_rec_yr, m2_rec_yr.se,
   SW2_leader, SW2_leader_yr, SW2_leader_yr.se, SW2_leader.se,
   SW2_rec, SW2_rec_yr, SW2_rec_yr.se, SW2_rec.se)


### Table A5 A6: including people without the captured/sentenced year  -----------------------------------------------------------------
load('replication/data/modData.rda')
m1 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander,
          data = modData,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

m2 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander+
            SW_leadership ,
          data = modData,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

m2_rec <- glm(charge_death ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander+
                SW_centrality,
              data = modData,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(m1.se, m2.se, m2_rec.se, 
          covariate.labels = labs_SW,
          label = 'main_death_all_noYear',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression (Including the Released and Recaptured and No Captured Year)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations', 
              nrow(modData[,c(dv, idvs)]), nrow(modData[,c(dv, idvs)]),
              nrow(modData[,c(dv, idvs)]), nrow(modData[,c(dv, idvs)]),
              nrow(modData[,c(dv, idvs)]), nrow(modData[,c(dv, idvs)])
            )),
          # out = 'figures/main_death_all_noYear.tex',
          notes = "Standard errors clustered at the trial case level"
          )


# Table A6
SW2_leader <- glm(charge_death ~ 
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in,
                  data = modData,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

SW2_rec <- glm(charge_death ~ 
                 leadership + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in,
               data = modData,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

labs = c("Operation leaders", "Active recruiters", "Defection",
         "Leaking military intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, 
          SW2_rec.se, 
          covariate.labels = labs,
          label = 'main2_death_all_noYear',
          type ="text",
          include.intercept = F,
          title = 'The Conditional Effects of Defection (Including the Released and Recaptured and No Captured Year)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, 
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData[,c(dv, idvs)])), nrow(na.omit(modData[,c(dv, idvs)])),
              nrow(na.omit(modData[,c(dv, idvs)])), nrow(na.omit(modData[,c(dv, idvs)]))
            )),
          # out = 'figures/main2_death_all_noYear.tex',
          notes = "Standard errors clustered at the trial case level"
          )

rm(basemodel, m1, m1.se, m1_yr, m1_yr.se,
   m2, m2.se, m2_rec, m2_rec.se,m2_yr, m2_yr.se, m2, m2_rec_yr, m2_rec_yr.se,
   SW2_leader, SW2_leader_yr, SW2_leader_yr.se, SW2_leader.se,
   SW2_rec, SW2_rec_yr, SW2_rec_yr.se, SW2_rec.se)

### Table A7 A8: Including people of Released-and-Recaptured, no years, and no names --------------------------------------------
load('replication/data/modData_AddnoNames.rda')

m1 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander,
          data = modData,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

m2 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander+
            SW_leadership ,
          data = modData,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

m2_rec <- glm(charge_death ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander+
                SW_centrality,
              data = modData,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(m1.se, m2.se, m2_rec.se, 
          covariate.labels = labs_SW,
          label = 'main_death_all_noYear_noTrueNames',
          type ="text",
          include.intercept = F,
          # style = 'io',
          title = 'The Effect of Network Relationships on Severity of Repression (Including the Released-and-Recaptured, No Captured Year, and No True Names)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, 
          add.lines = list(
            c('Observations', 
              nrow(modData[,c(dv, idvs)]), nrow(modData[,c(dv, idvs)]),
              nrow(modData[,c(dv, idvs)]), nrow(modData[,c(dv, idvs)]),
              nrow(modData[,c(dv, idvs)]), nrow(modData[,c(dv, idvs)])
            )),
          # out = 'figures/main_death_all_noYear_noTrueNames.tex',
          notes = "Standard errors clustered at the trial case level"
          )


# Table A8
SW2_leader <- glm(charge_death ~ 
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in,
                  data = modData,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

SW2_rec <- glm(charge_death ~ 
                 leadership + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in,
               data = modData,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData$caseNum))

labs = c("Operation leaders", "Active recruiters", "Defection",
         "Leaking military intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, 
          SW2_rec.se, 
          covariate.labels = labs,
          label = 'main2_death_all_noYear_noTrueNames',
          type ="text",
          include.intercept = F,
          title = 'The Conditional Effects of Defection (Including the Released-and-Recaptured, No Captured Year, and No True Names)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, 
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData[,c(dv, idvs)])), nrow(na.omit(modData[,c(dv, idvs)])),
              nrow(na.omit(modData[,c(dv, idvs)])), nrow(na.omit(modData[,c(dv, idvs)]))
            )),
          # out = 'figures/main2_death_all_noYear_noTrueNames.tex',
          notes = "Standard errors clustered at the trial case level"
          )

rm(basemodel, m1, m1.se, m1_yr, m1_yr.se,
   m2, m2.se, m2_rec, m2_rec.se,m2_yr, m2_yr.se, m2, m2_rec_yr, m2_rec_yr.se,
   SW2_leader, SW2_leader_yr, SW2_leader_yr.se, SW2_leader.se,
   SW2_rec, SW2_rec_yr, SW2_rec_yr.se, SW2_rec.se)

## Table A9 A10: ordered DV (ordered logit model) -------------------------------------
modData_dedup_noNA_complete = modData_dedup_noNA %>% na.omit

# trim down to 4 categories
modData_dedup_noNA$orderedDV_4 %>% table 
modData_dedup_noNA$orderedDV_4 = (5-modData_dedup_noNA$orderedDV_4) 
modData_dedup_noNA$orderedDV_4 %>% table

modData_dedup_noNA$orderedDV_4 = modData_dedup_noNA$orderedDV_4 %>% as.factor()

library("MASS")
library("sandwich") 

fit.1 <- polr(orderedDV_4 ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part + 
                gender +
                student + doctor + military + inmate + islander,
              data = modData_dedup_noNA,
              Hess=TRUE)
basemodel <- fit.1
fit.1.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.1_yr <- polr(orderedDV_4 ~ leadership + outDegree + turn_in +
                   reason_intel + reason_advert + reason_support + reason_part +
                   gender +
                   student + doctor + military + inmate + islander +
                   factor(sentence_year),
                 data = modData_dedup_noNA,
                 Hess=TRUE)
basemodel <- fit.1_yr
fit.1_yr.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.2 <- polr(orderedDV_4 ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander+
                SW_leadership ,
              data = modData_dedup_noNA,
              Hess=TRUE)
basemodel <- fit.2
fit.2.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.2_yr <- polr(orderedDV_4 ~ leadership + outDegree + turn_in +
                   reason_intel + reason_advert + reason_support + reason_part + 
                   gender +
                   student + doctor + military + inmate + islander +
                   SW_leadership +
                   factor(sentence_year),
                 data = modData_dedup_noNA,
                 Hess=TRUE)
basemodel <- fit.2_yr
fit.2_yr.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.2_rec <- polr(orderedDV_4 ~ leadership + outDegree + turn_in +
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander+
                    SW_centrality,
                  data = modData_dedup_noNA,
                  Hess=TRUE)
basemodel <- fit.2_rec
fit.2_rec.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.2_rec_yr <- polr(orderedDV_4 ~ leadership + outDegree + turn_in +
                       reason_intel + reason_advert + reason_support + reason_part +
                       gender +
                       student + doctor + military + inmate + islander +
                       SW_centrality+
                       factor(sentence_year),
                     data = modData_dedup_noNA,
                     Hess=TRUE)
basemodel <- fit.2_rec_yr
fit.2_rec_yr.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

dv = "orderedDV_4"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")
labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(fit.1.se, fit.1_yr.se, fit.2.se, fit.2_yr.se, fit.2_rec.se, fit.2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main',
          type ="text",
          include.intercept = F,
          # style = 'io',
          title = 'The Effect of Network Relationships on Severity of Repression (Ordered Logit)',
          dep.var.labels = "Punishment Severity (ordered outcome)",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations', 
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main_orderedDV_4.tex',
          notes = "Standard errors clustered at the trial case level"
          )

# Table A10
fit.SW2_leader <- polr(orderedDV_4 ~ 
                         leadership + outDegree + turn_in + 
                         reason_intel + reason_advert + reason_support + reason_part + 
                         gender +
                         student + doctor + military + inmate + islander +
                         SW_leadership*turn_in,
                       data = modData_dedup_noNA,
                       Hess=TRUE)
basemodel <- fit.SW2_leader
fit.SW2_leader.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.SW2_leader_yr <- polr(orderedDV_4 ~
                            leadership + outDegree + turn_in + 
                            reason_intel + reason_advert + reason_support + reason_part + 
                            gender +
                            student + doctor + military + inmate + islander +
                            SW_leadership*turn_in + factor(sentence_year),
                          data = modData_dedup_noNA,
                          Hess=TRUE)
basemodel <- fit.SW2_leader_yr
fit.SW2_leader_yr.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.SW2_rec <- polr(orderedDV_4 ~
                      leadership + outDegree + turn_in + 
                      reason_intel + reason_advert + reason_support + reason_part +
                      gender +
                      student + doctor + military + inmate + islander +
                      SW_centrality*turn_in,
                    data = modData_dedup_noNA,
                    Hess=TRUE)
basemodel <- fit.SW2_rec
fit.SW2_rec.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

fit.SW2_rec_yr <- polr(orderedDV_4 ~
                         leadership + outDegree + turn_in + 
                         reason_intel + reason_advert + reason_support + reason_part +
                         gender +
                         student + doctor + military + inmate + islander +
                         SW_centrality*turn_in + factor(sentence_year),
                       data = modData_dedup_noNA,
                       Hess=TRUE)
basemodel <- fit.SW2_rec_yr
fit.SW2_rec_yr.se <- coeftest(basemodel, vcov=vcovCL(basemodel, factor(modData_dedup_noNA$caseNum) ))

labs = c("Operation leaders", "Active recruiters", "Defection",
         "Leaking military intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")


stargazer(fit.SW2_leader.se, fit.SW2_leader_yr.se, 
          fit.SW2_rec.se, fit.SW2_rec_yr.se,
          covariate.labels = labs,
          label = 'main2',
          type ="text",
          include.intercept = F,
          title = 'The Conditional Effects of Defection (Ordered Logit)',
          dep.var.labels = "Punishment Severity (Ordered outcome)",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main2_orderedDV_4.tex',
          notes = "Standard errors clustered at the trial case level"
)

rm(basemodel, fit.1,fit.1.se,fit.1_yr,fit.1_yr.se,fit.2.se, fit.2_yr.se, fit.2_rec, fit.1_sw,fit.1_sw.se, fit.2_sw,fit.2_sw.se, fit.3_sw, fit.3_sw.se,
   fit.SW2_leader, fit.SW2_leader_yr, fit.SW2_leader.se, fit.SW2_leader_yr.se,
   fit.SW2_rec, fit.SW2_rec_yr, fit.SW2_rec.se, fit.SW2_rec_yr.se,
   fit.2_rec, fit.2_rec_yr, fit.2_rec.se, fit.2_rec_yr.se)

## Table A11 A12: spatial and county FE  --------------------------------------------------
load('replication/data/modData_dedup_noNA_yearCountyFE.rda')

m1 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander + 
            factor(tw_born),
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m1_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander +
               factor(sentence_year) + 
               factor(tw_born),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m1_yr
m1_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander+
            SW_leadership + 
            factor(tw_born),
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part +
               gender +
               student + doctor + military + inmate + islander +
               SW_leadership +
               factor(sentence_year)+ 
               factor(tw_born),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m2_yr
m2_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


m2_rec <- glm(charge_death ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander+
                SW_centrality+ 
                factor(tw_born),
              data = modData_dedup_noNA,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_rec_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
                   reason_intel + reason_advert + reason_support + reason_part +
                   gender +
                   student + doctor + military + inmate + islander +
                   SW_centrality+
                   factor(sentence_year)+ 
                   factor(tw_born),
                 data = modData_dedup_noNA,
                 family=binomial)
basemodel <- m2_rec_yr
m2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


dv = "charge_death"
idvs = c("leadership", "outDegree", "turn_in",
         "reason_intel", "reason_part", "reason_advert", "reason_support", 
         "gender", "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(m1.se, m1_yr.se, m2.se, m2_yr.se, m2_rec.se, m2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main_death_spatialFE',
          type ="text",
          include.intercept = F,
          # style = 'io',
          title = 'The Effect of Network Relationships on Severity of Repression',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year", "tw_born"), omit.labels = c("sentence_year FE", "tw_born FE"),
          add.lines = list(
            c('Observations', 
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main_death_spatialFE.tex',
          notes = "Standard errors clustered at the trial case level"
)

# Table A12
SW2_leader <- glm(charge_death ~ 
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in + 
                    factor(tw_born),
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


SW2_leader_yr <- glm(charge_death ~
                       leadership + outDegree + turn_in + 
                       reason_intel + reason_advert + reason_support + reason_part + 
                       gender +
                       student + doctor + military + inmate + islander +
                       SW_leadership*turn_in + factor(sentence_year)+ 
                       factor(tw_born),
                     data = modData_dedup_noNA,
                     family=binomial)
basemodel <- SW2_leader_yr
SW2_leader_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec <- glm(charge_death ~ 
                 leadership + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in+ 
                 factor(tw_born),
               data = modData_dedup_noNA,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec_yr <- glm(charge_death ~
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_centrality*turn_in + factor(sentence_year)+ 
                    factor(tw_born),
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_rec_yr
SW2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


dv = "charge_death"
idvs = c("leadership", "outDegree", "turn_in",
         "reason_intel", "reason_part", "reason_advert", "reason_support", 
         "gender", "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

labs = c("Operation leaders", "Active recruiters", "Defection",
         "Leaking Military Intel.", "Spreading Rumors", "Aiding Subversion", "Joining Organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Closeness to recruiters", 
         "Defection x Closeness to leaders", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, SW2_leader_yr.se, 
          SW2_rec.se, SW2_rec_yr.se,
          covariate.labels = labs,
          label = 'main2_death_spatialFE',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year", "tw_born"), omit.labels = c("sentence_year FE", "tw_born FE"),
          add.lines = list(
            c('Observations', 
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main2_death_spatialFE.tex',
          notes = "Standard errors clustered at the trial case level")

rm(basemodel, m1, m1.se, m1_yr, m1_yr.se,
   m2, m2.se, m2_rec, m2_rec.se,m2_yr, m2_yr.se, m2, m2_rec_yr, m2_rec_yr.se,
   SW2_leader, SW2_leader_yr, SW2_leader_yr.se, SW2_leader.se,
   SW2_rec, SW2_rec_yr, SW2_rec_yr.se, SW2_rec.se)

## Figure A1: Types of Sentencing ------------------------------------------------------
# load('replication/data/modData_dedup_noNA.rda')
modData_dedup_noNA_complete = modData_dedup_noNA %>% na.omit()

modData_dedup_noNA_complete$orderedDV_4 = (5-modData_dedup_noNA_complete$orderedDV_4)

modData_dedup_noNA_complete$orderedDV_4[modData_dedup_noNA_complete$orderedDV_4 == 1] = "Innocence" 
modData_dedup_noNA_complete$orderedDV_4[modData_dedup_noNA_complete$orderedDV_4 == 2] = "Fixed-term imprisonment"
modData_dedup_noNA_complete$orderedDV_4[modData_dedup_noNA_complete$orderedDV_4 == 3] = "Life imprisonment"
modData_dedup_noNA_complete$orderedDV_4[modData_dedup_noNA_complete$orderedDV_4 == 4] = "Death penalty"

modData_dedup_noNA_complete$orderedDV_4 = factor(modData_dedup_noNA_complete$orderedDV_4, levels = c("Innocence", "Fixed-term imprisonment", "Life imprisonment", "Death penalty"))

# png(filename="figures/punishment_type.png",width = 800, height = 600, res = 100)

modData_dedup_noNA_complete %>%
  group_by(orderedDV_4) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x = (orderedDV_4), y = cnt)) +
  geom_bar(stat = "identity") + 
  xlab("Types of Punishment") + ylab("Count") +
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.x=element_text(angle = -45, hjust = 0))

# dev.off()

## Figure A2 A3: Correlation Matrix -------------------------------------------
modData_dedup_noNA_complete = modData_dedup_noNA

# devtools::install_github("b-rodrigues/brotools")
library(brotools)

dv = "charge_death"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

stats_tab = brotools::describe(modData_dedup_noNA_complete) %>% dplyr::select(variable, mean, min, max, sd) %>% filter(variable %in% c(dv, idvs)) %>% 
  as.data.frame() 

# round
stats_tab[, 2:ncol(stats_tab)] <- stats_tab[, 2:ncol(stats_tab)] %>% lapply(. , round, 2)

# order 
stats_tab
stats_tab = stats_tab[c(1,6,8,16,
                        10,9,11,12,
                        3,13,2,7,4,14,15), ]
rownames(stats_tab) = NULL
stats_tab

# rename
stats_tab$variable[stats_tab$variable == "charge_death"] <- "Death penalty"
stats_tab$variable[stats_tab$variable == "outDegree"] <- "Active recruiters"
stats_tab$variable[stats_tab$variable == "leadership"] <- "Operation leaders"
stats_tab$variable[stats_tab$variable == "turn_in"] <- "Defection"
stats_tab$variable[stats_tab$variable == "reason_intel"] <- "Leaking military intelligence"
stats_tab$variable[stats_tab$variable == "reason_part"] <- "Joining membership"
stats_tab$variable[stats_tab$variable == "reason_advert"] <- "Spreading rumors"
stats_tab$variable[stats_tab$variable == "reason_support"] <- "Aiding subversion"
stats_tab$variable[stats_tab$variable == "doctor"] <- "Doctor"
stats_tab$variable[stats_tab$variable == "student"] <- "Student"
stats_tab$variable[stats_tab$variable == "military"] <- "Police/Military"
stats_tab$variable[stats_tab$variable == "inmate"] <- "Inmate"
stats_tab$variable[stats_tab$variable == "islander"] <- "Islander"
stats_tab$variable[stats_tab$variable == "SW_leadership"] <- "Closeness to leaders"
stats_tab$variable[stats_tab$variable == "SW_centrality"] <- "Closeness to recruiters"
stats_tab

tab = xtable(stats_tab, type = "latex", caption = "Variable Statistics", label = "var_stats")
print(tab, 
      # file="figures/taiwan_stats.tex",
      include.rownames=FALSE)
rm(tab, stats_tab)

# 2. correlation of covariates
library(corrplot)
M = modData_dedup_noNA_complete %>% dplyr::select(Leaders = leadership, 
                                                  Recruiters = outDegree, 
                                                  # `Closeness to leaders` = SW_leadership, `Closeness to recruiters` = SW_centrality, 
                                                  Defectors = turn_in) %>% cor(.)

M2 = modData_dedup_noNA_complete %>% dplyr::select(Leaders = leadership, 
                                                   Recruiters = outDegree, 
                                                   `Close to leaders` = SW_leadership, `Close to recruiters` = SW_centrality, 
                                                   Defectors = turn_in) %>% cor(.)

# png(filename="figures/corr.png",width = 650, height = 650, res = 100)

corrplot::corrplot(M, method="color", type="upper",
                   tl.col="black", tl.srt=45,
                   addCoef.col = "black")

# dev.off()

# png(filename="figures/corr2.png",width = 650, height = 650, res = 100)

corrplot::corrplot(M2, method="color", type="upper",
                   tl.col="black", tl.srt=45,
                   addCoef.col = "black")

# dev.off()

## Table A13: Who defects? ----------------------------------
m2 <- glm(turn_in ~ leadership + outDegree + 
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander+ 
            SW_leadership ,
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_yr <- glm(turn_in ~ leadership + outDegree +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander +
               SW_leadership +
               factor(sentence_year),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m2_yr
m2_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec <- glm(turn_in ~ 
                 leadership + outDegree +  
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality,
               data = modData_dedup_noNA,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec_yr <- glm(turn_in ~ 
                    leadership + outDegree + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander + 
                    SW_centrality + factor(sentence_year),
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_rec_yr
SW2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


labs_SW = c("Operation leaders", "Active recruiters", 
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(m2.se, m2_yr.se, SW2_rec.se, SW2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main_defect',
          type ="text",
          include.intercept = F,
          # style = 'io',
          title = 'Who Defects',
          dep.var.labels = "Defection",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA[,c(dv, idvs)]))
            )),
          # out = 'figures/main_defect.tex',
          notes = "Standard errors clustered at the trial case level")


## Table A14 A15: survival analysis ----------------------------------------------

# Estimating BTSCS logit model
load("replication_taiwanTerror/data/modData_expanded.rda")

fit.1 <- glm(binaryCaptured_i ~ prior_captures +
               leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander +
               t + I(t^2) + I(t^3),
             data = mod_expanded_merged, family = binomial(logit))
basemodel <- fit.1
fit.1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=mod_expanded_merged$caseNum))

fit.2 <- glm(binaryCaptured_i ~ prior_captures +
               leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander + 
               t + I(t^2) + I(t^3), 
             data = mod_expanded_merged, family = binomial(logit))
basemodel <- fit.2
fit.2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=mod_expanded_merged$caseNum))

fit.1_sw <- glm(binaryCaptured_i ~ SW_leadership_lag_priorCap +
                  leadership + outDegree + turn_in +
                  reason_intel + reason_advert + reason_support + reason_part + 
                  gender +
                  student + doctor + military + inmate + islander + 
                  t + I(t^2) + I(t^3), 
                data = mod_expanded_merged, family = binomial(logit))
basemodel <- fit.1_sw
fit.1_sw.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=mod_expanded_merged$caseNum))

fit.2_sw <- glm(binaryCaptured_i ~ SW_centrality_lag_priorCap +
                  leadership + outDegree + turn_in +
                  reason_intel + reason_advert + reason_support + reason_part + 
                  gender +
                  student + doctor + military + inmate + islander +
                  t + I(t^2) + I(t^3), 
                data = mod_expanded_merged, family = binomial(logit))
basemodel <- fit.2_sw
fit.2_sw.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=mod_expanded_merged$caseNum))

fit.3_sw <- glm(binaryCaptured_i ~ SW_turn_in_lag_priorCap +
                  leadership + outDegree + turn_in +
                  reason_intel + reason_advert + reason_support + reason_part + 
                  gender +
                  student + doctor + military + inmate + islander +
                  t + I(t^2) + I(t^3),
                data = mod_expanded_merged, family = binomial(logit))
basemodel <- fit.3_sw
fit.3_sw.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=mod_expanded_merged$caseNum))


dv = "binaryCaptured_i"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership_lag_priorCap","SW_centrality_lag_priorCap", "prior_captures")

labs_SW2 = c("Prior Captures_{t-1}", 
             "Prior Captures (close to leaders)", 
             "Prior Captures (close to recruiters)", 
             "Prior Captures (close to defectors)", 
             "Operation leaders", "Active recruiters", "Defection",
             "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
             "Male",
             "Student", "Doctor", "Police/Military", "Inmate", "Islander",
             "t", "t^{2}", "t^{3}"
)

stargazer(fit.1.se,fit.1_sw.se, fit.2_sw.se,fit.3_sw.se,
          covariate.labels = labs_SW2,
          label = 'survivalPanel_BTSCS_leaders',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression',
          dep.var.labels = "Organization member arrests (leaders only)",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, 
          add.lines = list(
            c('Observations', 
              nrow(na.omit(mod_expanded_merged[,c(dv, idvs)])), nrow(na.omit(mod_expanded_merged[,c(dv, idvs)])),
              nrow(na.omit(mod_expanded_merged[,c(dv, idvs)])), nrow(na.omit(mod_expanded_merged[,c(dv, idvs)]))
            )),
          # out = 'figures/survivalPanel_BTSCS_leaders.tex',
          notes = "Standard errors clustered at the trial case level")

rm(fit.1,fit.1.se,fit.1_yr,fit.1_yr.se,fit.2.se, fit.2_rec, fit.1_sw,fit.1_sw.se, fit.2_sw,fit.2_sw.se, fit.3_sw, fit.3_sw.se)


# Table A15: Estimating Survival Panel data (Cox model)
library(survival)
library(ranger)
library(ggfortify)

fit.1 <- coxph(Surv(t, obsID, binaryCaptured_i) ~ prior_captures + 
                 leadership + outDegree + turn_in +
                 reason_intel + reason_advert + reason_support + reason_part + 
                 gender +
                 student + doctor + military + inmate + islander + cluster(name), 
               data=mod_expanded_merged)

fit.1_sw <- coxph(Surv(t, obsID, binaryCaptured_i) ~ SW_leadership_lag_priorCap +
                    leadership + outDegree + turn_in +
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander + cluster(name),
                  data=mod_expanded_merged)

fit.2_sw <- coxph(Surv(t,  obsID, binaryCaptured_i) ~ SW_centrality_lag_priorCap + 
                    leadership + outDegree + turn_in +
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander + cluster(name),
                  data=mod_expanded_merged)

fit.3_sw <- coxph(Surv(t,  obsID, binaryCaptured_i) ~ SW_turn_in_lag_priorCap + 
                    leadership + outDegree + turn_in +
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander+ cluster(name),
                  data=mod_expanded_merged) 

labs_SW2 = c("Prior Captures_{t-1}", 
             "Prior Captures (close to leaders)_{t-1}", 
             "Prior Captures (close to recruiters)_{t-1}", 
             "Prior Captures (close to defectors)_{t-1}", 
             "Operation leaders", "Active recruiters", "Defection",
             "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
             "Male",
             "Student", "Doctor", "Police/Military", "Inmate", "Islander")

stargazer(fit.1, fit.1_sw, fit.2_sw, fit.3_sw, # Hazard rate (risk of being captured)
          covariate.labels = labs_SW2,
          label = 'survivalPanel_cox_leaders',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression',
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          # out = 'figures/survivalPanel_cox_leaders.tex',
          intercept.bottom = T
)

## Table A16: Network effect of defection on penalty ---------------------------------------------
modData_dedup_noNA_elite = modData_dedup_noNA %>% filter(leadership == 1 | outDegree >0)
m1 <- glm(charge_death ~ 
            turn_in +
            SW_turn_in +
            reason_intel + reason_advert + reason_support + reason_part +
            gender +
            student + doctor + military + inmate + islander,
          data = modData_dedup_noNA_elite,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_elite$caseNum))

m2 <- glm(charge_death ~
            turn_in +
            SW_turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander +
            factor(sentence_year), 
          data = modData_dedup_noNA_elite,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_elite$caseNum))

dv = "charge_death"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

labs_SW_a16 = c("Defection",
            "Closeness to Defectors",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander")

stargazer(m1.se, m2.se,
          covariate.labels = labs_SW_a16,
          label = 'main',
          type ="text",
          include.intercept = F,
          title = 'Network Effect of Defection on Penalty',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations', 
              nrow(modData_dedup_noNA_elite[,c(dv, idvs)]), nrow(na.omit(modData_dedup_noNA_elite[,c(dv, idvs)]))
            )),
          # out = 'figures/robust_network_defect_a16.tex',
          notes = "Standard errors clustered at the trial case level"
)

rm(m1, m2, m1.se, m2.se,basemodel)


## Table A17 A18: Effect wanes after 1965 -----------------------------------------
modData_dedup_noNA_half = modData_dedup_noNA[modData_dedup_noNA$sentence_year> 1965, ]

m1 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            # gender +
            student + doctor + military + inmate + islander,
          data = modData_dedup_noNA_half,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))

m1_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part +
               # gender +
               student + doctor + military + inmate + islander +
               factor(sentence_year),
             data = modData_dedup_noNA_half,
             family=binomial)
basemodel <- m1_yr
m1_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))

m2 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            # gender +
            student + doctor + military + inmate + islander+
            SW_leadership ,
          data = modData_dedup_noNA_half,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))

m2_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               # gender +
               student + doctor + military + inmate + islander +
               SW_leadership +
               factor(sentence_year),
             data = modData_dedup_noNA_half,
             family=binomial)
basemodel <- m2_yr
m2_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))

m2_rec <- glm(charge_death ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                # gender +
                student + doctor + military + inmate + islander+
                SW_centrality,
              data = modData_dedup_noNA_half,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))

m2_rec_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
                   reason_intel + reason_advert + reason_support + reason_part +
                   # gender +
                   student + doctor + military + inmate + islander +
                   SW_centrality+
                   factor(sentence_year),
                 data = modData_dedup_noNA_half,
                 family=binomial)
basemodel <- m2_rec_yr
m2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))


dv = "charge_death"
idvs = c("leadership", "outDegree","turn_in",
         "reason_intel", "reason_advert", "reason_support", "reason_part",
         "gender",
         "student", "doctor", "military", "inmate", "islander",
         "SW_leadership","SW_centrality")

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            # "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")
stargazer(m1.se, m1_yr.se, m2.se, m2_yr.se, m2_rec.se, m2_rec_yr.se,
          type ="text",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"))

stargazer(m1.se, m1_yr.se, m2.se, m2_yr.se, m2_rec.se, m2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main_death_wane',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression (After 1965)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations', 
              nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)]))
            )),
          # out = 'figures/main_death_wane_a17.tex',
          notes = "Standard errors clustered at the trial case level"
)


# Table A18
SW2_leader <- glm(charge_death ~ 
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part + 
                    # gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in,
                  data = modData_dedup_noNA_half,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))


SW2_leader_yr <- glm(charge_death ~
                       leadership + outDegree + turn_in + 
                       reason_intel + reason_advert + reason_support + reason_part + 
                       student + doctor + military + inmate + islander +
                       SW_leadership*turn_in + factor(sentence_year),
                     data = modData_dedup_noNA_half,
                     family=binomial)
basemodel <- SW2_leader_yr
SW2_leader_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))


SW2_rec <- glm(charge_death ~ 
                 leadership + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in,
               data = modData_dedup_noNA_half,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))

SW2_rec_yr <- glm(charge_death ~
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    # gender +
                    student + doctor + military + inmate + islander +
                    SW_centrality*turn_in + factor(sentence_year),
                  data = modData_dedup_noNA_half,
                  family=binomial)
basemodel <- SW2_rec_yr
SW2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA_half$caseNum))


# Male can't be fitted in truncated data
labs = c("Operation leaders", "Active recruiters", "Defection", 
         "Leaking Military Intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         # "Male",
         "Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, SW2_leader_yr.se, 
          SW2_rec.se, SW2_rec_yr.se,
          covariate.labels = labs,
          label = 'main2_death_wane',
          type ="text",
          include.intercept = F,
          title = 'The Conditional Effects of Defection (After 1965)',
          dep.var.labels = "Death Sentence",
          model.numbers = T,
          multicolumn = T,
          no.space = T,
          font.size = "scriptsize",
          intercept.bottom = T, omit = c("sentence_year"),omit.labels = c("sentence_year FE"),
          add.lines = list(
            c('Observations',
              nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])),
              nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)])), nrow(na.omit(modData_dedup_noNA_half[,c(dv, idvs)]))
            )),
          # notes = "Standard errors clustered at the trial case level",
          out = 'figures/main2_death_wane_a18.tex')

