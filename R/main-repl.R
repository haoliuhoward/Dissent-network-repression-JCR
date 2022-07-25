#### replication of the main text
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

## Figure 1: Recruitment Network -----------------------------
load("replication_taiwanTerror/data/fig1.rda")

library(visNetwork)
# panel a
visNetwork(nodes, edges, height = "800px", width = "100%") %>%
  visLayout(randomSeed = 123) %>%
  visHierarchicalLayout(direction = "UD", sortMethod = "hubsize") %>%
  visGroups(groupname = "red", color = "red")  

# panel b
visNetwork(nodes, edges, height = "800px", width = "100%") %>%
  visLayout(randomSeed = 123) %>%
  # visHierarchicalLayout(direction = "UD", sortMethod = "hubsize") %>%
  visGroups(groupname = "red", color = "red")  

rm(edges,nodes)

## Figure3: Time series plot ------------------------------------------
charge_dat = modData_dedup_noNA %>% dplyr::select(sentence_year) 
charge_dat$count = 1 
charge_dat =  charge_dat[complete.cases(charge_dat),]
dat = charge_dat %>% group_by(sentence_year) %>% summarise_all(funs(sum)) %>% filter(sentence_year >= 1945)

# png(filename=paste0("figures/time-series2.png"), height = 500, width = 900, res = 100)

ggplot(dat, aes(y=count, x=sentence_year)) + 
  geom_bar(position="dodge", stat="identity") + xlab(element_blank()) + ylab("Number of Cases") +
  theme_bw() +
  theme(text = element_text(size=20))

# dev.off()
rm(dat,charge_dat)


## Table1: Descriptive stats ------------------------------------------------------
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
rm(tab, stats_tab, modData_dedup_noNA_complete)

## Table 2: Effects on Death --------------------------------------------------
m1 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander,
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m1
m1.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m1_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander +
               factor(sentence_year),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m1_yr
m1_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2 <- glm(charge_death ~ leadership + outDegree + turn_in +
            reason_intel + reason_advert + reason_support + reason_part + 
            gender +
            student + doctor + military + inmate + islander+
            SW_leadership ,
          data = modData_dedup_noNA,
          family=binomial)
basemodel <- m2
m2.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
               reason_intel + reason_advert + reason_support + reason_part + 
               gender +
               student + doctor + military + inmate + islander +
               SW_leadership +
               factor(sentence_year),
             data = modData_dedup_noNA,
             family=binomial)
basemodel <- m2_yr
m2_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_rec <- glm(charge_death ~ leadership + outDegree + turn_in +
                reason_intel + reason_advert + reason_support + reason_part +
                gender +
                student + doctor + military + inmate + islander+
                SW_centrality,
              data = modData_dedup_noNA,
              family=binomial)
basemodel <- m2_rec
m2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

m2_rec_yr <- glm(charge_death ~ leadership + outDegree + turn_in +
                   reason_intel + reason_advert + reason_support + reason_part +
                   gender +
                   student + doctor + military + inmate + islander +
                   SW_centrality+
                   factor(sentence_year),
                 data = modData_dedup_noNA,
                 family=binomial)
basemodel <- m2_rec_yr
m2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

labs_SW = c("Operation leaders", "Active recruiters", "Defection",
            "Leaking military intel.", "Spreading rumors", "Aiding subversion","Joining organization", 
            "Male",
            "Student", "Doctor", "Police/Military", "Inmate", "Islander",
            "Closeness to leaders","Closeness to recruiters")

stargazer(m1.se, m1_yr.se, m2.se, m2_yr.se, m2_rec.se, m2_rec_yr.se,
          covariate.labels = labs_SW,
          label = 'main_death',
          type ="text",
          include.intercept = F,
          title = 'The Effect of Network Relationships on Severity of Repression',
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
          # out = 'figures/main_death.tex',
          notes = "Standard errors clustered at the trial case level")


## Table 3: Conditional effects --------------------------------------------------
SW2_leader <- glm(charge_death ~ 
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part + 
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_leadership*turn_in,
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_leader
SW2_leader.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))


SW2_leader_yr <- glm(charge_death ~
                       leadership + outDegree + turn_in + 
                       reason_intel + reason_advert + reason_support + reason_part + 
                       gender +
                       student + doctor + military + inmate + islander +
                       SW_leadership*turn_in + factor(sentence_year),
                     data = modData_dedup_noNA,
                     family=binomial)
basemodel <- SW2_leader_yr
SW2_leader_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec <- glm(charge_death ~ 
                 leadership + outDegree + turn_in + 
                 reason_intel + reason_advert + reason_support + reason_part +
                 gender +
                 student + doctor + military + inmate + islander +
                 SW_centrality*turn_in,
               data = modData_dedup_noNA,
               family=binomial)
basemodel <- SW2_rec
SW2_rec.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

SW2_rec_yr <- glm(charge_death ~
                    leadership + outDegree + turn_in + 
                    reason_intel + reason_advert + reason_support + reason_part +
                    gender +
                    student + doctor + military + inmate + islander +
                    SW_centrality*turn_in + factor(sentence_year),
                  data = modData_dedup_noNA,
                  family=binomial)
basemodel <- SW2_rec_yr
SW2_rec_yr.se <- coeftest(basemodel, cluster.vcov(model=basemodel, cluster=modData_dedup_noNA$caseNum))

labs = c("Operation leaders", "Active recruiters", "Defection", 
         "Leaking Military Intel.", "Spreading rumors", "Aiding subversion", "Joining organization",
         "Male","Student", "Doctor", "Police/Military", "Inmate","Islander", 
         "Closeness to leaders", "Defection x Closeness to leaders", 
         "Closeness to recruiters", "Defection x Closeness to recruiters")

stargazer(SW2_leader.se, SW2_leader_yr.se, 
          SW2_rec.se, SW2_rec_yr.se,
          covariate.labels = labs,
          label = 'main_death2',
          type ="text",
          include.intercept = F,
          title = 'The Conditional Effects of Defection',
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
          # out = 'figures/main_death2.tex'
          notes = "Standard errors clustered at the trial case level")


# Figure 4: Effect Plots -------------------------------------------

# 1. leader
# png(filename="figures/predicted_leader.png",width = 650, height = 900, res = 100)

modData = modData_dedup_noNA
modData$leadership = modData$leadership %>% as.factor()
modData$turn_in = modData$turn_in %>% as.factor()
mod <- glm(charge_death ~ 
             leadership + outDegree + turn_in + 
             reason_intel + reason_advert + reason_support + reason_part + 
             gender +
             student + doctor + military + inmate + islander +
             SW_leadership,
           data = modData,
           binomial(link = "probit"))

dat <-ggpredict(mod, terms = c("leadership"), ci.lvl = 0.95,
                vcov.fun ="vcovHC",  
                vcov.type = "HC3")
dat$outcome = 'Predicted Probability of Death Penalty'
plot(dat, connect.lines = F, facet = F, colors = "bw", alpha = 0.15 ) +
  ylab("Predicted Probability of Death Penalty") +
  xlab("Leadership") +
  theme(text = element_text(size=20)) +
  theme(plot.title =element_blank()) + scale_y_continuous() 

# dev.off()


# 2. recruiter
# png(filename="figures/predicted_rec.png",width = 650, height = 900, res = 100)

dat <-  ggeffect(mod, terms = c("outDegree[all]"), ci.lvl = 0.95,
                 vcov.fun ="vcovHC",  
                 vcov.type = "HC3")
dat$outcome = 'Recruiters'
plot(dat, connect.lines = TRUE, facet = F, colors = "bw", alpha = 0.15) +
  ylab("Predicted Probability of Death Penalty") +
  xlab("Recruiter (Degree Centrality)") +
  theme(text = element_text(size=20)) +
  theme(plot.title =element_blank()) + scale_y_continuous()

# dev.off()

# 3. defection
# png(filename="figures/predicted_defection.png",width = 650, height = 900, res = 100)

dat <- ggeffect(mod, terms = c("turn_in"), ci.lvl = 0.95,
                vcov.fun ="vcovHC",
                vcov.type = "HC3")
dat$outcome = 'Defector'
plot(dat, connect.lines = F, facet = F) +
  ylab("Predicted Probability of Death Penalty") +
  xlab("Defection") +
  theme(text = element_text(size=20)) +
  theme(plot.title =element_blank()) + scale_y_continuous()

# dev.off()

# 4. SW effect
# png(filename="figures/predicted_SW_leader.png",width = 650, height = 900, res = 100)

dat <- ggpredict(mod, terms = c("SW_leadership [all]"), ci.lvl = 0.95,
                 vcov.fun ="vcovHC",  
                 vcov.type = "HC3") 
dat$outcome = 'Closeness to leaders'
plot(dat, connect.lines = F, facet = F) +
  ylab("Predicted Probability of Death Penalty") +
  xlab("Closeness to leaders") +
  theme(text = element_text(size=20)) +
  theme(plot.title =element_blank()) + scale_y_continuous()

# dev.off()


# 5. SW_recruit
# png(filename="figures/predicted_SW_rec.png",width = 650, height = 900, res = 100)

mod2 <- glm(charge_death ~ 
              leadership + outDegree + turn_in + 
              reason_intel + reason_advert + reason_support + reason_part + 
              gender +
              student + doctor + military + inmate + islander +
              SW_centrality,
            data = modData_dedup_noNA,
            family=binomial)

dat <- ggpredict(mod2, terms = c("SW_centrality [all]"), ci.lvl = 0.95,
                 vcov.fun ="vcovHC",  
                 vcov.type = "HC3") 
dat$outcome = 'Closeness to recruiters'
plot(dat, connect.lines = F, facet = F) +
  ylab("Predicted Probability of Death Penalty") +
  xlab("Closeness to recruiters") +
  theme(text = element_text(size=20)) +
  theme(plot.title =element_blank()) + scale_y_continuous()

# dev.off()



## Figure 5: Interaction Effect Plots --------------------------------------
library(sjPlot)
## model 1
# png(filename="figures/pred_inter_leader_prob.png",height = 800, width = 700, res = 100)
modData <- modData_dedup_noNA
modData$turn_in = modData$turn_in %>% as.factor()
mod3 <- glm(charge_death ~ 
              leadership + outDegree + turn_in + 
              reason_intel + reason_advert + reason_support + reason_part +
              gender +
              student + doctor + military + inmate + islander +
              SW_leadership*turn_in,
            data = modData,
            family=binomial)

plot_model( mod3, type = "pred", terms = c("turn_in", "SW_leadership"), ci.lvl = 0.95, robust = T, vcov.type = c("HC3"),bpe.style = "line" ) +
  theme_bw() +
  xlab("Defection") + ylab("Predicted Probabilities for Death Penalty") +
  labs(caption = element_blank()) +
  theme(legend.position = c(.5, .8), legend.justification = c(0, .5), text = element_text(size=18), axis.ticks.x=element_blank()) +
  scale_colour_grey( start = 0.8, end = 0.2, guide = guide_legend(title = "Closeness (leaders)"), labels = c("Low", "Median", "High")) +
  ggtitle(element_blank()) + scale_y_continuous(breaks = seq(0, 0.1, by = 0.02))

# dev.off()

## model 2
# png(filename="figures/pred_inter_rec_prob.png",height = 800, width = 700, res = 100)
modData <- modData_dedup_noNA
modData$turn_in = modData$turn_in %>% as.factor()
mod4 <- glm(charge_death ~
              leadership + outDegree + turn_in +
              reason_intel + reason_advert + reason_support + reason_part + 
              gender +
              student + doctor + military + inmate + islander +
              SW_centrality*turn_in,
            data = modData,
            family=binomial)

plot_model( mod4, type = "pred", terms = c("turn_in", "SW_centrality"), ci.lvl = 0.95, robust = T, vcov.type = c("HC3"),bpe.style = "line") +
  theme_bw() +
  xlab("Defection") + ylab("Predicted Probabilities of Death Penalty") +
  labs(caption = element_blank()) +
  theme(legend.position = c(.45, .8), legend.justification = c(0, .5), text = element_text(size=18), axis.ticks.x=element_blank()) +
  scale_colour_grey( start = 0.8, end = 0.2, guide = guide_legend(title = "Closeness (recruiters)"), labels = c("Low", "Median", "High")) +
  ggtitle(element_blank()) + scale_y_continuous(breaks = seq(0, 0.15, by = 0.03))

# dev.off()




