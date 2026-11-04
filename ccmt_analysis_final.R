library(ggplot2)
library(readxl)
library(dplyr)
library(data.table)
library(stringr)


cts <- read_excel("Rating CCMT crime threat scenarios.xlsx", 
                  skip = 2)
View(cts)
setDT(cts)
str(cts)

# GET NAMES OF CRIME THREAT SCENARIOS
scenes <- names(cts[,4:length(cts)])
scenes <- unique(str_match(scenes, "^(.*?):"))[1:35]
scenes <- gsub("\\s\\((.*?)$", "", scenes)
scenes <- gsub(":", "", scenes)


# SUBSET EACH RATING TYPE

# HARM RATINGS
harm <- subset(cts, select = grepl("Harm", colnames(cts)))

# DEFEATIBILITY RATINGS
defeat <- subset(cts, select = grepl("Defeat-ability", colnames(cts)))

# FREQ RATINGS
freq <- subset(cts, select = grepl("Frequency", colnames(cts)))

# ACHIEVABILITY RATINGS
achiev <- subset(cts, select = grepl("Achievability", colnames(cts)))


# SCALE RATINGS BY CONFIDENCE
cts_scaled <- data.table()

for(scene in scenes){
  print(scene)
  
  h <- subset(harm, select = grepl(scene, colnames(harm)))
  h_conf <- mean(h[[1]] * h[[2]] / 10, na.rm=T)
  
  f <- subset(freq, select = grepl(scene, colnames(freq)))
  f_conf <- mean(f[[1]] * f[[2]] / 10, na.rm=T)
  
  d <- subset(defeat, select = grepl(scene, colnames(defeat)))
  d_conf <- mean(d[[1]] * d[[2]] / 10, na.rm=T)
  d_conf_rev <- mean((10-d[[1]]) * d[[2]] / 10, na.rm=T)
  
  a <- subset(achiev, select = grepl(scene, colnames(achiev)))
  a_conf <- mean(a[[1]] * a[[2]] / 10, na.rm=T)
  
  tmp <- data.table("Crime threat scenario" = scene,
                    "Harm" = mean(h[[1]], na.rm=T),
                    "Harm (scaled)" = h_conf,
                    "Defeatability" = mean(d[[1]], na.rm=T),
                    "Defeatability (scaled)" = d_conf,
                    "Defeatability (reverse-coded, scaled)" = d_conf_rev,
                    "Frequency" = mean(f[[1]], na.rm=T),
                    "Frequency (scaled)" = f_conf,
                    "Achievability" = mean(a[[1]], na.rm=T),
                    "Achievability (scaled)" =  a_conf,
                    "Pooled confidence" = mean(c(h_conf, f_conf)),
                    "Pooled confidence (h x d)" = mean(c(h_conf, d_conf)),
                    "Pooled confidence (h x d rev)" = mean(c(h_conf, d_conf_rev)))
  cts_scaled <- rbindlist(list(cts_scaled, tmp))
}

cts_scaled$Risk <- cts_scaled$`Harm (scaled)` * cts_scaled$`Frequency (scaled)`
cts_scaled


# TEST WHETHER DIMENSION RATING HIGHER THAN GROUP AVERAGE
tests <- data.table()

for(scene in scenes){
  print(scene)
  
  h <- subset(harm, select = grepl(scene, colnames(harm)))
  h_conf <- h[[1]] * h[[2]] / 10
  t_h <- t.test(h_conf, alternative = "g", mu = mean(cts_scaled$`Harm (scaled)`))
  
  f <- subset(freq, select = grepl(scene, colnames(freq)))
  f_conf <- f[[1]] * f[[2]] / 10
  t_f <- t.test(f_conf, alternative = "g", mu = mean(cts_scaled$`Frequency (scaled)`))
  
  d <- subset(defeat, select = grepl(scene, colnames(defeat)))
  d_conf <- d[[1]] * d[[2]] / 10
  t_d <- t.test(d_conf, alternative = "l", mu = mean(cts_scaled$`Defeatability (scaled)`))
  
  a <- subset(achiev, select = grepl(scene, colnames(achiev)))
  a_conf <- a[[1]] * a[[2]] / 10
  t_a <- t.test(a_conf, alternative = "g", mu = mean(cts_scaled$`Achievability (scaled)`))
  
  r_conf <- h_conf * f_conf
  t_r <- t.test(r_conf, alternative = "g", mu = mean(cts_scaled$`Risk`))
  
  tmp <- data.table("Crime threat scenario" = scene,
                    
                    "harm_estim" = t_h$estimate,
                    "harm_t_stat" = t_h$statistic[[1]],
                    "harm_t_p" = t_h$p.value,
                    
                    "freq_estim" = t_f$estimate,
                    "freq_t_stat" = t_f$statistic[[1]],
                    "freq_t_p" = t_f$p.value,
                    
                    "defeat_t_stat" = t_d$statistic[[1]],
                    "defeat_t_p" = t_d$p.value,
                    "defeat_estim" = t_d$estimate,
                    
                    "achiev_estim" = t_a$estimate,
                    "achiev_t_stat" = t_a$statistic[[1]],
                    "achiev_t_p" = t_a$p.value,
                    
                    "risk_estim" = t_r$estimate,
                    "risk_t_stat" = t_r$statistic[[1]],
                    "risk_t_p" = t_r$p.value)
  
  tests <- rbindlist(list(tests, tmp))
}


# MERGE STAT DATA SETS
cts_scaled <- merge(cts_scaled, tests, by = 'Crime threat scenario')
cts_scaled <- cts_scaled[order(risk_estim)]


# INSPECT SIGNIFICANT CT
tmp <- cts_scaled[cts_scaled$harm_t_p < .05, c("Crime threat scenario", "Harm (scaled)", "harm_t_stat", "harm_t_p", "harm_estim")]
tmp <- tmp[order(tmp$harm_t_p, decreasing = F)]
paste0(tmp$`Crime threat scenario`, " (confidence-weighted average = ", round(tmp$`Harm (scaled)`, 2), ", t(24) = ", round(tmp$harm_t_stat, 2), ", p = ", round(tmp$harm_t_p, 3), ")")
# paste0(tmp$`Crime threat scenario`, " (confidence-weighted average = ", round(tmp$harm_estim, 2), ", t(24) = ", round(tmp$harm_t_stat, 2), ", p = ", round(tmp$harm_t_p, 3), ")")

tmp <- cts_scaled[cts_scaled$freq_t_p < .05, c("Crime threat scenario", "Frequency (scaled)", "freq_t_stat", "freq_t_p")]
tmp <- tmp[order(tmp$freq_t_p, decreasing = F)]
paste0(tmp$`Crime threat scenario`, "'' (confidence-weighted average $= ", round(tmp$`Frequency (scaled)`, 2), ", t(24) = ", round(tmp$freq_t_stat, 2), ", p = ", round(tmp$freq_t_p, 3), "$)")

tmp <- cts_scaled[cts_scaled$defeat_t_p < .05, c("Crime threat scenario", "Defeatability (reverse-coded, scaled)", "defeat_t_stat", "defeat_t_p")]
tmp <- tmp[order(tmp$defeat_t_p, decreasing = F)]
paste0(tmp$`Crime threat scenario`, " (confidence-weighted average $= ", round(tmp$`Defeatability (reverse-coded, scaled)`, 2), ", t(24) = ", round(tmp$defeat_t_stat, 2), ", p = ", round(tmp$defeat_t_p, 3), "$)")

tmp <- cts_scaled[cts_scaled$achiev_t_p < .05, c("Crime threat scenario", "Achievability (scaled)", "achiev_t_stat", "achiev_t_p")]
tmp <- tmp[order(tmp$achiev_t_p, decreasing = F)]
paste0(tmp$`Crime threat scenario`, " (confidence-weighted average $= ", round(tmp$`Achievability (scaled)`, 2), ", t(24) = ", round(tmp$achiev_t_stat, 2), ", p = ", round(tmp$achiev_t_p, 3), "$)")

tmp <- cts_scaled[cts_scaled$risk_t_p < .05, c("Crime threat scenario", "Risk", "risk_t_stat", "risk_t_p")]
tmp <- tmp[order(tmp$risk_t_p, decreasing = F)]
paste0(tmp$`Crime threat scenario`, " (confidence-weighted average $= ", round(tmp$`Risk`, 2), ", t(24) = ", round(tmp$risk_t_stat, 2), ", p = ", round(tmp$risk_t_p, 3), "$)")


# PLOT HEATMAP
label_harm <- ifelse(cts_scaled$harm_t_p < .05, paste(round(cts_scaled$`Harm (scaled)`, 2), "*"), round(cts_scaled$`Harm (scaled)`, 2))
label_freq <- ifelse(cts_scaled$freq_t_p < .05, paste(round(cts_scaled$`Frequency (scaled)`, 2), "*"), round(cts_scaled$`Frequency (scaled)`, 2))
label_prevent <- ifelse(cts_scaled$defeat_t_p < .05, paste(round(cts_scaled$`Defeatability (reverse-coded, scaled)`, 2), "*"), round(cts_scaled$`Defeatability (reverse-coded, scaled)`, 2))
label_achiev <- ifelse(cts_scaled$achiev_t_p < .05, paste(round(cts_scaled$`Achievability (scaled)`, 2), "*"), round(cts_scaled$`Achievability (scaled)`, 2))
label_risk <- ifelse(cts_scaled$risk_t_p < .05, paste(round(cts_scaled$risk_estim, 2), "*"), round(cts_scaled$risk_estim, 2))

sig_labels <- c(label_harm, label_freq, label_prevent, label_achiev, label_risk) 


# HEATMAP MAIN FOUR RATINGS
keep <- c("Crime threat scenario", "Harm (scaled)", "Frequency (scaled)", "Defeatability (reverse-coded, scaled)", "Achievability (scaled)", "risk_estim")

cts_scaled_long <- melt(cts_scaled[,..keep], id.vars = "Crime threat scenario", value.name = "Confidence-weighted rating average")
cts_scaled_long$`Crime threat scenario` <- sub("\\d*\\.\\s", "", cts_scaled_long$`Crime threat scenario`)
cts_scaled_long$`Crime threat scenario` <- factor(cts_scaled_long$`Crime threat scenario`, levels = unique(cts_scaled_long$`Crime threat scenario`))

ggplot(cts_scaled_long, aes(cts_scaled_long$`Crime threat scenario`, variable)) +
  geom_tile(aes(fill = cts_scaled_long$`Confidence-weighted rating average`)) + coord_flip() +
  scale_fill_gradient(low = "white", high = "red") + theme_classic() +
  geom_text(aes(label = sig_labels)) +
  theme(axis.text.x = element_text(size = 12)) + 
  labs(fill = "Mean value") +
  ylab("") + xlab("") + ggtitle("Confidence-weighted rating average per crime threat") +
  scale_y_discrete(label = c("Harm","Frequency", "Defeat-\nability\n(reverse-\ncoded)", "Achievability", "Risk"))


#######

cm <- read_excel("Rating CCMT countermeasures.xlsx", 
                 skip = 2)
# View(cm)
setDT(cm)

# GET NAMES OF COUNTERMEASURES
counters <- names(cm[,4:length(cm)])
counters <- unique(str_match(counters, "^(.*?):"))[1:36]
counters <- gsub("\\s\\((.*?)$", "", counters)
counters[11] <- "frameworks to define and prevent tax avoidance"

# HARM RATINGS
scope <- subset(cm, select = grepl(": Scope", colnames(cm)))

# DEFEATIBILITY RATINGS
defeat <- subset(cm, select = grepl(": Defeat-ability", colnames(cm)))

# FREQ RATINGS
feas <- subset(cm, select = grepl(": Feasibiility", colnames(cm)))

# ACHIEVABILITY RATINGS
effec <- subset(cm, select = grepl(": Effectiveness", colnames(cm)))


# GET CONFIDENCE-WEIGHTED AVERAGES
cm_scaled <- data.table()

for(counter in counters){
  print(counter)
  
  h <- subset(scope, select = grepl(counter, colnames(scope)))
  h_conf <- mean(h[[1]] * h[[2]] / 10, na.rm=T) 
  
  f <- subset(feas, select = grepl(counter, colnames(feas)))
  f_conf <- mean(f[[1]] * f[[2]] / 10, na.rm=T)
  
  d <- subset(defeat, select = grepl(counter, colnames(defeat)))
  d_conf <- mean(d[[1]] * d[[2]] / 10, na.rm=T)
  d_conf_rev <- mean((10-d[[1]]) * d[[2]] / 10, na.rm=T)
  
  a <- subset(effec, select = grepl(counter, colnames(effec)))
  a_conf <- mean(a[[1]] * a[[2]] / 10, na.rm=T)
  
  tmp <- data.table("Countermeasure" = counter,
                    "Scope (scaled)" =  h_conf,
                    "Defeatability (scaled)" =  d_conf,
                    "Defeatability (reverse-coded, scaled)" = d_conf_rev,
                    "Feasibility (scaled)" =  f_conf,
                    "Effectiveness (scaled)" =  a_conf,
                    "Pooled confidence" = mean(c(f_conf, a_conf))
  )
  cm_scaled <- rbindlist(list(cm_scaled, tmp))
}

cm_scaled$Potential <- cm_scaled$`Feasibility (scaled)` * cm_scaled$`Effectiveness (scaled)`


# TEST IF CONFIDENCE-WEIGHTED AVERAGE IS DIFFERENT FROM GROUP AVERAGE FOR EACH DIMENSION
tests <- data.table()

for(counter in counters){
  print(counter)
  
  h <- subset(scope, select = grepl(counter, colnames(scope)))
  h_conf <- h[[1]] * h[[2]] / 10
  t_h <- t.test(h_conf, alternative = "g", mu = mean(cm_scaled$`Scope (scaled)`))
  
  f <- subset(effec, select = grepl(counter, colnames(effec)))
  f_conf <- f[[1]] * f[[2]] / 10
  t_f <- t.test(f_conf, alternative = "g", mu = mean(cm_scaled$`Effectiveness (scaled)`))
  
  d <- subset(defeat, select = grepl(counter, colnames(defeat)))
  d_conf <- d[[1]] * d[[2]] / 10
  t_d <- t.test(d_conf, alternative = "l", mu = mean(cm_scaled$`Defeatability (scaled)`))
  
  a <- subset(feas, select = grepl(counter, colnames(feas)))
  a_conf <- a[[1]] * a[[2]] / 10
  t_a <- t.test(a_conf, alternative = "g", mu = mean(cm_scaled$`Feasibility (scaled)`))
  
  p_conf <- f_conf * a_conf
  t_p <- t.test(p_conf, alternative = "g", mu = mean(cm_scaled$`Potential`))
  
  tmp <- data.table("Countermeasure" = counter,
                    
                    "scope_t_stat" = t_h$statistic[[1]],
                    "scope_t_p" = t_h$p.value,
                    
                    "effec_t_stat" = t_f$statistic[[1]],
                    "effec_t_p" = t_f$p.value,
                    
                    "defeat_t_stat" = t_d$statistic[[1]],
                    "defeat_t_p" = t_d$p.value,
                    
                    "feas_t_stat" = t_a$statistic[[1]],
                    "feas_t_p" = t_a$p.value,
                    
                    "pot_estim" = t_p$estimate,
                    "pot_t_stat" = t_p$statistic[[1]],
                    "pot_t_p" = t_p$p.value)
  
  tests <- rbindlist(list(tests, tmp))
}


# MERGE DATA SETS
cm_master <- merge(cm_scaled, tests, by = 'Countermeasure')
cm_master <- cm_master[order(pot_t_p)]

# inspect statistical significance
tmp <- cm_master[cm_master$pot_t_p < .05, c("Countermeasure", "pot_estim", "pot_t_stat", "pot_t_p")]
tmp <- tmp[order(tmp$pot_t_p, decreasing = F)]
paste0(tmp$Countermeasure, " (confidence-weighted average $= ", round(tmp$pot_estim, 2), ", t(24) = ", round(tmp$pot_t_stat, 2), ", p = ", round(tmp$pot_t_p, 3), "$)")

tmp <- cm_master[cm_master$scope_t_p < .05, c("Countermeasure", "Scope (scaled)", "scope_t_stat", "scope_t_p")]
tmp <- tmp[order(tmp$scope_t_p, decreasing = F)]
paste0(tmp$Countermeasure, " (confidence-weighted average $= ", round(tmp$`Scope (scaled)`, 2), ", t(24) = ", round(tmp$scope_t_stat, 2), ", p = ", round(tmp$scope_t_p, 3), "$)")

tmp <- cm_master[cm_master$effec_t_p < .05, c("Countermeasure", "Effectiveness (scaled)", "effec_t_stat", "effec_t_p")]
tmp <- tmp[order(tmp$effec_t_p, decreasing = F)]
paste0(tmp$Countermeasure, " (confidence-weighted average $= ", round(tmp$`Effectiveness (scaled)`, 2), ", t(24) = ", round(tmp$effec_t_stat, 2), ", p = ", round(tmp$effec_t_p, 3), "$)")

tmp <- cm_master[cm_master$feas_t_p < .05, c("Countermeasure", "Feasibility (scaled)", "feas_t_stat", "feas_t_p")]
tmp <- tmp[order(tmp$feas_t_p, decreasing = F)]
paste0(tmp$Countermeasure, " (confidence-weighted average $= ", round(tmp$`Feasibility (scaled)`, 2), ", t(24) = ", round(tmp$feas_t_stat, 2), ", p = ", round(tmp$feas_t_p, 3), "$)")

tmp <- cm_master[cm_master$defeat_t_p < .05, c("Countermeasure", "Defeatability (reverse-coded, scaled)", "defeat_t_stat", "defeat_t_p")]
tmp <- tmp[order(tmp$defeat_t_p, decreasing = F)]
paste0(tmp$Countermeasure, " (confidence-weighted average $= ", round(tmp$`Defeatability (reverse-coded, scaled)`, 2), ", t(24) = ", round(tmp$defeat_t_stat, 2), ", p = ", round(tmp$defeat_t_p, 3), "$)")


# CREATE PLOT LABELS
label_scope <- ifelse(cm_master$scope_t_p < .05, paste(round(cm_master$`Scope (scaled)`, 2), "*"), round(cm_master$`Scope (scaled)`, 2))
label_defeat <- ifelse(cm_master$defeat_t_p < .05, paste(round(cm_master$`Defeatability (reverse-coded, scaled)`, 2), "*"), round(cm_master$`Defeatability (reverse-coded, scaled)`, 2))
label_feas <- ifelse(cm_master$feas_t_p < .05, paste(round(cm_master$`Feasibility (scaled)`, 2), "*"), round(cm_master$`Feasibility (scaled)`, 2))
label_effec <- ifelse(cm_master$effec_t_p < .05, paste(round(cm_master$`Effectiveness (scaled)`, 2), "*"), round(cm_master$`Effectiveness (scaled)`, 2))
label_pot <- ifelse(cm_master$pot_t_p < .05, paste(round(cm_master$pot_estim, 2), "*"), round(cm_master$pot_estim, 2))

sig_labels <- c(label_scope, label_defeat, label_feas, label_effec, label_pot)

# PREPARE DATA SET IN LONG FORMAT FOR HEATMAP PLOT
keep <- c("Countermeasure", "Scope (scaled)", "Defeatability (reverse-coded, scaled)", "Feasibility (scaled)", "Effectiveness (scaled)", "pot_estim")
cm_master$Countermeasure <- gsub("\\d*\\.\\s", "", cm_master$Countermeasure)
cm_master$Countermeasure <- str_remove(cm_master$Countermeasure, ":")
cm_master$Countermeasure[7] <- "(Inter)national legal structures and frameworks to define and prevent tax avoidance"

cm_scaled_long <- melt(cm_master[,..keep], id.vars = "Countermeasure", value.name = "Confidence-weighted rating average")
cm_scaled_long$Countermeasure <- factor(cm_scaled_long$Countermeasure, levels = unique(cm_scaled_long$Countermeasure))

ggplot(cm_scaled_long, aes(cm_scaled_long$Countermeasure, variable)) +
  geom_tile(aes(fill = cm_scaled_long$`Confidence-weighted rating average`)) + coord_flip() +
  geom_text(aes(label = sig_labels)) +
  scale_fill_gradient(low = "white", high = "blue") + theme_classic() +
  theme(axis.text.x = element_text(size=12)) + 
  ylab("") + xlab("") + ggtitle("Confidence-weighted rating averages per countermeasure") +
  labs(fill = "Mean value") +
  scale_y_discrete(label = c("Scope", "Defeat-\nability\n(reverse-\ncoded)", "Feasibility", "Effectiveness", "Implementation\npotential"))

