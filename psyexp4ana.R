### Analysis code for psyexp course
### Tzu-Yao Chiu, 2020.09.21

library(dplyr)
library(memisc)
library(ggplot2)

# Clean workplace
rm(list = ls())

# Experiment name
exp_name<-'Navon'
output_dir<-'./Routput/'
if(!file.exists(output_dir)){dir.create(output_dir)}

# Read in the data file
file_list <- list.files(path = paste0('./data/'),pattern = paste0(exp_name, '_'))

# If run result for students
#file_list<-file_list[!file_list == 'data_G8.csv']

for(file in file_list){
  if(!exists('dta')){
    dta<-read.csv(file = paste0('./data/',file))
  }else{
    temp<-read.csv(file = paste0('./data/',file))
    dta<-rbind(dta,temp)
  }
}
write.csv(dta,file = paste0('./data/',exp_name,'.csv'),row.names = F)

# Set parameters
N_total <- length(unique(dta$ID))
within_var<-c('instr','cong')
#between_var<-c('gender')
run_str<-function(x){
  eval(parse(text = x))
}

# Filter data for RT & ACC
dta_RT<-dta[dta['bno']<99 & dta['Status']==1,]
dta_ACC<-dta[dta['bno']<99,]
print(paste0('Data points for RT : ',nrow(dta_RT)))
print(paste0('Data points for ACC : ',nrow(dta_ACC)))

dta_ACC$ACC<-sapply(dta_ACC$Status,function(x){
  if(x == 1){
    temp<-1
  }else{
    temp<-0
  }
  return(temp)
})

# Calculate mean per participant
command<-c('ID',within_var)
if(exists('between_var')){command<-c(command,between_var)}
RT<-aggregate(dta_RT[,'RT'],
              by = dta_RT[command],
              FUN = mean)
ACC<-aggregate(dta_ACC[,'ACC'],
               by = dta_ACC[command],
               FUN = mean)

# Perform ANOVA
RT[,command]<-lapply(RT[,command],factor)
ACC[,command]<-lapply(ACC[,command],factor)
formula<-paste0(within_var,collapse = '*')
error<-paste0('Error(ID/(',formula,'))')
if(exists('between_var')){formula<-paste(formula,between_var,sep = '*')}
sink(file = paste0(output_dir,exp_name,'_ANOVA_N',N_total,'.txt'))
aov_RT<-aov(formula = as.formula(paste0('x ~ ',formula,' + ',error)),data = RT)
aov_ACC<-aov(formula = as.formula(paste0('x ~ ',formula,' + ',error)),data = ACC)
summary(aov_RT)
summary(aov_ACC)
sink()

# Data for plot
command<-command[command!='ID']
RT<-do.call(data.frame,aggregate(RT[,'x'],
                by = RT[command],
                FUN = function(x)c(mean(x),sd(x)/sqrt(length(x)))))
ACC<-do.call(data.frame,aggregate(ACC[,'x'],
                                  by = ACC[command],
                                  FUN = function(x)c(mean(x),sd(x)/sqrt(length(x)))))

dta_plot<-merge(RT,ACC,by = command)
colnames(dta_plot)<-c(command,'RT','SE_RT','ACC','SE_ACC')
dta_plot[,! colnames(dta_plot) %in% command] <- round(dta_plot[,! colnames(dta_plot) %in% command],digits = 3)

# Relabel the data & export
dta_plot$cong<-relabel(factor(dta_plot$cong),'1' = 'Congruent','2' = 'Incongruent')
dta_plot$instr<-relabel(factor(dta_plot$instr),'1' = 'Global','2' = 'Local')
write.csv(dta_plot,paste0(output_dir,exp_name,'_stats_N',N_total,'.csv'),row.names = F)

# Plot the results
ty_theme<-theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size = 16, color = 'royalblue4'),
                axis.title = element_text(size = 16, color = 'royalblue4'),
                axis.line = element_line(size = 0.4),
                plot.title = element_text(size = 16, color = 'royalblue4'),
                legend.position = c(0.9,0.2),
                legend.text = element_text(size = 16, color = 'royalblue4'),
                legend.title = element_blank())

# Calculate total variable
total_var<-length(within_var)
x_title <- 'Congruency'; y_title <- 'Reaction Time (ms)';
if(exists('between_var')){total_var<-total_var+length(between_var)}  
fg_RT<-ggplot(dta_plot,aes(x = get(within_var[2]),y = RT,group = get(within_var[1]),color = get(within_var[1])))+
  geom_point(pch = 21,size = 1.5)+
  geom_line(size = 1.5)+
  geom_errorbar(aes(ymin = RT-SE_RT,ymax = RT+SE_RT),width = 0.1,size = 1.2)+
  ty_theme+
  scale_y_continuous(limits = c(400,900))+
  labs(x = x_title,y = y_title)+
  ggtitle('Mean reaction time')
fg_RT

y_title <- 'Accuracy Rate'
fg_ACC<-ggplot(dta_plot,aes(x = get(within_var[2]),y = ACC,group = get(within_var[1]),color = get(within_var[1])))+
  geom_point(pch = 21,size = 1.5)+
  geom_line(size = 1.5)+
  geom_errorbar(aes(ymin = ACC-SE_ACC,ymax = ACC+SE_ACC),width = 0.1,size = 1.2)+
  ty_theme+
  scale_y_continuous(limits = c(0.5,1))+
  labs(x = x_title,y = y_title)+
  ggtitle('Mean accuracy rate')
fg_ACC

pdf(file = paste0(output_dir, exp_name,'_plot_N',N_total,'.pdf'),11,6)
fg_RT;fg_ACC;
dev.off()
