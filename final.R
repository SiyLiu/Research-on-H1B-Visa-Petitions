
library(ggplot2) # Data visualizationd
library(dplyr)
library(ggmap)
library(ggrepel)
library(lazyeval)
library(stringr)
library(readr)
library(gsubfn)
library(gcookbook)
library(sp)
library(RgoogleMaps)



file=choose.files()
h1b.data=read_csv(file)
h1b.data$YEAR = factor(h1b.data$YEAR)
case.status.all <- as.data.frame(h1b.data %>% filter(.,!is.na(CASE_STATUS))
                                  %>% group_by(.,CASE_STATUS) %>%
                                    summarise(.,PROPORTION = round(n()*100/nrow(h1b.data),1)))


#######################################PROPORTION of CASE STATUS################################33

case.status.all = case.status.all[order(case.status.all$PROPORTION, decreasing = TRUE),]
myLabel = as.vector(case.status.all$CASE_STATUS)   
myLabel = paste(myLabel, "(", round(case.status.all$PROPORTION / sum(case.status.all$PROPORTION) * 100, 2), "%)", sep = "")   



ppt.overall = ggplot(case.status.all, aes(x = "", y = PROPORTION, fill = CASE_STATUS)) +
  geom_bar(stat = "identity", width = 1) +    
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  ggtitle("Proportion of Case Status (2011 - 2016)")+
  theme(axis.ticks = element_blank()) + 
  scale_fill_discrete(breaks = case.status.all$CASE_STATUS, labels = myLabel) + 
  theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Blues")+
  geom_text(aes(y = PROPORTION/2 + c(0, cumsum(PROPORTION)[-length(PROPORTION)]), x = "", size = 2.5,label = c(myLabel[1],"","","","","","")))# sum(PROPORTION)/100, label = myLabel), size = 2.5) 
ppt.overall
#############################################################################################################3


h1b.data$City <- gsub("^(.*?), (.*)", "\\1", h1b.data$WORKSITE)
h1b.data$State <- gsub("^(.*?), (.*)", "\\2", h1b.data$WORKSITE)


certified_df <- h1b.data%>%
  filter(CASE_STATUS == 'CERTIFIED')
certified_df$YEAR = factor(certified_df$YEAR)

# Select the states where the certified with 
# h1b visa where issued in total per year
######################## top 25 states########################3
bar_df <- h1b.data%>%
  group_by(State)%>%
  summarize(Total_Certified = n())%>%
  arrange(desc(Total_Certified))

topStates <- bar_df$State[1:25]

df2 <- certified_df%>%
  filter(State %in% topStates)

df2$State <- factor(df2$State, levels = topStates)

# bar chart of top 25 states for the last 6 years.
top.state= ggplot()+
  geom_bar(data = df2, aes(x = State),fill="lightblue")+
  scale_y_continuous(limits = c(0, 500000), 
                     breaks = seq(0, 500000, 50000))+
  ylab("No of App")+
  ggtitle("Top 25 States with the most Applicants")+get_theme()
  #theme(
   # plot.title = element_text(size = rel(2)),
    #panel.background = element_rect(fill = '#e0e0d1'),
    #axis.text.x=element_text(angle = -90, hjust = 0),
    #panel.grid.major.x = element_line(linetype = 'blank'), 
    #panel.grid.major = element_line(colour = "#e8e8e8"),
    #panel.grid.minor = element_line(linetype = 'blank')
  #)
top.state


#############################Top Employers######################################33
employer <- h1b.data%>%
  group_by(EMPLOYER_NAME)%>%
  summarize(Total_Certified = n())%>%
  arrange(desc(Total_Certified))

topEmployers <- employer$EMPLOYER_NAME[1:20]

df_employer <- certified_df%>%
  filter(EMPLOYER_NAME %in% topEmployers)

df_employer$EMPLOYER_NAME <- factor(df_employer$EMPLOYER_NAME, levels = topEmployers)

top.employer= ggplot()+
  geom_bar(data = df_employer, aes(x = EMPLOYER_NAME ),fill="lightblue")+
  ylab("No of App")+
  ggtitle("Top 20 Employers with the most Applicants (2011 - 2016)")+get_theme()

top.employer 
#########################################################################

################ Top Job Title###############################################

bar_title <- h1b.data%>% filter(is.na(CASE_STATUS)==0) %>%
  group_by(JOB_TITLE)%>%
  summarize(Job_titles = n())%>%
  arrange(desc(Job_titles))

topJobs <- bar_title$JOB_TITLE[1:25]

job_title <- h1b.data%>% filter(is.na(CASE_STATUS)==0) %>%
  filter(JOB_TITLE %in% topJobs)

job_title$JOB_TITLE <- factor(job_title$JOB_TITLE, levels = topJobs)

# visualizing total number of applications per each top 25 job titles
top.title =ggplot()+
  geom_bar(data = job_title, aes(x = JOB_TITLE), fill = "lightblue")+
  ggtitle('Top 25 Job Titles with the most Applicants (2011-2016)')+ get_theme()+
  xlab("Job Title")+ylab("No of App")

top.title


###############################################################################3




h1b.2016=h1b.data[h1b.data$YEAR==2016,]



case.status.2016 <- as.data.frame(h1b.2016 %>% filter(.,!is.na(CASE_STATUS))
                             %>% group_by(.,CASE_STATUS) %>%
                               summarise(.,PROPORTION = round(n()*100/nrow(h1b.2016),1)))

# Missing values



# X        CASE_STATUS      EMPLOYER_NAME           SOC_NAME 
# 0                 13                 45              17733 
# JOB_TITLE FULL_TIME_POSITION    PREVAILING_WAGE               YEAR 
# 38                 15                 85                 13 
# WORKSITE                lon                lat 
# 0             107242             107242 


h1b.data = h1b.data %>% mutate(YEAR = as.character(YEAR))


#############################CASE_STATUS COUNT by YEAR################################3
case.status.year = as.data.frame(h1b.data %>% filter(.,!is.na(CASE_STATUS))
                                %>% group_by(YEAR,CASE_STATUS) %>%
                                  summarise(Count = n()))
year.ct = ggplot(case.status.year, aes(x=CASE_STATUS, y=Count, fill=YEAR)) + 
  geom_bar(stat="identity", position="dodge")+get_theme1()+scale_fill_brewer(palette="Blues")+ggtitle("Case Status (2011 - 2016)")

year.ct
##############################PETITION by YEAR#################################################
h1b.data.rn= h1b.data %>% filter(!is.na(CASE_STATUS))
year.count = h1b.data.rn %>% group_by(YEAR)%>% summarise(All = n())
year.certified = case.status.year[case.status.year$CASE_STATUS=="CERTIFIED",]
names(year.certified)[3]="Certified"
h1b.year = cbind(year.count,year.certified["Certified"])
h1b.year1=cbind(rep(c(2011,2012,2013,2014,2015,2016),2),stack(h1b.year[,c(2,3)]))
names(h1b.year1)=c("Year","Count","Item")
year.trend = ggplot(h1b.year1, aes(x=factor(Year), y=Count, fill=factor(Item))) + 
  geom_bar(stat="identity", position="dodge")+get_theme()+scale_fill_brewer(palette="Blues")+
  ggtitle("H1B Petitions by Year (2011 - 2016)")+xlab("Year")

year.trend
##################################################################################


##################PROPORTION by YEAR###############################
h1b.year$Proportion = h1b.year$Certified/h1b.year$All
proportion.label = paste(round(h1b.year$Proportion*100,2),"%")
proportion.by.year = ggplot(h1b.year, aes(x=factor(YEAR),y =Proportion))+
  geom_bar(stat= "identity",position = "dodge",fill = "lightblue")+get_theme()+scale_fill_brewer()+
  ggtitle("Certified Proportion (2011 - 2016)")+geom_text(aes(label=proportion.label))+ xlab("Year")
proportion.by.year
########################################################################################
input <- plot_input(h1b.data, "EMPLOYER_NAME", "YEAR", 
                    "TotalApps",filter = TRUE, Ntop = 10)

top.employers.plot <- plot_output(input, 'EMPLOYER_NAME','YEAR','TotalApps', 
                 'EMPLOYER','TOTAL NO. of APPLICATIONS')+ 
                 theme(axis.title = element_text(size = rel(1.5)),
                       axis.text.y = element_text(size=rel(1)))


share <- plot_output(input, 'EMPLOYER_NAME','YEAR','Share', 'EMPLOYER','% SHARE') +  
  theme(axis.title = element_text(size = rel(1.5)),
                                                                                
               axis.text.y = element_text(size=rel(1)))

#Finding the top 5 employers with the most petitions
top_employers <- unlist(find_top(h1b.data,"EMPLOYER_NAME","TotalApps",Ntop = 10))

####################333#Finding the most common Job Titles with Full-Time Positions
h1b.data %>%
  filter(EMPLOYER_NAME %in% top_employers & FULL_TIME_POSITION == 'Y') %>%
  group_by(JOB_TITLE) %>%
  summarise(COUNT = n()) %>%
  arrange(desc(COUNT)) -> common_jobs


high_jobs.full <- ggplot(common_jobs[1:20,], aes(x=reorder(JOB_TITLE,COUNT),y=COUNT)) +
  geom_bar(stat = "identity", fill = "lightblue") + coord_flip() +
  xlab("JOB TITLE") + ylab("TOTAL NO. OF APPLICATIONS") + get_theme()+
  ggtitle("Top 20 Job Titles with Full-Time Positions in the most popular companies")

high_jobs.full
#********************SOC_NAMES with the highest wage******************************************************************************

job_wages_df = as.data.frame(h1b.data %>% group_by(SOC_NAME) %>% 
  summarise(WAGE = median(PREVAILING_WAGE),Count = n()) %>% arrange(desc(WAGE)))

wage = ggplot(job_wages_df[1:20,], aes(x=reorder(SOC_NAME,WAGE),y = WAGE)) +
  geom_bar(stat = "identity",fill = "lightblue") + xlab("SOC_NAME") + ylab("WAGE (USD)") +
  get_theme()+coord_flip()+ggtitle("Top 20 SOC_NAMES with the highest wage")

wage

job_list <- c("Data Scientist","Data Engineer","Machine Learning","Statistician")

data_science_df <- plot_input(job_filter(h1b.data,job_list),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")

data.science = plot_output(data_science_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")


data.wage = ggplot(job_filter(h1b.data,job_list), aes(x=JOB_INPUT_CLASS,y= PREVAILING_WAGE)) +
  geom_boxplot(aes(fill=YEAR)) + xlab("JOB TITLE") + ylab("WAGE (USD)") +
  get_theme() + coord_cartesian(ylim=c(25000,200000))



h1b.data %>%
  mutate(SOC_NAME = toupper(SOC_NAME)) -> h1b.data

job_filter(h1b.data,job_list) %>%
  filter(!is.na(SOC_NAME)) %>%
  group_by(SOC_NAME) %>%
  summarise(TotalApps = n(), Wage = median(PREVAILING_WAGE)) %>%
  filter(TotalApps > 10) %>% 
  arrange(desc(Wage))

USA = map_data(map = "usa")

h1b.map <- map_gen(job_filter(h1b.data,job_list),"TotalApps",USA,Ntop = 3)

h1b.map
