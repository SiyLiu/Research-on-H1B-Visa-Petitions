
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
library(fiftystater)
library(sqldf)


file=choose.files()
h1b.data=read_csv(file)
full_data = read_csv(file)

dim(h1b.data)
colnames(h1b.data)[1]="id"
apply(h1b.data, 2, function(x) sum(is.na(x)))
# id        CASE_STATUS      EMPLOYER_NAME           SOC_NAME 
# 0                 13                 45              17733 
# JOB_TITLE FULL_TIME_POSITION    PREVAILING_WAGE               YEAR 
# 38                 15                 85                 13 
# WORKSITE                lon                lat 
# 0             107242             107242 

# Data cleaning - 1
h1b.data = h1b.data[!is.na(h1b.data$YEAR),]
dim(h1b.data)
full_data$WORKSITE[grep("WASHINGTON D([. ])?C.?", full_data$WORKSITE)] = "WASHINGTON D.C., DISTRICT OF COLUMBIA" 
h1b.data$WORKSITE[grep("WASHINGTON D([. ])?C.?", h1b.data$WORKSITE)] = "WASHINGTON D.C., DISTRICT OF COLUMBIA" 

# Summary by Year
p_year = full_data%>%filter(!is.na(YEAR))%>%group_by(.,YEAR)%>%summarise(.,Num_Petitions = n())%>%as.data.frame()
ggplot(data = p_year, aes(x = YEAR, y = Num_Petitions/1000)) +  
  geom_line() + geom_point() + get_theme()  +
  labs(x="Year", y="Applications (thousands)", 
       title="Amount of petitions trend (2011-2016)")+geom_text(aes(label=Num_Petitions))

# Summary by Year and Case status
full_data %>% filter(!is.na(CASE_STATUS)) %>%  filter(!is.na(YEAR)) %>% group_by(CASE_STATUS,YEAR) %>% summarise(nr = n()) %>% as.data.frame()
# INVALIDATED 2014      1
# PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED 2013     15
# REJECTED 2014      2

# Data cleaning - 2
h1b.data$CASE_STATUS[h1b.data$CASE_STATUS=="CERTIFIED-WITHDRAWN"] = "CERTIFIED"
h1b.data = h1b.data%>%filter(., CASE_STATUS %in% c("CERTIFIED","DENIED","CERTIFIED-WITHDRAWN") )
h1b.data$CASE_STATUS = ifelse(h1b.data$CASE_STATUS=="CERTIFIED",1,0)

h1b.data$City <- gsub("^(.*?), (.*)", "\\1", h1b.data$WORKSITE)
h1b.data$State <- gsub("^(.*?), (.*)", "\\2", h1b.data$WORKSITE)

full_data$City = gsub("^(.*?), (.*)", "\\1", full_data$WORKSITE)
full_data$State = gsub("^(.*?), (.*)", "\\2", full_data$WORKSITE)

## Addressing missing values
h1b.data$SOC_NAME = ifelse(!is.na(h1b.data$SOC_NAME), h1b.data$SOC_NAME, "undefined")
h1b.data$PREVAILING_WAGE[is.na(h1b.data$PREVAILING_WAGE)] = 
  median(na.omit(h1b.data$PREVAILING_WAGE[(h1b.data$SOC_NAME==h1b.data$SOC_NAME[is.na(h1b.data$PREVAILING_WAGE)] & 
                                             h1b.data$WORKSITE==h1b.data$WORKSITE[is.na(h1b.data$PREVAILING_WAGE)])]))
h1b.data$log_wage = log(h1b.data$PREVAILING_WAGE)
h1b.data = h1b.data[!is.na(h1b.data$EMPLOYER_NAME) & !is.na(h1b.data$JOB_TITLE) & !is.na(h1b.data$FULL_TIME_POSITION),] 
dim(h1b.data)
h1b.data$CASE_STATUS=as.factor(h1b.data$CASE_STATUS)
h1b.data$YEAR = as.factor(h1b.data$YEAR)
h1b_data = as.data.frame(h1b.data)

# Exploratory Data Analysis
# Acceptance rate by year
year_rate = h1b_data%>%group_by(YEAR)%>%summarise(Certified_rate = sum(CASE_STATUS=="1")/n())%>% as.data.frame()
ggplot(data = year_rate, aes(x = YEAR, y = Certified_rate, group = 1)) +  
  geom_line() + geom_point() + get_theme()  +
  labs(x="Year", y="Applications (thousands)", 
       title="Certified Rate (2011-2016)")+geom_text(aes(label=round(Certified_rate,3)))
# 


#############################Top Employers######################################33
employer <- full_data%>%filter(.,!is.na(EMPLOYER_NAME))%>%
  group_by(EMPLOYER_NAME)%>%
  summarize(Num_Petitions = n())%>%
  arrange(desc(Num_Petitions))%>%as.data.frame()

topEmployers <- employer[1:20,]

ggplot()+
  geom_bar(data = topEmployers, aes(x = reorder(EMPLOYER_NAME, - Num_Petitions),y = Num_Petitions  ),stat = "identity")+
  ylab("No of App")+ xlab('Employer Name')+get_theme1()+
  ggtitle("Top 20 Employers with the most Applicants (2011 - 2016)")


job_title = full_data%>%filter(!is.na(JOB_TITLE))%>%
  group_by(JOB_TITLE)%>%
  summarise(Num_Petitions= n())%>%
  arrange(desc(Num_Petitions))%>%as.data.frame

ggplot()+
  geom_bar(data = top_job, aes(x = reorder(JOB_TITLE, - Num_Petitions),y = Num_Petitions  ),stat = "identity")+
  ylab("No of App")+ xlab('Employer Name')+get_theme1()+
  ggtitle("Top 20 Popular Job Titles (2011 - 2016)")


top_job = job_title[1:20,]

##########################TOP State#####################################
state = full_data%>%filter(State!="NA")%>%group_by(State)%>%summarise(Num_Petitions = n())%>%arrange(desc(Num_Petitions))%>%as.data.frame()
topStates = state[1:15,]
state_salary = full_data%>%filter(!is.na(PREVAILING_WAGE))%>%filter(State!="NA")%>%group_by(State)%>%summarise(Median = median(PREVAILING_WAGE))%>%arrange(desc(Median))%>%as.data.frame()
low_color='#ccdbe5' 
high_color="#114365"
legend_title = "Geographic Distribution"
ggplot(state, aes(map_id = tolower(State))) + 
  geom_map(aes(fill = Num_Petitions ),color="#ffffff",size=.15, map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_continuous(low = low_color, high= high_color, guide = guide_colorbar(title = legend_title)) + # creates shading pattern
  theme(#legend.position = "bottom", 
    panel.background = element_blank()) + 
  fifty_states_inset_boxes() +
  ggtitle('Geographic Features for # of Petitions(2011-2016)')+get_theme()


ggplot(state_salary, aes(map_id = tolower(State))) + 
  geom_map(aes(fill = Median ),color="#ffffff",size=.15, map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_continuous(low = low_color, high= high_color) + # creates shading pattern
  theme(#legend.position = "bottom", 
    panel.background = element_blank()) + 
  fifty_states_inset_boxes() +
  ggtitle('Geographic Feature of Salary(2011-2016)')+get_theme()




ggplot(topStates)+geom_bar(aes(x =reorder(State, -Num_Petitions),
                               y = Num_Petitions), stat = "identity")+
  get_theme1()+ylab("No of App")+ggtitle("Top 15 States with the Most Applicants")

ggplot(state_salary[1:20,])+geom_bar(aes(x =reorder(State, -Median),
                                  y = Median), stat = "identity")+
  get_theme1()+ylab("Prevailing Wage")+xlab("State")+ggtitle("Top 20 States with the Highest Salary")

################################TOP CITIES###################################
full_data$City[full_data$City=="WINSTON SALEM"]="WINSTON-SALEM"
full_data$City[full_data$City=="WINSTON- SALEM"]="WINSTON-SALEM"

city = full_data%>%group_by(City)%>%summarise(Num_Petitions=n())%>%arrange(desc(Num_Petitions))%>%as.data.frame()
topCities = city[1:30,]
ggplot(topCities)+geom_bar(aes(x =reorder(City, -Num_Petitions),
                               y = Num_Petitions), stat = "identity")+
  get_theme1()+ylab("No of App")+ggtitle("Top 20 Cities with the Most Applicants")

############################Quantitative Related###############################
quantitative = sqldf('select * from full_data where JOB_TITLE like "%Quantitative%"')
quan_employer = quantitative%>%group_by(EMPLOYER_NAME)%>%summarise(Num_Petitions = n())%>%arrange(desc(Num_Petitions))%>%as.data.frame()
colnames(quan_employer)[2]="num"
ggplot(quan_employer[1:30,],aes(x = reorder(EMPLOYER_NAME, -num), y = num,label = num))+geom_bar(stat = "identity")+get_theme1()+
  geom_text()+xlab("Employer Name")+ylab("No. of App")+ggtitle("Top 30 Employers about Quantitative Jobs")

# Salary Trend
quan_salary = quantitative%>%group_by(YEAR)%>%summarise(Median = median(PREVAILING_WAGE))%>%as.data.frame()
ggplot(data = quan_salary, aes(x = YEAR, y = Median)) +  
  geom_line() + geom_point() + get_theme()  +
  labs(x="Year", y="Prevailing Wage", 
       title="Wage Trand for Quantitative Jobs (2011-2016)")+geom_text(aes(label=Median))

#####################################Data Related#################################
data_analyst = sqldf('select * from full_data where JOB_TITLE like "%Data%Analyst%"')
data_engineer = sqldf('select * from full_data where JOB_TITLE like "%Data%Engineer%"')
data_scientist = sqldf('select * from full_data where JOB_TITLE like "%Data%Scientist%"')
statistics = sqldf('select * from full_data where JOB_TITLE like "%Statistic%"')
business = sqldf('select * from full_data where JOB_TITLE like "%Business%Analyst%"')
# Salary
SALARY = function(dataframe){  
res = dataframe%>%group_by(YEAR)%>%summarise(Median = median(PREVAILING_WAGE))%>%as.data.frame()
return(res)
}

ds_salary = cbind(rep("Data Science",6),SALARY(data_scientist))
colnames(ds_salary)[1]="Job_title"
de_salary = cbind(rep("Data Engineer",6),SALARY(data_engineer))
colnames(de_salary)[1]="Job_title"
da_salary = cbind(rep("Data Analyst",6),SALARY(data_analyst))
colnames(da_salary)[1]="Job_title"
stat_salary = cbind(rep("Statistics",6), SALARY(statistics))
colnames(stat_salary)[1]="Job_title"
business_salary = cbind(rep("Business Analyst",6), SALARY(business))
colnames(business_salary)[1]="Job_title"

data_salary = rbind(ds_salary, de_salary,da_salary, stat_salary,business_salary)

ggplot(data_salary, aes(x=YEAR,y=Median,fill = Job_title, group  = Job_title)) +
  geom_bar(stat = "identity", aes(fill = Job_title, group = Job_title), position = 'dodge') + 
  xlab("YEAR") + ylab("Median Salary") + get_theme()+scale_fill_brewer(palette="Blues")+geom_text(aes(label = Median), position = position_dodge(0.9),vjust = 0.5,size = 3)+
  ggtitle("Salary Trend for Data Jobs")

#############################EDA about Predictive Analysis#########################
employer_rate = h1b.data%>%group_by(EMPLOYER_NAME)%>%summarise(Num_Petitions = n(),CERTIFIED_RATE = sum(CASE_STATUS=="1")/n())
h1b_data = as.data.frame(h1b.data)
h1b_data  = sqldf('select *
      from h1b_data join employer_rate using (EMPLOYER_NAME)')
soc_rate = h1b.data%>%group_by(SOC_NAME)%>%summarise(SOC_Num = n(), SOC_RATE = sum(CASE_STATUS=="1")/n())
h1b_data = sqldf('select *
      from h1b_data join soc_rate using (SOC_NAME)')
h1b_data$FULL_TIME_POSITION = as.factor(h1b_data$FULL_TIME_POSITION)



############WORD CLOUD

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

