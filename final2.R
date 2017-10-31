data.related = job_filter(h1b.data,c("data","statistic"))
data.related %>% group_by(JOB_TITLE)%>%summarise(Count = n(), WAGE = median(PREVAILING_WAGE)) %>%
  arrange(desc(Count)) -> data.job.count
data.related %>% group_by(JOB_TITLE)%>%summarise(Count = n(), WAGE = median(PREVAILING_WAGE)) %>%
  arrange(desc(WAGE)) -> data.job.wage
data.job.wage = data.job.wage[data.job.wage$WAGE<200000,]
data.top.jobs=
  ggplot(data.job.count[1:10,])+geom_bar(aes(x = reorder(JOB_TITLE,desc(Count)), y = Count),
                                       stat = "identity",fill = "lightblue")+
  get_theme()+ggtitle("Top 10 Data related jobs with the most applicants")+
  xlab("Job Title")+ylab("Count")
data.top.jobs

data.top.wage=
  ggplot(data.job.wage[1:20,])+geom_bar(aes(x = reorder(JOB_TITLE,desc(WAGE)), y = WAGE),
                                         stat = "identity",fill = "lightblue")+
  get_theme()+ggtitle("Top 10 Data related jobs with the highest wage")+
  xlab("Job Title")+ylab("Count")
data.top.wage


for (i in 2011:2016){
data.201x = job_filter(h1b.data[h1b.data$YEAR==i,],"data")
data.201x %>% group_by(JOB_TITLE)%>%summarise(Count = n(), WAGE = median(PREVAILING_WAGE)) %>%
  arrange(desc(Count)) -> data.job.201x
data.plot=  
ggplot(data.job.201x[1:10,])+geom_bar(aes(x = reorder(JOB_TITLE,desc(Count)), y = Count),
                                         stat = "identity",fill = "lightblue")+
  get_theme()+ggtitle(paste("Top 10 Data related jobs with the most applicants in",i))+
  xlab("Job Title")+ylab("Count")
print(data.plot)
}


job_list <- c("DATABASE","DATA ANALY","Data SCIEN","DATA ENGINEER","STATISTIC","FINANCIAL ANALY")

data_science_df <- plot_input(job_filter(h1b.data,job_list),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")

data.science = plot_output(data_science_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")+
  ggtitle("No of Applicants in data related jobs")
h1b.data1 = h1b.data[h1b.data$FULL_TIME_POSITION=="Y",]
data.wage = ggplot(job_filter(h1b.data1,job_list), aes(x=JOB_INPUT_CLASS,y= PREVAILING_WAGE)) +
  geom_boxplot(aes(fill=YEAR)) + xlab("JOB TITLE") + ylab("WAGE (USD)") +
  get_theme() + coord_cartesian(ylim=c(25000,200000))+scale_fill_brewer(palette="Blues")+
  ggtitle ("Wage of Data and Statistic related Full-time Jobs")
data.wage

job_list2 = c("Machine Learning", "Data mining", "Data Scientist", "Data Research","Statistic" )
data.wage.ml = ggplot(job_filter(h1b.data1,job_list2), aes(x=JOB_INPUT_CLASS,y= PREVAILING_WAGE)) +
  geom_boxplot(aes(fill=YEAR)) + xlab("JOB TITLE") + ylab("WAGE (USD)") +
  get_theme() + coord_cartesian(ylim=c(25000,200000))+scale_fill_brewer(palette="Blues")+
  ggtitle ("Wage of Interested Data and Statistic related Full-time Jobs")
data.wage.ml

job_list <- c("Data Scientist","Data Engineer","Machine Learning","Data Scientist","Statistic")

data_science_df <- plot_input(job_filter(h1b.data,job_list2),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")

h <- plot_output(data_science_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")+
  ggtitle("No of Applicants in data related jobs (2011 - 2016)")

h


data.state = job_filter(h1b.data1,c("data","statistic"))%>%filter(is.na(State)==0)%>%group_by(State)%>%
  filter(PREVAILING_WAGE<200000)

data.state.plot = ggplot(data.state, aes(x=reorder(State,PREVAILING_WAGE,median),y= PREVAILING_WAGE)) +
  geom_boxplot(fill = "lightblue") + xlab("State") +ylab("WAGE(USD)") +
  get_theme() + ggtitle ("WAGE of Data and Statistic related Jobs in 50 states")

data.state.plot

data.state.app = data.state %>% group_by(State)%>% summarise(Count = n())%>% arrange(desc(Count))
data.app.plot = ggplot(data.state.app)+
  geom_bar(aes(x = reorder(State,desc(Count)),y = Count),stat = "identity", 
           fill = "lightblue")+get_theme()+ggtitle("No of Applicants in data related jobs in states")+
  xlab("State")
data.app.plot

data.ctf.dn = h1b.data.rn %>% group_by(CASE_STATUS)
ctf.dn.wage = ggplot(data.ctf.dn,aes(x = CASE_STATUS, y = PREVAILING_WAGE)) + 
  geom_boxplot(aes(fill = YEAR))+xlab("Case Status")+ylab("Wage (USD)")+get_theme()+
  scale_fill_brewer(palette="Blues")+ggtitle("Wages of different Case Status")


#---------------------------------------
h1b_boxplot <- h1b.data%>%
  filter(CASE_STATUS == 'CERTIFIED' | CASE_STATUS == 'DENIED')

ggplot(aes(y = PREVAILING_WAGE, x = CASE_STATUS, fill = CASE_STATUS,
           notch = TRUE, notchwidth = .3), 
       data = h1b_boxplot) + 
  geom_boxplot(notch = TRUE,fill = "lightblue") + 
  scale_fill_manual(values = c("#29a329", "#ea4b1f"))+
  scale_y_continuous(limits = c(0, 200000), 
                     breaks = seq(0, 200000, 50000)) + 
  ggtitle("Wages for certified & denied H1B cases (Overall)")+get_theme()
  

# plot boxplot
ggplot(aes(y = PREVAILING_WAGE, x = YEAR, Y = PREVAILING_WAGE,
           notch = TRUE, notchwidth = .3), 
       data = h1b_boxplot) + 
  geom_boxplot(notch = TRUE,aes(fill = CASE_STATUS)) + 
 scale_fill_brewer(palette="Blues")+
  scale_y_continuous(limits = c(0, 200000)) + 
  ggtitle("Wages for certified & denied H1B cases")+get_theme()

#Map for states with the most certified
bar_df <- data.related%>%
  group_by(State)%>%
  summarize(Total_Certified = n())%>%
  arrange(desc(Total_Certified))

topStates <- bar_df$State[1:11]

df2 <- data.related>%
  filter(State %in% topStates)

h1b_df <- df2
h1b_df$lat <- as.numeric(h1b_df$lat)
h1b_df$lon <- as.numeric(h1b_df$lon)
coordinates(h1b_df) <- ~ lon + lat
proj4string(h1b_df) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

map <- MapBackground(lat = h1b_df$lat, lon = h1b_df$lon)

PlotOnStaticMap(map, lat = h1b_df$lat, lon = h1b_df$lon, 
                GRAYSCALE = F, NEWMAP = T, pch = 21, 
                col = "#29a329", cex = 1.1, zoom = 1, FUN = points)

bar_city <- certified_df%>%
  group_by(City)%>%
  summarize(Count = n())%>%
  arrange(desc(Count))

topCities <- bar_city$City[1:25]

df_city <- certified_df%>%
  filter(City %in% topCities)

h1b_df2 <- df_city
h1b_df2$lat <- as.numeric(h1b_df2$lat)
h1b_df2$lon <- as.numeric(h1b_df2$lon)
coordinates(h1b_df2) <- ~ lon + lat
proj4string(h1b_df2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

map <- MapBackground(lat = h1b_df2$lat, lon = h1b_df2$lon)

PlotOnStaticMap(map, lat = h1b_df2$lat, lon = h1b_df2$lon, 
                GRAYSCALE = F, NEWMAP = T, pch = 19, 
                col = "#29a329", cex = 1.5, zoom = 1, FUN = points)

# t-test for wage mean in certified and denied
c.wage=h1b.data$PREVAILING_WAGE[h1b.data$CASE_STATUS=="CERTIFIED"]
c.wage=c.wage[c.wage<200000]
d.wage=h1b.data$PREVAILING_WAGE[h1b.data$CASE_STATUS=="DENIED"]
d.wage=d.wage[d.wage<200000]
summary(t.test(c.wage,d.wage,"greater"))

data_wage = data.related$PREVAILING_WAGE[data.related$JOB_INPUT_CLASS == "DATA"]
data_wage = data_wage[data_wage<200000]
stat_wage = data.related$PREVAILING_WAGE[data.related$JOB_INPUT_CLASS == "STATISTIC"]
stat_wage = stat_wage[stat_wage<200000]
t.test(data_wage,stat_wage,"greater")

