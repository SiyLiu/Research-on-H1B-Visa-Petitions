#FUNCTION1
job_filter <- function(df,input_vec) {
  # Function to filter only the rows from dataframe 
  # with Job titles provided in the inputs
  # Inputs:
  # df         : H-1B dataset dataframe
  # input_vec  : vector of job types input
  # Output     : filtered dataframe
  # If no match, returns an empty data frame
  # If the inputs are all equal to "", it returns the complete dataframe
  # A new column JOB_INPUT_CLASS is created to identify the Job Type
  # If multiple job type inputs match with a single row in the dataframe df, the 
  # output contains them in different rows each with distinct JOB_INPUT_CLASS
  
  # If input_vec is empty, return without any filtering
  if(length(input_vec) == 0) {
    return(df %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      filter(regexpr(value,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(value)))
    # regexpr(value, text, ignore.case=FALSE by default)
    # return 1: sucess, -1 not 
  }
  return(unique(new_df))
}

#FUNCTION2

find_top <- function(df,x_feature,metric, Ntop = 3) {
  # Function to find the top values in x_feature based on metric value
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric        : metric for data comparison 
  # Output        : list of top values in x_feature based on metric
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    # Metrics that I will be using in my data analysis
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE,na.rm=T), 
              CertiApps = sum(certified),
              Ratio = CertiApps/TotalApps) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  print(dim(top_df))
  return(top_df[1:top_len,])
}

# top.title= find_top(h1b.data, "JOB_TITLE", "TotalApps", 20)
# p2 <- ggplot(top.title, aes(x = reorder(JOB_TITLE, -TotalApps), y = TotalApps)) +
#      geom_bar(stat = "identity")+
#   xlab("Job_TITLE")+
#   ylab("Total Application")+
#   ggtitle("Top 25 states hiring the most")+
#   theme(
#     plot.title = element_text(size = rel(2)),
#     panel.background = element_rect(fill = '#e0e0d1'),
#     axis.text.x=element_text(angle = -90, hjust = 0),
#     panel.grid.major.x = element_line(linetype = 'blank'), 
#     panel.grid.major = element_line(colour = "#e8e8e8"),
#     panel.grid.minor = element_line(linetype = 'blank')
#   )
# top.ctf.title = find_top(h1b.data, "JOB_TITLE", "CertiApps",20)
# 

#FUNCTION3



plot_input <- function(df, x_feature, fill_feature, metric,filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
  # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
  
  #Finding out the top across the entire range independent of the fill_feature e.g. Year
  top_x <- unlist(find_top(df,x_feature,metric, ...))
  
  # lazyeval package interp () generates expression that interprets x_feature and metric arguments
  # this is fed into filter_ and arrange_ accordingly
  # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  #Grouping by not just x_feature but also fill_feature
  return(df %>% 
           group_by_(.dots=c(x_feature,fill_feature)) %>% 
           mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
           # metrics I will be using in my data analysis   
           summarise(TotalApps = n(),
                     CertiApps = sum(certified), 
                     Wage = median(PREVAILING_WAGE),
                     Share = CertiApps/850))
}

plot_output <- function(df, x_feature,fill_feature,metric, xlabb,ylabb) {  
  # Function to plot output
  # Inputs:
  # df            : dataframe output of plot_input()
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # xlabb         : x label
  # ylabb         : y label
  # Output        : ggplot object
  
  # Prevents numbers on plot transforming into scientific notation
  options(scipen = 999)
  
  g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
    geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
    xlab(xlabb) + ylab(ylabb) + get_theme()+scale_fill_brewer(palette="Blues")
  
  return(g)
}


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.2)),
          legend.position = "right",
          plot.title = element_text(colour = "black", face = "bold", 
                                    size = 20, hjust = 0.5),
          axis.text.x=element_text( hjust = 0,size = rel(1.2)),
          legend.text = element_text(size = rel(1)),
          legend.title = element_text(size=rel(1)))
    
  )
}


get_theme1 <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1)),
          legend.position = "right",
          axis.text.x=element_text(angle = -20, hjust = 0),
          legend.text = element_text(size = rel(1)),
          legend.title = element_text(size=rel(1)))
  )
}


map_gen <- function(df,metric,USA,...) {
  # Function to generate map plot for given metric in df 
  # This is laid on top of USA map
  # Inputs:
  # df      : dataframe with metrics, lat, lon, WORKSITE columns
  # metric  : metric for data comparison 
  # USA     : dataframe for US maps with lat, long columns. map_data(map = "usa") from ggplot2
  # Output  : ggplot object
  
  
  # Creating Map Dataframe
  df %>%
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    group_by(WORKSITE,lat,lon) %>%
    summarise(TotalApps = n(),CertiApps = sum(certified), Wage = median(PREVAILING_WAGE)) -> map_df
  
  # # Lat-Long Limits
  # df %>%
  #   summarise(lat_min = min(lat,na.rm=TRUE),
  #             lat_max = max(lat,na.rm=TRUE),
  #             long_min = min(lon,na.rm=TRUE),
  #             long_max = max(lon,na.rm=TRUE)) -> geo_coord
  
  # Finding top Locations for metric
  top_locations <- unlist(find_top(df,"WORKSITE",metric, ...))
  
  # First layer    : USA Map
  # Second layer   : geom_point() with point alpha and size varying with metric
  # Third layer    : points mapping to top locations using ggrepel package
  g <- ggplot(USA, aes(x=long, y=lat)) + 
    geom_polygon() + xlab("Longitude (deg)") + ylab("Latitude(deg)") + 
    geom_point(data=map_df, aes_string(x="lon", y="lat", label = "WORKSITE", alpha = metric, size = metric), color="yellow") + 
    geom_label_repel(data=map_df %>% filter(WORKSITE %in% top_locations),aes_string(x="lon", y="lat",label = "WORKSITE"),
                     fontface = 'bold', color = 'black',
                     box.padding = unit(0.0, "lines"),
                     point.padding = unit(1.0, "lines"),
                     segment.color = 'grey50',
                     force = 3) +
    # Zoom into the specific location input
    #coord_map(ylim = c(max(geo_coord$lat_min - 5,23), min(geo_coord$lat_max - 5,50)),xlim=c(max(geo_coord$long_min - 5,-130),min(geo_coord$long_max + 5,-65))) +
    # Using the whole USA map
    coord_map(ylim = c(23,50),xlim=c(-130,-65)) +
    get_theme()
  
  return(g)
}
