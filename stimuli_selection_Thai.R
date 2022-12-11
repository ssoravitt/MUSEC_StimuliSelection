library(dplyr)
library(ggplot2)
library(reshape)
library(textcat)
library(forcats)
data2018 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/TH2018.csv")
data2019 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/TH2019.csv")
data2020 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/TH2020.csv")
data2021 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/TH2021.csv")

top50 <- function(data) {
  top50_filter <- data %>% filter(if_all(starts_with("peak"), ~ .<= 1),
                                  weeks_on_chart > median(data$weeks_on_chart),
                                  # weeks_on_chart_gb > median(data$weeks_on_chart_gb),
                                  # weeks_on_chart_aus > median(data$weeks_on_chart_aus),
                                  # weeks_on_chart_can > median(data$weeks_on_chart_can),
                                  # weeks_on_chart_gbr > median(data$weeks_on_chart_gbr),
                                  # weeks_on_chart_nzl > median(data$weeks_on_chart_nzl),
                                  # weeks_on_chart_usa > median(data$weeks_on_chart_usa),
                                  # weeks_on_chart_arg > median(data$weeks_on_chart_arg),
                                  # weeks_on_chart_col > median(data$weeks_on_chart_col),
                                  # weeks_on_chart_esp > median(data$weeks_on_chart_esp),
                                  # weeks_on_chart_mex > median(data$weeks_on_chart_mex),
                                  # weeks_on_chart_per > median(data$weeks_on_chart_per),
  )
  return(top50_filter)
}  

merge_top50 <- function(data, year) {
  result <- data %>%
    mutate(year_on_chart = year) %>% 
    select(year_on_chart, track_name, artist_names, source,  starts_with('peak'), starts_with('week'))
  
  return (result)
}

top50_THchart <- rbind(merge_top50(top50(data2018), 2018),
                         merge_top50(top50(data2019), 2019),
                         merge_top50(top50(data2020), 2020),
                         merge_top50(top50(data2021), 2021))

source_list = c("GMM GRAMMY PCL", "What The Duck", "Smallroom", "Og-Anic",
                "LOVEiS", "123Records", "HOLYFOX CO., LTD.", "Nadao Music",
                "Already Deadd Records", "Yupp", "Marr", "Macrowave", 
                "Spicy Disc", "Boxx Music", "Anatomy Rabbit", "MEYOU")

top50_THchart_onlyTHsongs <- top50_THchart %>% 
  filter(source %in% source_list) %>%
  distinct(track_name, .keep_all = TRUE) 

top50_THchart_onlyTHsongs %>%
  # mutate(name = fct_reorder(peak_rank, track_name)) %>%
  ggplot(aes(peak_rank, as.numeric(row.names(top50_THchart_onlyTHsongs))))+ #color=year_on_chart
  geom_point(aes(size = weeks_on_chart), alpha = 0.5)+
  geom_smooth()+ # create smooth of line
  # facet_wrap(~status)+
  labs(title = "25 popular Thai songs reaching to No.1", y = "No. of songs")+
  xlim(0.75, 1.25)
  ggsave(file="./MUSEC_SpotifybyOrn/Visualization/plot_popularThaisongs.png", width=4, height=4, dpi=300)

  

as.numeric(top50_THchart_onlyTHsongs)




