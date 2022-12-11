library(dplyr)
library(ggplot2)
library(reshape)
data2018 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/commonsongs2018.csv")
data2019 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/commonsongs2019.csv")
data2020 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/commonsongs2020.csv")
data2021 = read.csv("MUSEC_SpotifybyOrn/Unique Songs/commonsongs2021.csv")

top50 <- function(data) {
  top50_filter <- data %>% filter(if_all(starts_with("peak"), ~ .<= 50),
                                  weeks_on_chart_th > median(data$weeks_on_chart_th),
                                  weeks_on_chart_gb > median(data$weeks_on_chart_gb),
                                  weeks_on_chart_aus > median(data$weeks_on_chart_aus),
                                  weeks_on_chart_can > median(data$weeks_on_chart_can),
                                  weeks_on_chart_gbr > median(data$weeks_on_chart_gbr),
                                  weeks_on_chart_nzl > median(data$weeks_on_chart_nzl),
                                  weeks_on_chart_usa > median(data$weeks_on_chart_usa),
                                  weeks_on_chart_arg > median(data$weeks_on_chart_arg),
                                  weeks_on_chart_col > median(data$weeks_on_chart_col),
                                  weeks_on_chart_esp > median(data$weeks_on_chart_esp),
                                  weeks_on_chart_mex > median(data$weeks_on_chart_mex),
                                  weeks_on_chart_per > median(data$weeks_on_chart_per),
                                 )
  return(top50_filter)
}

non_hit <- function(data) {
  non_hit_filter <- data %>% filter(if_any(starts_with("peak"), ~ . >= 150),
                                    # if_any(starts_with("peak"), ~ . <= 50),
                                  # if_all(starts_with("weeks_on_chart"), ~ . > median(data))
                                    weeks_on_chart_th < median(data$weeks_on_chart_th),
                                    weeks_on_chart_gb < median(data$weeks_on_chart_gb),
                                    weeks_on_chart_aus < median(data$weeks_on_chart_aus),
                                    weeks_on_chart_can < median(data$weeks_on_chart_can),
                                    weeks_on_chart_gbr < median(data$weeks_on_chart_gbr),
                                    weeks_on_chart_nzl < median(data$weeks_on_chart_nzl),
                                    weeks_on_chart_usa < median(data$weeks_on_chart_usa),
                                    weeks_on_chart_arg < median(data$weeks_on_chart_arg),
                                    weeks_on_chart_col < median(data$weeks_on_chart_col),
                                    weeks_on_chart_esp < median(data$weeks_on_chart_esp),
                                    weeks_on_chart_mex < median(data$weeks_on_chart_mex),
                                    weeks_on_chart_per < median(data$weeks_on_chart_per),
                                 
                                  )
  return(non_hit_filter)
}

song_in_common_each_year <- function(data, year, status) {
  
  if (status == 'top50'){
    func <- top50(data)
  } else if (status == 'non_hit') {
    func <- non_hit(data)
  }
  
  result <- func %>%
    mutate(year_on_chart = year) %>% 
    select(year_on_chart, track_name, artist_names, starts_with('peak'), starts_with('week'))
  
  return (result)
}

top50_in_common <- rbind(song_in_common_each_year(data2018, 2018, status='top50'),
                         song_in_common_each_year(data2019, 2019, status='top50'),
                         song_in_common_each_year(data2020, 2020, status='top50'),
                         song_in_common_each_year(data2021, 2021, status='top50'))
  

non_hit_in_common <- rbind(song_in_common_each_year(data2018, 2018, status='non_hit'),
                           song_in_common_each_year(data2019, 2019, status='non_hit'),
                           song_in_common_each_year(data2020, 2020, status='non_hit'),
                           song_in_common_each_year(data2021, 2021, status='non_hit'))

# print(top50_in_common$track_name[duplicated(top50_in_common$track_name)])
# # New rules - Dua lipa (2018 - 2019)
# # I don't care - Ed sheeran and Justin bieber (2019 - 2020)
# # Senorita - Shawn Mendes (2019-2020)
# # Don't start now - Dua lipa (2020 - 2021)


top50_in_common_no_duplicatedsongs <- top50_in_common %>%
  distinct(track_name, .keep_all = TRUE)
non_hit_in_common_no_duplicatedsongs <- non_hit_in_common %>%
  distinct(track_name, .keep_all = TRUE) %>%
  subset(track_name != "All I Want for Christmas Is You")

top50_in_common_no_duplicatedsongs$status <- rep(c('hit'), each = nrow(top50_in_common_no_duplicatedsongs))
non_hit_in_common_no_duplicatedsongs$status <- rep(c('non_hit'), each = nrow(non_hit_in_common_no_duplicatedsongs))

divide_df <- function(peak_or_week) {
  divide <- rbind(top50_in_common_no_duplicatedsongs, non_hit_in_common_no_duplicatedsongs) %>%
    select(track_name, artist_names, year_on_chart, status, starts_with(peak_or_week))
  return (divide)
}

df_final_with_peak <- divide_df("peak") %>%
  melt(id = c("track_name", "artist_names", "year_on_chart", "status"),
       variable_name = "country")
  colnames(df_final_with_peak)[ncol(df_final_with_peak)] <- "peak"
  df_final_with_peak$country <- as.character(df_final_with_peak$country)
  df_final_with_peak_country <- df_final_with_peak$country[!duplicated(df_final_with_peak$country)]
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[1]] <- 'th'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[2]] <- 'gb'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[3]] <- 'aus'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[4]] <- 'can'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[5]] <- 'gbr'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[6]] <- 'nzl'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[7]] <- 'usa'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[8]] <- 'arg'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[9]] <- 'col'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[10]] <- 'esp'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[11]] <- 'mex'
  df_final_with_peak[df_final_with_peak == df_final_with_peak_country[12]] <- 'per'

df_final_with_week <- divide_df("week") %>%
  melt(id=c("track_name", "artist_names", "year_on_chart", "status"),
       variable_name = "country")
  colnames(df_final_with_week)[ncol(df_final_with_week)] <- "week"
  df_final_with_week$country <- as.character(df_final_with_week$country)
  df_final_with_week_country <- df_final_with_week$country[!duplicated(df_final_with_week$country)]
  df_final_with_week[df_final_with_week == df_final_with_week_country[1]] <- 'th' 
  df_final_with_week[df_final_with_week == df_final_with_week_country[2]] <- 'gb'
  df_final_with_week[df_final_with_week == df_final_with_week_country[3]] <- 'aus'
  df_final_with_week[df_final_with_week == df_final_with_week_country[4]] <- 'can'
  df_final_with_week[df_final_with_week == df_final_with_week_country[5]] <- 'gbr'
  df_final_with_week[df_final_with_week == df_final_with_week_country[6]] <- 'nzl'
  df_final_with_week[df_final_with_week == df_final_with_week_country[7]] <- 'usa'
  df_final_with_week[df_final_with_week == df_final_with_week_country[8]] <- 'arg'
  df_final_with_week[df_final_with_week == df_final_with_week_country[9]] <- 'col'
  df_final_with_week[df_final_with_week == df_final_with_week_country[10]] <- 'esp'
  df_final_with_week[df_final_with_week == df_final_with_week_country[11]] <- 'mex'
  df_final_with_week[df_final_with_week == df_final_with_week_country[12]] <- 'per'

df_final_with_peak <- df_final_with_peak %>%
  mutate(
    official_language = case_when(
      country == "th" ~ "thai",
      country == "gb" ~ "undefinded",
      country == "aus" ~ "english",
      country == "can" ~ "english",
      country == "gbr" ~ "english",
      country == "nzl" ~ "english",
      country == "usa" ~ "english",
      country == "arg" ~ "spanish",
      country == "col" ~ "spanish",
      country == "esp" ~ "spanish",
      country == "mex" ~ "spanish",
      country == "per" ~ "spanish",
    ), .before = "peak"
  )

df_final_with_week <- df_final_with_week %>%
  mutate(
    official_language = case_when(
      country == "th" ~ "thai",
      country == "gb" ~ "undefinded",
      country == "aus" ~ "english",
      country == "can" ~ "english",
      country == "gbr" ~ "english",
      country == "nzl" ~ "english",
      country == "usa" ~ "english",
      country == "arg" ~ "spanish",
      country == "col" ~ "spanish",
      country == "esp" ~ "spanish",
      country == "mex" ~ "spanish",
      country == "per" ~ "spanish",
    ), .before = "week"
  )

df_final_all = merge(x = df_final_with_peak, y = df_final_with_week, all.x=TRUE, sort = FALSE)
colnames(df_final_all)[1] <- "track"
colnames(df_final_all)[2] <- "artist"
write.csv(df_final_all, "Z:\\musec_code\\MUSEC_SpotifybyOrn\\musec_stimuli_info.csv", row.names = FALSE)

df_final_all %>%
  filter(status == "hit") %>%
  ggplot(aes(peak, track,
             colour = official_language))+
  geom_point(aes(size = week), alpha = 0.3)+
  geom_smooth()+ # create smooth of line
  # facet_wrap(~status)+
  labs(title = "22 popular songs reaching to Top50 across 11 country charts and 1 global chart ")
  ggsave(file="./MUSEC_SpotifybyOrn/Visualization/plot_popularsongs_across12charts", width=4, height=4, dpi=300)


# 
# df_final_with_peak[df_final_with_peak == 'peak_rank_th'] <- 'th'
# 
# 
# df_final_with_peak$country <- as.character(df_final_with_peak$country)  
# 
# df_final_with_peak[df_final_with_peak == 'peak_rank_th'] <- 'th'


  # theme_bw()

# df_final_with_week %>%
#   ggplot(aes(track_name, week,
#              colour = country))+
#   geom_point(size = 3, alpha = 0.5)+
#   geom_smooth()+ # create smooth of line
#   facet_wrap(~status)+
#   labs(title = "Peak")


  
# df_final_with_peak$country <- 


# df_final <- melt(df_final, id= c("track_name", "artist_names", "year_on_chart", "status"))
                # ,measure.vars = c("Group1", "Group2"))

# 
# ggplot(melt(df_final, id.vars = "track_name"), aes(track_name, value, color=variable, stat='identity')) +
#   geom_line(size=1.4) + labs(color="")



