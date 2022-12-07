library(baseballr)
library(lubridate)
library(tidyverse)
library(anytime)
library(ggalt)

anydate(18898)
dates = seq(as.Date("2022-04-06"), as.Date('2022-11-05'), by = 1)

SavantPitching2022 = data.frame()

for(i in seq_along(dates)){
  dailyPitcherData = baseballr::scrape_statcast_savant(start_date = dates[i]+1, 
                                           end_date = dates[i + 1],
                                           player_type = "pitcher")
  
  i + 1
  SavantPitching2022 = rbind(SavantPitching2022, dailyPitcherData)
}

SavantPitching2022$pitch_team = with(SavantPitching2022, ifelse(SavantPitching2022$inning_topbot == 'Bot', SavantPitching2022$away_team, SavantPitching2022$home_team))

SavantPitching2022$pitch_name[SavantPitching2022$pitch_name == 'Split-Finger'] = 'Splitter'
SavantPitching2022$pitch_name[SavantPitching2022$pitch_name %in% c('Eephus', 'Knuckleball')] = 'Other'

pitchColors = c('4-Seam Fastball'='#9e0142',
                "Fastball"="#f46d43",
                "Cutter"="#d53e4f",
                "Sinker"="#fdae61",
                "Curveball"="#fee08b",
                "Knuckle Curve"='#abdda4',
                "Slider"="#e6f598",
                "Changeup"="#66c2a5",
                "Splitter"="#3288bd",
                'Other'='#5e4fa2'
)

write.csv(SavantPitching2022, 'C:/Users/sport/Desktop/Class Files/22-23 Fall/DS 330/Final/SavantPitching2022.csv')

SavantPitching2022 = read.csv('SavantPitching2022.csv')

g = ggplot(SavantPitching2022 %>% filter(player_name == 'Nola, Aaron'), aes(x = pfx_x * 12, y = pfx_z * 12, color = pitch_type)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
  scale_y_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
  geom_encircle(aes(color=pitch_type, fill=pitch_type), alpha = 0.2, expand = 0.02, show.legend = FALSE, na.rm = TRUE) +
  labs(x = "Horizontal Break (in.)", y = "Vertical Break (in.)", color = "Pitch Type")
g

h = ggplot(SavantPitching2022 %>% filter(player_name == 'Nola, Aaron'), aes(x = release_speed, y = pitch_type, fill = pitch_type)) +
  geom_boxplot()
h
