library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(ggalt)
library(ggiraph)
library(reactable)
library(colorspace)
library(hrbrthemes)

csvPath = 'SavantPitching2022.csv'

SavantPitching2022 = read.csv(csvPath) 

SavantPitching2022 = SavantPitching2022 %>% arrange(pitch_team, player_name)

# https://loading.io/color/feature/Spectral-10/
pitchColors = c('4-Seam Fastball'='#d22d49',
                "Fastball"="#e857d9",
                "Cutter"="#933f2c",
                "Sinker"="#f46d43",
                "Curveball"="#00d1ed",
                "Knuckle Curve"='#6236cd',
                "Slider"="#c5bf00",
                "Changeup"="#1dbe3a",
                "Splitter"="#3bacac",
                'Other'='#000000'
)

wOBA <- function(Singles, Doubles, Triples, HomeRuns, Walks, HBP, PA) {
  wOBA <- ((.72*HBP) + (.689*Walks) + (.884*Singles) + (1.261*Doubles) + (1.601*Triples) + (2.072*HomeRuns))/(PA)
  return(wOBA)
}

style = "position:relative;width:100%;height:0;padding-bottom:75%;"

# Strike Zone for plots
strikeZone <- data.frame(
  x1 = rep(c(-.75, -.25, .25), each = 3),
  x2 = rep(c(-.25, .25, .75), each = 3),
  y1 = rep(c(1.6, 2.23, 2.86), 3),
  y2 = rep(c(2.23, 2.86, 3.5), 3),
  Zone = factor(c(7, 4, 1, 8, 5, 2, 9, 6, 3))
)

# Home Plate
homePlate = data_frame(x = c(-.75, .75, .75, 0, -.75, -.75),
                       y = c(.5, .5, .35, .15, .35, .5))

ggTheme = theme(axis.title=element_text(size=18), axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = +3),
                axis.text.x=element_text(size=12), axis.text.y=element_text(size=12),
                legend.text=element_text(size=12), legend.title=element_text(size=14),
                plot.margin = margin(10, 10, 15, 10)) # margin(top, right, bottom, left)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  title = 'LionDash',
  skin = 'blue',
  dashboardHeader(title = "2021 Statcast Data",
                  titleWidth = 200),
  dashboardSidebar(
    disable = T
  ),
  
  dashboardBody(
    fluidRow(
      tabBox(title = 'User Input', width = 4,
             tabPanel('Inputs',
                      pickerInput(label = 'Team',
                                  inputId = 'team',
                                  choices = unique(SavantPitching2022$pitch_team),
                                  options = list(size = 10, 'live-search' = TRUE)),
                      pickerInput(label = 'Player',
                                  inputId = 'player',
                                  choices = unique(SavantPitching2022$player_name),
                                  options = list(size = 10, 'live-search' = TRUE)))
      ),
      
      box(title = 'Break Map', width = 4,girafeOutput("breakMap"), style=style),
      box(title = 'Veolcity', width = 4,girafeOutput('veloBox'), style=style)),
    fluidRow(box(title = 'Usage Bar Chart', width = 4,girafeOutput('usageBar'), style=style),
             box(title = 'Release Point', width = 4, girafeOutput('release'), style=style),
             box(title = 'Heatmap', width = 4, girafeOutput('pitchHeatmap'), style=style)),
    fluidRow(),
    fluidRow(box(title = 'Spray Chart', width = 12,reactableOutput('table')))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$team, {
    pitcherIds = SavantPitching2022 %>%
      filter(pitch_team == input$team)
    
    updatePickerInput(session,
                      inputId = 'player',
                      choices = unique(pitcherIds$player_name))
  })
  
  pitcherEvents = reactive({
    h = SavantPitching2022 %>%
      filter(player_name == input$player)
  })
  
  pitchGrouping = reactive({
    pitcherEvents()  %>%
      group_by(pitch_name) %>%
      summarise(BA = round(sum(events %in% c('single', 'double', 'triple', 'home_run')) / 
                             sum(events %in% c('strikeout'), events %in% c('single', 'double', 'triple', 'home_run', 'field_out')), 3),
                EV = round(mean(launch_speed, na.rm = T), 1),
                LA = round(mean(launch_angle, na.rm = T), 1),
                'Whiff%' = round(sum(description == 'swinging_strike') / sum(description %in% c('called_strike', 'swinging_strike', 'hit_into_play', 'foul', 'foul_tip')) * 100, 1),
                'PutAway%' = round(sum(events == 'strikeout', na.rm = T) / sum(strikes == 2, na.rm = T) * 100,1),
                wOBA = round(wOBA(sum(events == 'single', na.rm = T),
                                  sum(events == 'double', na.rm = T),
                                  sum(events == 'triple', na.rm = T),
                                  sum(events == 'home_run', na.rm = T),
                                  sum(events == 'walk', na.rm = T),
                                  sum(description == 'hit_by_pitch', na.rm = T),
                                  sum(events %in% c('walk', 'skrikeout', 'catcher_interf', 'field_error',"sac_bunt","sac_fly_double_play","sac_bunt_double_play","sac_fly"),
                                      description %in% c('hit_by_pitch'),
                                      events %in% c('single', 'double', 'triple', 'home_run', 'field_out'), na.rm = T)),
                             3)
      )
  })

  output$breakMap = renderGirafe({
    g = ggplot(pitcherEvents(), aes(x = pfx_x * 12, y = pfx_z * 12, color = pitch_name)) +
      geom_point(size = 1) +
      scale_x_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
      scale_y_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
      geom_encircle(aes(color=pitch_name, fill=pitch_name), alpha = 0.2, expand = 0.02, show.legend = FALSE, na.rm = TRUE) +
      labs(x = "Horizontal Break (in.)", y = "Vertical Break (in.)", color = "Pitch Type") +
      scale_color_manual(values = pitchColors, limits = force) +
      scale_fill_manual(values = pitchColors, limits = force) +
      ggTheme
    
    girafe(ggobj = g, width_svg = 7)
  })
  
  output$veloBox = renderGirafe({
    g = ggplot(pitcherEvents(), aes(x = release_speed, y = pitch_name, fill = pitch_name)) +
      geom_boxplot() +
      scale_fill_manual(values = pitchColors, limits = force) +
      labs(x = "Release Velocity (MPH)", y = "Pitch Type", fill = "Pitch Type") +
      ggTheme
      
    girafe(ggobj = g, width_svg = 7)
  })
  
  output$usageBar = renderGirafe({
    pitchPer = pitcherEvents() %>% 
      group_by(pitch_name) %>% 
      summarise(Per = round(n() / nrow(pitcherEvents()) * 100,1))
    
    g = ggplot(pitchPer, aes(x = reorder(pitch_name, -Per), y = Per, fill = pitch_name)) +
      geom_col() +
      scale_fill_manual(values = pitchColors, limits = force) +
      labs(x = "Pitch Type", y = "Pitch Usage (%)", fill = "Pitch Type") +
      ggTheme
    girafe(ggobj = g, width_svg = 7)
  })
  
  output$release = renderGirafe({
    g = ggplot(pitcherEvents(), aes(x=release_pos_x,y=release_pos_z,color=pitch_name)) +
      geom_point() +
      coord_equal() +
      scale_size(range = c(0.1,3)) +
      coord_cartesian(xlim = c(-5,5), ylim = c(0,7.5)) +
      scale_colour_manual(values = pitchColors, limits = force) +
      labs(x = "Feet From Home Plate", y = "Feet Above the Ground", color = "Pitch Type") +
      ggTheme
    
    girafe(ggobj = g, width_svg = 7)
  })
  
  output$pitchHeatmap = renderGirafe({
    g = ggplot(pitcherEvents(),aes(x=plate_x, y=plate_z)) +
      xlim(-2.5, 2.5)+
      ylim(0,5) +
      stat_density_2d(geom = "polygon", aes(fill = ..level..)) +
      scale_fill_continuous_divergingx(palette = 'RdBu', rev = TRUE, mid = 0.1, l3 = 35) +
      geom_rect(data = strikeZone, inherit.aes = F, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill=NA, color = 'black') +
      geom_path(data = homePlate, aes(x = x, y = y)) +
      coord_fixed() +
      labs(x = "Plate Location Side", y = "Plate Location Height", fill = "Frequency") + 
      facet_wrap(~ pitch_name,) +
      ggTheme
    
    girafe(ggobj = g, width_svg = 7)
  })
  
  output$table = renderReactable({
    reactable(pitchGrouping())
  })
}

options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
