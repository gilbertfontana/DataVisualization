

#Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)


# Data
df_2015 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2015.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2016 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2016.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2017 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2017.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2018 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2018.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2019 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2019.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2020 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2020.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2021 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2021.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)

df_2022 <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Laser/Laser_Report_2022.xlsx") %>% 
  clean_names() %>% 
  select(incident_date,state)


df <- bind_rows(
  df_2015,
  df_2016,
  df_2017,
  df_2018,
  df_2019,
  df_2020,
  df_2021,
  df_2022
)

df2 <- df %>% 
  filter(state %in% state.name) %>% 
  group_by(incident_date, state) %>% 
  count() %>%
  ungroup() %>% 
  group_by(state) %>% 
  mutate(cumsum=cumsum(n))


top4 <- df2 %>% 
  group_by(state) %>%
  arrange(cumsum) %>% 
  slice_max(cumsum) %>% 
  arrange(desc(cumsum)) %>% 
  ungroup() %>% 
  slice_head(n=4) %>% 
  pull(state)


# Misc
font <- "Arya"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "grey20"
showtext_auto(enable = TRUE)

# Plot
ggplot() +
  geom_line(data=df2 %>% filter(state %in% top4), aes(x=incident_date, y=cumsum, color=state)) +
  geom_line(data=df2 %>% filter(!state %in% top4), aes(x=incident_date, y=cumsum, group=state), color="grey80") +
  geom_point(data=df2 %>% 
               group_by(state) %>%
               arrange(cumsum) %>% 
               slice_max(cumsum) %>% 
               arrange(desc(cumsum)) %>% 
               ungroup() %>% 
               slice_head(n=4),
             aes(x = incident_date, y = cumsum, color=state)) +
  geom_text(data=df2 %>% 
              group_by(state) %>%
              arrange(cumsum) %>% 
              slice_max(cumsum) %>% 
              arrange(desc(cumsum)) %>% 
              ungroup() %>% 
              slice_head(n=4),
            aes(x = incident_date + 2000000, y = cumsum, label=state, color=state),
            hjust = 0,
            vjust = 0.5,
            size=3,
            family=font,
            fontface="bold") +
  scale_x_datetime(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_color_manual(values = met.brewer("Renoir")) +

  coord_cartesian(clip="off") +
  labs(
    title = "Laser Strikes on Aircrafts",
    subtitle = "Pointing a laser at an aircraft remain a serious threat to aviation safety\nand is a federal crime. Over the last years, laser strike incidents has\nincreased in many states.",
    caption = "Gilbert Fontana | Data: Federal Aviation Administration",
    y="Cumulative number of incidents"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.title.y  = element_text(color=txt_col, size=9, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=0,size=26, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,20,0)),
    plot.subtitle = element_text(hjust=0,size=14, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.title.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(20,60,20,20),
    legend.position = "none"
  )

showtext_opts(dpi = 320) 

ggsave("Laser.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  


  




