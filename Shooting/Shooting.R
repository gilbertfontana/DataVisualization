

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(lubridate)


# Data
INCIDENT <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Shooting/SSDB_Raw_Data.xlsx",
                         sheet="INCIDENT")
SHOOTER <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Shooting/SSDB_Raw_Data.xlsx",
                               sheet="SHOOTER")
VICTIM <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Shooting/SSDB_Raw_Data.xlsx",
                              sheet="VICTIM")
WEAPON <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Shooting/SSDB_Raw_Data.xlsx",
                               sheet="WEAPON")

df <- INCIDENT %>% 
  select(Incident_ID, Date, State) %>% 
  left_join(
    VICTIM,
    by=c("Incident_ID"="incidentid")
  ) 

top10 <- df %>% 
  group_by(State) %>% 
  count(sort=TRUE) %>% 
  ungroup() %>% 
  slice_head(n=10)

df2 <- df %>% 
  filter(State %in% top10$State) %>% 
  mutate(Date=year(as.Date(Date))) %>% 
  group_by(State, Date) %>% 
  count() %>%
  ungroup() %>% 
  arrange(State, Date) %>% 
  group_by(State) %>% 
  mutate(cumsum=cumsum(n)) %>% 
  ungroup()


# Misc
font <- "Homenaje"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#FFF2F2"
txt_col <- "grey20"
showtext_auto(enable = TRUE)

# Plot
df2 %>% 
  ggplot() +
  geom_line(aes(x=Date, y=cumsum, color=State)) +
  geom_point(data= df2 %>% filter(Date==2022),
               aes(x=Date, y=cumsum, color=State),
             size=1) +
  geom_text(data=df2 %>% filter(Date==2022),
            aes(x = max(Date) + .5, y = cumsum, label=State, color=State),
            hjust = 0,
            vjust = 0.5,
            size=2,
            family=font,
            fontface="bold") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_manual(values = met.brewer("Redon")) +
  coord_cartesian(clip="off") +
  labs(
    title="School Shooting Victims",
    subtitle = "The cumulative number of school shooting victims since 1970\nin the 10 states with the highest number of victims",
    caption = "Gilbert Fontana | Data: K-12 School Shooting database",
    y="Cumulative number of victims"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=9),
    axis.title.y  = element_text(color=txt_col, size=10, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=0,size=26, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,10,0)),
    plot.subtitle = element_text(hjust=0,size=16, color=txt_col, margin=margin(0,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=12, color=txt_col, face="bold"),
    plot.title.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  )


# Save
showtext_opts(dpi = 320) 

ggsave("Shooting.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  







