

# Libs
library(tidyverse)
library(janitor)
library(lubridate)
library(showtext)

# Data + data cleaning
df <- read.csv("https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/Mask/MTA_Subway_and_Bus_Mask_Compliance_Statistics__Beginning_2020.csv") %>% 
clean_names() %>% 
  select(-survey_period_start_date, - no_of_observations) %>% 
  rename(date=survey_period_end_date) %>% 
  mutate(date=mdy(date)) %>% 
  na.omit()


bus <- df %>% 
  filter(mode=="Bus") %>% 
  arrange(date) %>% 
  mutate(survey_index=seq(1:n())) %>% 
  pivot_longer(!date & !mode & !survey_index,
               names_to = "mask",
               values_to = "share")

subway <- df %>% 
  filter(mode=="Subway") %>% 
  arrange(date) %>% 
  mutate(survey_index=seq(1:n()))  %>% 
  pivot_longer(!date & !mode & !survey_index,
               names_to = "mask",
               values_to = "share")

df2 <- full_join(
  bus,
  subway,
  by=c("survey_index", "mask")
) %>% 
  clean_names() %>%
  mutate(daydiff=date_x-date_y,
         date_new=date_y+(daydiff/2)
         ) %>% 
  select(-date_y, -date_x,-daydiff) %>% 
  na.omit() %>% 
  pivot_longer(cols = c(mode_x, mode_y)) %>% 
  select(-name) %>% 
  rename(mode=value) %>% 
  pivot_longer(cols = c(share_x, share_y)) %>% 
  filter((!mode=="Bus" | !name=="share_y") & (!mode=="Subway" | !name=="share_x")) %>% 
  select(-name, -survey_index) %>% 
  mutate(paired = rep(1:(n()/2),each=2),
         date_new=factor(date_new)) %>% 
  arrange(date_new, mask)


size <- df2 %>% 
  filter(mask=="mask_worn_incorrectly") %>% 
  select(size=value, date_new, mode)


# Misc
font <- "Oswald"
font_add_google(family=font, font)
bg <- "#57837B"
txt_col <- "grey95"
bus <- "#FFB319"
subway <- "#911F27"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 


# Plot
df2 %>% 
  filter(mask=="total_wearing_a_mask") %>% 
  left_join(
    size
  ) %>% 
  mutate(date_new=as_date(date_new)) %>% 
  ggplot(aes(x=date_new, y=value*100)) +
  geom_line(aes(group = paired),size=.5, color=txt_col) +
  geom_point(aes(color=mode, size=size*100)) +
  scale_color_manual(values = c(bus,subway)) +
  scale_y_continuous(limits = c(80,100)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  coord_cartesian(clip="off") +
  annotate("text", x=as_date("2022-04-05"), y=88, label="Bus",
           color=bus,
           family=font,
           fontface="bold",
           size=3,
           hjust=0) +
  annotate("text", x=as_date("2022-04-05"), y=83, label="Subway",
           color=subway,
           family=font,
           fontface="bold",
           size=3,
           hjust=0) +
  xlab("Survey date") +
  ylab("mask usage rates (%)") +
  labs(
    title = "Mask usage in New Yorks transit system",
    subtitle = "- The recent weeks shows a declining trend",
    caption = "Gilbert Fontana | Data: Metropolitan Transportation Authority"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(hjust=1, color=txt_col, size=10, margin = margin(10,0,0,0)),
    axis.title.y = element_text(hjust=1, color=txt_col, size=10, margin = margin(0,10,0,0)),
    axis.text = element_text(color=txt_col, size=8),
    axis.line.x = element_line(color = txt_col),
    plot.title = element_text(hjust=0, size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0, size=18, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_text(color = txt_col, size=10),
    legend.text = element_text(color = txt_col, size=8),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = txt_col,)
  ) +
  guides(color = "none",
         size = guide_legend(title.position="left",
                             title.hjust = .5,
                             title = "Share of masks\nworn incorrect (%)",
                             override.aes=list(colour=txt_col))
         )

# Save
showtext_opts(dpi = 320) 

ggsave("Mask.png",
       height = 7,
       width = 9,
       dpi=320,
       
)  

showtext_auto(FALSE)


