

# Lib
library(tidyverse)
library(lubridate)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
df <- read_csv("https://data.humdata.org/dataset/4ce16b45-5526-471d-8918-2ed76082f1c8/resource/4cfc43b6-cc9c-403b-966f-fc37b8dd7d4b/download/odp_noticas.csv") %>% 
  clean_names() %>% 
  mutate(year_raw=as.numeric(str_sub(incident_date,-2,-1))) %>% 
  mutate(
    year = case_when(
      year_raw>=48 ~ as.numeric(paste0(19,year_raw,sep="")),
      between(year_raw,0,9) ~ as.numeric(paste0(200,year_raw,sep="")),
      TRUE ~ as.numeric(paste0(20,year_raw,sep=""))
    )
  )

# Misc
font <- "Inconsolata"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#F9F6EE"
txt_col <- "grey10"
showtext_auto(enable = TRUE) 

# Plot
df %>% 
  group_by(year, casualty_personnel_type) %>% 
  count() %>% 
  ggplot(aes(x=year,y=n)) +
  geom_bar(aes(fill=casualty_personnel_type),
           stat = "identity",
           position = "stack",
           color=bg) +
  scale_fill_manual(values = met.brewer("Archambault", type = "discrete"),
                    name = "Type of personnel") +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = str_to_upper("Risky peacekeeping"),
       subtitle= "Fatalities in UN peacekeeping operations",
       caption = "Gilbert Fontana | Data: UN"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted"),
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=10),
    axis.line.x = element_line(),
    plot.title = element_text(hjust=.5,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=.5,size=14, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_text(face="bold")
  ) +
  guides(fill = guide_legend(title.position= "top",
                             title.hjust = .5,
                             nrow=2,byrow=TRUE))


# Save
showtext_opts(dpi = 320) 

ggsave("UN.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  




         