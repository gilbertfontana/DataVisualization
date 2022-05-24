

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(lubridate)


# Data
df <- read_delim("http://ksgleditsch.com/data/1March_Archigos_4.1.txt")

df2 <- df %>% 
  filter(exit=="Irregular") %>% 
  mutate(year=str_sub(enddate,1,4),
         exit_new = case_when(
           exitcode %in% c("Assassination by Unsupported Individual") ~ "Assassination",
           exitcode %in% c("Irregular, Other","Unknown","Removed in Military Power Struggle Short of Coup") ~ "Unknown/Other",
           exitcode %in% c("Popular Protest, with Foreign Support","Popular Protest, without Foreign Support") ~ "Protest",
           exitcode %in% c("Removed by Military, with Foreign Support","Removed by Military, without Foreign Support") ~ "Removed by Military",
           exitcode %in% c("Removed by Other Government Actors, with Foreign Support","Removed by Other Government Actors, without Foreign Support") ~ "Removed by Other Government",
           exitcode %in% c("Removed by Rebels, with Foreign Support","Removed by Rebels, without Foreign Support") ~ "Removed by Rebels"
         )
  ) %>% 
  select(year, idacr, leader, exit_new) %>% 
  mutate(year=as.numeric(year)) %>% 
  arrange(year, exit_new) %>% 
  group_by(year) %>% 
  mutate(counter = rev(row_number(year)))

# Misc
font <- "Saira Extra Condensed"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#D0C9C0"
txt_col <- "grey10"
showtext_auto(enable = TRUE) 

#Plot
df2 %>% 
  group_by(year, exit_new) %>% 
  count() %>% 
  ggplot(aes(x=year, y=n, fill=exit_new)) +
  geom_bar(position = "stack", stat="identity", color=bg) +
  geom_hline(yintercept = seq(0,15,1), color=bg) +
  scale_fill_manual(values = met.brewer("Hokusai1", type = "discrete"),
                    name = "Reason for removal") +
  scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
  labs(
    title = "Removal of political leaders",
    subtitle = "600 political leaders have been removed from office due to irregular reasons since 1875",
    caption = "Gilbert Fontana | Data: Goemans, H. E., Gleditsch, K. S., & Chiozza, G. (2009) - Archigos v 4.1"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=10),
    axis.text.y = element_blank(),
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

ggsave("Leaders.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  

