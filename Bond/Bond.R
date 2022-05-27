
# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(lubridate)


# Data
df <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Bond/JSTdatasetR5.xlsx",
                         sheet = "Data") %>% 
  select(year, country, bill_rate) %>% 
  na.omit()
  

# Misc
font <- "Raleway"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#F8EBE4"
txt_col <- "black"
showtext_auto(enable = TRUE) 

# Plot
df %>% 
  ggplot() +
  geom_jitter(aes(x=year, y=bill_rate*100, color=country), alpha=.8) +
  geom_smooth(aes(x=year, y=bill_rate*100), se = FALSE, color="#A7252E") +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_x_continuous(breaks = seq(1870,2020,50)) +
  scale_color_manual(values = met.brewer("Renoir", n=16)) +
  labs(
    title = "Interest rates over 150 years",
    caption = "Gilbert Fontana | Data: Jordà, Ò., Schularick, M., & Taylor, A. M. (2017)",
    y = "Government bond interest rate (%)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.title.y  = element_text(color=txt_col, size=9, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line.y = element_line(),
    plot.title = element_text(hjust=.5,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = c(.3,.85),
    legend.title = element_blank(),
    legend.spacing.x = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
  ) +
  guides(color = guide_legend(title.position= "top",
                              title.hjust = .5,
                              ncol=4,
                              byrow=TRUE,
                              reverse = TRUE))


# Save
showtext_opts(dpi = 320) 

ggsave("Bond.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  
