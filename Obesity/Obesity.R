

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(ggdist)

# Data
df <-  read.csv("https://www.ncdrisc.org/downloads/bmi/NCD_RisC_Lancet_2017_BMI_age_standardised_world.csv") %>% 
  clean_names()


# Misc
font <- "Raleway"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#FBF3EA"
txt_col <- "black"
showtext_auto(enable = TRUE) 

# Plot
df%>% 
  ggplot(aes(x = year,
             y = prevalence_of_bmi_30_kg_m_u_fffd_obesity*100,
             ymin = prevalence_of_bmi_30_kg_m_u_fffd_lower_95_uncertainty_interval*100,
             ymax = prevalence_of_bmi_30_kg_m_u_fffd_upper_95_uncertainty_interval*100)) +
  geom_pointinterval(aes(color=sex)) +
  scale_y_continuous(limits = c(0,20), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1975,2015,5)) +
  scale_color_manual(values = rev(met.brewer("Johnson", type = "discrete", n=2))) +
  labs(
    title = "World obesity is steadily increasing",
    caption = "Gilbert Fontana | Data: NCD Risk Factor Collaboration",
    y="Obesity prevalence (Body Mass Index \u2265 30), percent of world population"
    
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.title.y  = element_text(color=txt_col, size=9, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=.5,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = c(.1,.9),
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(title.position= "top",
                              title.hjust = .5,
                              ncol=1,
                              byrow=TRUE,
                              reverse = TRUE))

# Save
showtext_opts(dpi = 320) 

ggsave("Obesity.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  


