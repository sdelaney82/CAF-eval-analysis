#app.Q19  Please complete the following statement. "I am confident in our ability to adapt to a changing climate" ####
TappQ20.13 <- COMBOdata %>%
  group_by(app.Q20.13) %>%
  summarize(n = n(), Statement = "2020 Application \n(our ability to adapt)")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = app.Q20.13)

# In 2021 workshop evaluation data, Q31.1  "I am confident in northeast agriculture's ability to adapt to a changing climate"  ####
TW1_Q31.1 <- COMBOdata %>%
  group_by(wkshp1.Q31.1) %>%
  summarize(n = n(), Statement = "2021 post-workshop")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = wkshp1.Q31.1) 

# In 2022 workshop evaluation data, Q2.1.1  I am confident in northeast agriculture's ability to adapt to a changing climate ####
TW2_21.1 <- COMBOdata %>%
  group_by(wkshp2.Q2.1.1) %>%
  summarize(n = n(), Statement = "2022 post-workshop")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = wkshp2.Q2.1.1) 

#add in 2023 survey!
TS1_Q2.1_1 <- CAF2023data %>%
  group_by(Q2.1_1) %>%
  summarize(n = n(), Statement = "2023 1-yr post-CAF")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = Q2.1_1) 

#Join dataframes #### 
NEagadapt_combo <- rbind(TappQ20.13,
                            TW1_Q31.1,
                            TW2_21.1, TS1_Q2.1_1)

NEagadapt_combo$Statement <- factor(NEagadapt_combo$Statement, levels=c("2023 1-yr post-CAF", "2022 post-workshop", "2021 post-workshop","2020 Application \n(our ability to adapt)"))

as.tibble(NEagadapt_combo)

# STEP 3: Create stacked bar chart ####
NEagadapt_plot <- ggplot(NEagadapt_combo, aes(             
  x = Statement,                             # ID data frame column for X axis
  y = perc,  # ID data frame column for Y axis
  fill = factor(                             
    Response,                                # ID data frame column for fill (bars)
    levels = c("Strongly agree",                 # set string values in manual order
               "Somewhat agree",
               "Neutral",
               "Somewhat disagree",
               "Strongly disagree")
  )
)) +
  geom_bar(stat = "identity", width = 0.5) + # manually set bar width
  # stat="identity" to geom_bar() then you're telling R to calculate the sum of the y variable, grouped by the x variable and use bars to display the sums
  geom_text(
    aes(label = paste0(perc, "%")),          # add % labels #paste0 is for concatenating character vectors
    # add %
    position = "stack",                      # position % labels and use adjust to tweak placement
    #align labels with bars
    vjust = .5,
    hjust = .75,
    size = 4,                                # label text size
    color = 'black'                          # label text color
  ) +
  scale_fill_brewer(palette = "BrBG", direction = -1) +      # set bar color scheme
  #facet_grid( ~ Statement) +                 # ID data from column if more than one facet
  coord_flip() +
  labs(
    y = "percent of respondents",
    fill = "",
    x = "",
    title = "CAF Fellow agreement with statement: 'I am confident in northeast agriculture's ability to adapt to a changing climate'"
  ) +
  guides(fill = guide_legend(reverse = TRUE))+
  #theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "bottom") 

NEagadapt_plot
