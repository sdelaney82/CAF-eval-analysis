####Look at the questions related to change in mindset: (3.7, 4.6, 4.10 )
#3.7.1, Hearing other CAF farmer participants discuss climate risks faced on their farms made me think about climate risks that I face in a new way.	
#3.7.2, Hearing other CAF agricultural advisor participants discuss climate risks helped me think about how to address these climate risks in my programs in a new way.	
#3.7.3, Hearing other CAF farmer participants discuss climate adaptations used on their farms made me think about climate adaptations that I could use in a new way.	
#3.7.4, Hearing CAF participants discuss climate adaptations helped me think about how to address these climate adaptations in my programs in a new way.
#4.6, Since beginning the CAF program, my confidence in addressing climate change on my farm has increased.
#4.10, Since beginning the CAF program, my confidence in incorporating climate change issues into my programing has increased.

#Make a df with just these columns, and fellow names
colnames(COMBOdata)

Mindset <- COMBOdata[,c(
  "NAME", "wkshp2.Q3.7.1", "wkshp2.Q3.7.2", "wkshp2.Q3.7.3", "wkshp2.Q3.7.4", "wkshp2.Q4.6", "wkshp2.Q4.10")]

tibble(Mindset)

####convert variables to factors and rename columns
Mindset <- 
  Mindset %>% 
  select(-NAME) %>% #take off name column temporarily, want it to stay a character
  mutate_if(is.character, as.factor)  %>%
  rename ("RisksNW.F" ="wkshp2.Q3.7.1") %>% #rename columns to make them readable
  rename("RisksNW.A" ="wkshp2.Q3.7.2") %>%
  rename("AdaptNW.F" ="wkshp2.Q3.7.3") %>%
  rename("AdaptNW.A" ="wkshp2.Q3.7.4") %>%
  rename("Conf.F" ="wkshp2.Q4.6") %>%
  rename("Conf.A" ="wkshp2.Q4.10") %>%
  cbind(NAME = Mindset[,1], .) 

####create databases for farmers and advisors by taking out the NAs from columns
Mindset_f <- filter(Mindset, !is.na(RisksNW.F))
Mindset_aa <- filter(Mindset, !is.na(RisksNW.A))

####set the levels for the variables so they appear correctly in the plots. 
AgreeRisk.F <- factor(Mindset_f$RisksNW.F, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
AgreeAdapt.F <- factor(Mindset_f$AdaptNW.F, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
AgreeConf.F <- factor(Mindset_f$Conf.F, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
AgreeRisk.A <- factor(Mindset_aa$RisksNW.A, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
AgreeAdapt.A <- factor(Mindset_aa$AdaptNW.A, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
AgreeConf.A <- factor(Mindset_aa$Conf.A, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))

####create plots for farmers' responses to these 3 questions about change in mindset

ggplot(data=Mindset_f, mapping = aes(x = AgreeRisk.F, fill=AgreeRisk.F)) +
  geom_bar() +
  labs(x = " ",
       y = "Farmers") +
  scale_x_discrete(drop = FALSE) + #show levels with empty data to make charts consistent
  ylim(0, 11) + #make y-axis the same for all charts to show differences
  scale_fill_brewer(direction = -1) + #add color, fill specified in aes
  theme(legend.position = "none") + #take out legend, unnecessary
  ggtitle("Hearing other CAF farmer participants discuss climate risks faced on their farms\nmade me think about climate risks that I face in a new way")

ggplot(data=Mindset_f, mapping = aes(x = AgreeAdapt.F, fill=AgreeAdapt.F)) +
  geom_bar() +
  labs(x = " ",
       y = "Farmers") +
  scale_x_discrete(drop = FALSE) +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1) + 
  theme(legend.position = "none") + 
  ggtitle("Hearing other CAF farmer participants discuss climate adaptations used on their farms \nmade me think about climate adaptations that I could use in a new way")

ggplot(data=Mindset_f, mapping = aes(x = AgreeConf.F, fill=AgreeConf.F)) +
  geom_bar() +
  labs(x = " ",
       y = "Farmers") +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1) + 
  theme(legend.position = "none") + 
  scale_x_discrete(drop = FALSE, guide = guide_axis(n.dodge=3)) + #avoid overlapping x-labels due to more missing data
  ggtitle("Since beginning the CAF program, my confidence in addressing climate change \non my farm has increased")

####create plots for advisors' responses to these 3 questions about change in mindset

ggplot(data=Mindset_aa, mapping = aes(x = AgreeRisk.A, fill=AgreeRisk.A)) +
  geom_bar() +
  labs(x = " ",
       y = "Advisors") +
  scale_x_discrete(drop = FALSE) + #show levels with empty data to make charts consistent
  ylim(0, 11) + #make y-axis the same for all charts to show differences
  scale_fill_brewer(direction = -1) + #add color, fill specified in aes
  theme(legend.position = "none") + #take out legend, unnecessary
  ggtitle("Hearing other CAF agricultural advisor participants discuss climate risks helped \nme think about how to address these climate risks in my programs in a new way")

ggplot(data=Mindset_aa, mapping = aes(x = AgreeAdapt.A, fill=AgreeAdapt.A)) +
  geom_bar() +
  labs(x = " ",
       y = "Advisors") +
  scale_x_discrete(drop = FALSE) +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1) + 
  theme(legend.position = "none") + 
  ggtitle("Hearing CAF participants discuss climate adaptations helped me think about \nhow to address these climate adaptations in my programs in a new way")

ggplot(data=Mindset_aa, mapping = aes(x = AgreeConf.A, fill=AgreeConf.A)) +
  geom_bar() +
  labs(x = " ",
       y = "Advisors") +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1) + 
  theme(legend.position = "none") + 
  scale_x_discrete(drop = FALSE, guide = guide_axis(n.dodge=3)) + #avoid overlapping x-labels due to more missing data
  ggtitle("Since beginning the CAF program, my confidence in incorporating \nclimate change issues into my programing has increased")
