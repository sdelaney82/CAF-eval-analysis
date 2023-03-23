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

####take out NAs
Mindset_f <- filter(Mindset, !is.na(RisksNW.F))
Mindset_aa <- filter(Mindset, !is.na(RisksNW.A))



####set the levels for the variables so they appear correctly in the plots. 
#Only writing the levels that were given as responses for each Q.  
#AgreeRisk <- factor(filteredMD$RisksNW.A, levels = c("Strongly agree", "Somewhat agree", "Strongly disagree", "NA"))
#AgreeAdapt <- factor(filteredMD$AdaptNW.A, levels = c("Strongly agree", "Somewhat agree", "Strongly disagree", "NA"))
#AgreeConf <- factor(filteredMD$Conf.A, levels = c("Strongly agree", "Somewhat agree", "Strongly disagree", "NA"))
#AgreeRisk.F <- factor(removeNA.MD.F$RisksNW.F, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
#AgreeAdapt.F <- factor(removeNA.MD.F$AdaptNW.F, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))
#AgreeConf.F <- factor(removeNA.MD.F.Conf$Conf.F, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))