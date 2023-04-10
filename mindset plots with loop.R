#create a dataframe with only the columns that have (leveled) farmer data, so don't have to skip over columns when looping
Mindset_f.factors <- data.frame(Mindset_f$NAME, AgreeRisk.F, AgreeAdapt.F, AgreeConf.F)

#loop through the 3 columns in this df, creating a bar chart for each, same plot features for all
#cleaner code!
for(i in 2:ncol(Mindset_f.factors)) {
  print(ggplot(data=Mindset_f, mapping = aes(x = Mindset_f.factors[,i], fill= Mindset_f.factors[,i])) +
    geom_bar() +
    labs(x = " ",
         y = "Farmers") +
    scale_x_discrete(drop = FALSE) + #show levels with empty data to make charts consistent
    ylim(0, 11) + #make y-axis the same for all charts to show differences
    scale_fill_brewer(direction = -1, drop = FALSE) + #add color, fill specified in aes
    theme(legend.position = "none") + #take out legend, unnecessary
    ggtitle(paste(colnames(Mindset_f.factors[i])))) #change title to column name
       Sys.sleep(2)
}

#could do the same thing for advisor charts if desired
#The plot titles here are the column names, since that is easier with the loop. Full Q pasted below. Could change
#column names to be more descriptive if wanted to use publicly

#"Hearing other CAF farmer participants discuss climate risks faced on their farms made me think about climate risks that I face in a new way")
#"Hearing other CAF farmer participants discuss climate adaptations used on their farms  made me think about climate adaptations that I could use in a new way")
#"Since beginning the CAF program, my confidence in addressing climate change on my farm has increased")
