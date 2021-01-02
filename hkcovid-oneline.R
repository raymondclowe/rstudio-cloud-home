
# in one line nov 1 until  today
mycolors <- c("Cyan2")
Clusters <- c("Dance Studio")

as_tibble(read.csv('http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv')) %>% 
  mutate(Report.Date.Date = parse_date(Report.date, "%d/%m/%Y")) %>% 
  group_by(Report.Date.Date) %>%
  add_tally() %>%
  ggplot(.) + 
  aes(y = n, x = Report.Date.Date) + 
  geom_point() + 
  geom_smooth(fullrange = TRUE) + 
  scale_y_continuous(limit=c(1,NA),oob=squish) + 
  scale_x_date(limits = c(as.Date("2020-11-01"), Sys.Date())) +
  geom_text(aes(label=ifelse(Report.Date.Date == as.Date("2020-11-20"),"Dance Studio Cluster",""), col=Clusters, hjust=0,vjust=-5)) +
  scale_color_manual(values=mycolors)
