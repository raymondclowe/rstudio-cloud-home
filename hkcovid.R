# http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv

hkcovid <- as_tibble(read.csv('http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv'))
ggplot(hkcovid) + aes(Gender, Age) + geom_boxplot() + facet_grid(. ~ Hospitalised.Discharged.Deceased)
hkdeceased <- hkcovid %>% filter(Hospitalised.Discharged.Deceased == "Deceased")
summary(hkdeceased)
hkcovid %>% filter(Hospitalised.Discharged.Deceased == "Discharged") %>% ggplot(.) + aes(Age, color = Gender) + geom_bar()
hkcovid <- mutate(hkcovid, Onset.Date.Date = parse_date(Date.of.onset, "%d/%m/%Y"))
hkcovid <- mutate(hkcovid, Onset.Date.Date = parse_date(Date.of.onset, "%d/%m/%Y"))
hkcovid %>% filter(Hospitalised.Discharged.Deceased == "Discharged") %>% ggplot(.) + aes(Onset.Date.Date, color = Gender) + geom_bar()
ggplot(hkcovid) + aes(Report.Date.Date, color = Gender) + geom_bar()
hkcovid <- mutate(hkcovid, lower.hk.resident = tolower(HK.Non.HK.resident))
ggplot(hkcovid) + aes(x = lower.hk.resident, group = lower.hk.resident, color = lower.hk.resident) + geom_boxplot()
hkcovidagemodel <- lm(hkcovid$Age ~ hkcovid$Case.no.)
plot(hkcovidagemodel)

hkcovid <- mutate(hkcovid, Report.Date.Date = parse_date(Report.date, "%d/%m/%Y"))
#hkcovid <- mutate(hkcovid, Onset.Date.Date = parse_date(Date.of.onset, "%d/%m/%y"))
hkcovid <- mutate(hkcovid, lower.hk.resident = tolower(HK.Non.HK.resident))
hkcovid$lower.hk.resident <- as.factor(hkcovid$lower.hk.resident)
hkcovid$Hospitalised.Discharged.Deceased <- as.factor(hkcovid$Hospitalised.Discharged.Deceased)

# actual
hkcovid %>% group_by(Report.Date.Date, Gender) %>% add_tally() %>% ggplot(.) + aes(y = n, x = Report.Date.Date, color = Gender) + geom_point()

library("scales")
# actual with trend
hkcovid %>% group_by(Report.Date.Date, Gender) %>% add_tally() %>% ggplot(.) + aes(y = n, x = Report.Date.Date) + geom_line() + geom_smooth() + scale_y_continuous(limit=c(10,NA),oob=squish)

# prediction
hkcovid %>% group_by(Report.Date.Date, Gender) %>% add_tally() %>% ggplot(.) + aes(y = n, x = Report.Date.Date) + geom_line() + geom_smooth(fullrange = TRUE) + scale_y_continuous(limit=c(10,NA),oob=squish) + scale_x_date(limits = c(as.Date("2020-01-02"), as.Date("2022-06-02")))


library('scales')
# in one line
as_tibble(read.csv('http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv')) %>% 
  mutate(Report.Date.Date = parse_date(Report.date, "%d/%m/%Y")) %>% 
  group_by(Report.Date.Date, Gender) %>%
  add_tally() %>%
  ggplot(.) + 
    aes(y = n, x = Report.Date.Date) + 
    geom_line() + 
    geom_smooth(fullrange = TRUE) + 
    scale_y_continuous(limit=c(10,NA),oob=squish) + 
    scale_x_date(limits = c(as.Date("2020-01-02"), as.Date("2020-12-31")))
up in