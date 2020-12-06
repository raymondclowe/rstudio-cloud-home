fdh <- as_tibble(read.csv('https://www.immd.gov.hk/opendata/eng/law-and-security/visas/statistics_FDH.csv'))
ggplot(fdh) + 
  geom_point(aes(y = Philippines, x = As.at.end.of.Year), color = 'red') +
  geom_smooth(aes(y = Philippines, x = As.at.end.of.Year), method = "lm")

ggplot(fdh) + 
  geom_point(aes(y = Philippines, x = As.at.end.of.Year), color = 'red') +
  geom_smooth(aes(y = Philippines, x = As.at.end.of.Year), method = "lm") +
  geom_point(aes(y = Indonesia, x = As.at.end.of.Year), color = 'green') +
  geom_smooth(aes(y = Indonesia, x = As.at.end.of.Year), method = "lm")


fdhg <- gather(fdh, key = 'country', 
               value = 'HowMany', Philippines, Indonesia, Others)

ggplot(fdhg) + 
  aes(x = As.at.end.of.Year, y = HowMany, color = country) +
  geom_point()