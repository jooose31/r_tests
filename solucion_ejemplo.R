library(Lahman)
library("dplyr")
library("readxl")
data("Teams")

View(Teams)

# 1. cuantos anios diferentes existen en el ds
# R. 151
n_distinct(Teams$yearID)
# 2. cuantos equipos existen en el ds?
# R. 149
n_distinct(Teams$teamID)
# 3. cuantas franquicias existen den el ds?
# R. 120
n_distinct(Teams$franchID)
# 4. cuantos registros no tienen division?
# R. 1517
Teams <- Teams %>% mutate(div_na = is.na(Teams$divID))
sum(Teams$div_na)

# 5. cual es el equipo que mas juegos disputo?
# R. CHN
# R. Chicago Cubs
resultado_temp <- Teams %>% group_by(teamID) %>% summarise(juegos = sum(G)) %>% arrange(desc(juegos)) %>%  slice(1:1)
resultado_temp2 <- Teams %>% filter(teamID == resultado_temp$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)

resultado_temp2$name
# 6. Cual es el equipo que menos partidos en casa disputo?
# R. Milwaukee Brewers MLA
resultado_temp3 <- Teams %>% group_by(teamID) %>% summarise(juegos = sum(Ghome)) %>% arrange(juegos) %>%  slice(1:1)

resultado_temp4 <- Teams %>% filter(teamID == resultado_temp3$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)

resultado_temp4$name

# 7. Cual es el equipo que mas victorias obtuvo?
# R. Chicago Cubs
resultado_temp <- Teams %>% group_by(teamID) %>% summarise(juegos = sum(W)) %>% arrange(desc(juegos)) %>%  slice(1:1)
resultado_temp2 <- Teams %>% filter(teamID == resultado_temp$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)

resultado_temp2$name

# 8. cuanto es el promedio de victorias para los equipos?
Teams %>% group_by(teamID) %>% summarise(juegos = mean(W)) %>% arrange(desc(juegos)) %>%  View()

# 9. Cual es el equipo que mas derrotas obtuvo?
resultado_temp <- Teams %>% group_by(teamID) %>% summarise(juegos = sum(W)) %>% arrange(desc(juegos)) %>%  slice(1:1)
resultado_temp2 <- Teams %>% filter(teamID == resultado_temp$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)

resultado_temp2$name
#10. Cual es el equipo que mas carreras obtuvo?
# R. Chicago Cubs
resultado_temp <- Teams %>% group_by(teamID) %>% summarise(carreras = sum(R)) %>% arrange(desc(carreras)) %>%  slice(1:1)
resultado_temp2 <- Teams %>% filter(teamID == resultado_temp$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)

resultado_temp2$name

# 11. Cual es el equipo que mas simples, dobles, triples y hr realizo?

# R. Simples: Chicago Cubs
resultado_simples <- Teams %>% group_by(teamID) %>% summarise(HITS = sum(H)) %>% arrange(desc(HITS)) %>%  slice(1:1)
resultado_simples2 <- Teams %>% filter(teamID == resultado_simples$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_simples2$name

# R.Dobles: Chicago Cubs 
resultado_dobles <- Teams %>% group_by(teamID) %>% summarise(dobles = sum(X2B)) %>% arrange(desc(dobles)) %>%  slice(1:1)
resultado_dobles2 <- Teams %>% filter(teamID == resultado_dobles$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_dobles2$name

# R.Triples: Pittsburgh Pirates 
resultado_triples <- Teams %>% group_by(teamID) %>% summarise(triples = sum(X3B)) %>% arrange(desc(triples)) %>%  slice(1:1)
resultado_triples2 <- Teams %>% filter(teamID == resultado_triples$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_triples2$name.

# R. Homeruns: New York Yankees
resultado_HR <- Teams %>% group_by(teamID) %>% summarise(homes = sum(HR)) %>% arrange(desc(homes)) %>%  slice(1:1)
resultado_HR2 <- Teams %>% filter(teamID == resultado_HR$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_HR2$name

# R. Equipo con mas simples, dobles, triples y homeruns: Chicago Cubs

resultado_total <- Teams %>% 
  group_by(teamID) %>% summarise(simples = sum(H), dobles = sum(X2B), dobles = sum(X3B), homes = sum(HR)) %>% 
  arrange(desc(simples), desc(dobles), desc(dobles), desc(homes)) %>% slice(1:1)
resultado_total <- Teams %>% filter(teamID == resultado_total$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_total$name

#12. Cual es el equipo que mas strikes tuvo?
# R. Chicago Cubs
resultado_strikes <- Teams %>% group_by(teamID) %>% summarise(strikes = sum(SO)) %>% arrange(desc(strikes)) %>%  slice(1:1)
resultado_strikes <- Teams %>% filter(teamID == resultado_strikes$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_strikes$name

#13. Cual es el equipo que mas bases robadas realizo?
# R. Chicago Cubs
resultado_bases <- Teams %>% group_by(teamID) %>% summarise(bases = sum(SO)) %>% arrange(desc(bases)) %>%  slice(1:1)
resultado_bases2 <- Teams %>% filter(teamID == resultado_bases$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_bases2$name

#14. cuanto es el promedio de errores por equipo?
resultado_promedio_error <- Teams %>% group_by(teamID) %>% summarise(prom_error = mean(E)) %>% arrange(desc(prom_error)) %>% View()


#15. cual es el peor equipo para intentar robar base?
# R.Baltimore Marylands
resultado_bases3 <- Teams %>% group_by(teamID) %>% summarise(bases = sum(SO)) %>% arrange((bases)) %>%  slice(1:1)
resultado_bases4 <- Teams %>% filter(teamID == resultado_bases3$teamID) %>% arrange(desc(yearID)) %>% slice(1:1)
resultado_bases4$name

