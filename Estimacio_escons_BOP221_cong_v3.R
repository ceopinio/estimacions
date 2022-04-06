# *Codi per generar l'estimació de vot al Congreso 
# *del Baròmetre 1-2022 del Centre d'Estudis d'Opinió
# 
# 
# /*Aquest codi parteix de l'estimació de vot generada pel codi EstimacioBOP22_1_v3.do (Stata)
# 
# El codi realitza les següents operacions:
# 1. Aplica correcció vot ocult de vot
# 2. Estima distribució de vot entre províncies a partir de factor multiplicador de nov 2019 per cada partit
# 3. Crea una matriu d'incertesa (moe)
# 4. Simula la distribució d'escons mitjançant la regla d'Hondt a les 4 províncies, 1000 simulacions (dist. normal)
# 5. Representa gràficament els resultats

library(ceobarometre) #paquet encara no disponible. Les funcions moe i simulate a l'arxiu votes2seats.R
library(readr)
library(tidyverse)

#input data
proportionsm4 <- read_delim("proportionsm4cong.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)

#correction for vox underreporting
correcciovox<-1.15
proportionsm4 <- proportionsm4 %>%
  mutate(
    proporcio= case_when(partit == "Vox" ~ proporcio*correcciovox, 
                         TRUE ~ proporcio),
    lb95= case_when(partit == "Vox" ~ lb95*correcciovox, 
                    TRUE ~ lb95),
    up95= case_when(partit == "Vox" ~ up95*correcciovox, 
                    TRUE ~ up95))

propentren<-proportionsm4 %>% 
  filter(!grepl('Altres|BAN', partit)) %>%
  mutate(
    propcand=proporcio/sum(proporcio))

resultats<-(propentren$propcand)

#Estimation by district
factorsprov <- data.frame("partit"=c("PP", "ERC", "PSC", "Cs", "CUP", "Junts", "ECP", "Vox"),
                          resultats,
                          "factor.bcn"=c(1.048828125, 0.945442875, 1.073498233, 1.067010309, 0.968181818, 0.867584746, 1.101226994, 1.013824885),
                          "factor.gi"=c(0.6484375, 1.123876765, 0.709540636, 0.680412371, 1.372727273, 1.776483051, 0.656441718, 0.811059908),
                          "factor.lle"=c(0.912109375, 1.331835687, 0.674204947, 0.585051546, 1.018181818, 1.576271186, 0.531697342, 0.68202765),
                          "factor.tgn"=c(1.017578125, 1.102695764, 0.905300353, 1.023195876, 0.875, 0.94809322, 0.829243354, 1.246543779))
res2021<-factorsprov %>%
  mutate(
    Barcelona = resultats*factor.bcn,
    Girona=resultats*factor.gi,
    Lleida=resultats*factor.lle,
    Tarragona=resultats*factor.tgn) %>%
  select(Barcelona:Tarragona)


# Results

# Uncertainty
sig2021 <- ceobarometre::moe(res2021, N=2000, level=.95) 
# Party names
parties <- c("PP", "ERC", "PSC", "Cs", "CUP", "Junts", "ECP", "Vox")
# Simulation
set.seed(25627)
res <- ceobarometre::simulate(res2021, sig2021, parties, N=10000,
                dsize = c(Barcelona = 32, Girona = 6, Tarragona = 4, Lleida = 6))

# Transform to data.frame for analysis
res <- as.data.frame(res)

R<- res |>
  group_by(party) |>
  summarize(lo05=quantile(total, .05),
            median=quantile(total, .5),
            hi95=quantile(total, .95))
R

R3<-R
R3$party <- factor(R3$party,                                               # Replicate original data
                   levels = R3$party[order(R3$hi95, decreasing = TRUE)])


p<-ggplot(R3, aes(party, median, fill=party)) + 
  geom_col(width = 0.7, show.legend = FALSE) +  
  geom_text(    aes(x = party, y = lo05, label = sprintf("%0.0f", round(lo05, digits = 0))),
                position = position_dodge(width = 1),
                vjust = 1.5, size = 4  ) + 
  geom_text(  aes(x = party, y = hi95, label = sprintf("%0.0f", round(hi95, digits = 0))),
              position = position_dodge(width = 1),
              vjust = -0.7, size = 4  ) + 
  scale_fill_manual(values = c("#FFB232", "#E73B39",  "#00C3B2", "#6E236E", "#ffed00", "#63BE21",  "#0bb2ff", "#EB6109"))+
  geom_errorbar(aes(ymin = lo05, ymax = hi95), width=0.2)+
  labs(y="Escons ± 95%CI", x = "Partit") 
p
png("figesconscong.png", units="in", width=8, height=8, res=300)
print(p)
dev.off()


#gràfica pct vot
proportionsm4parlcong <- read_delim("proportionsm4parlcong.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)

proportionsm4parlcong<-proportionsm4parlcong %>% 
  filter(!grepl('Altres', partit)) %>%
  mutate(
    proporcio= case_when(partit == "Vox" ~ proporcio*correcciovox, 
                         TRUE ~ proporcio),
    lb95= case_when(partit == "Vox" ~ lb95*correcciovox, 
                    TRUE ~ lb95),
    up95= case_when(partit == "Vox" ~ up95*correcciovox, 
                    TRUE ~ up95))

Rpct<-proportionsm4parlcong %>% 
  mutate(
    proporcio=proporcio*100,
    lb95=lb95*100,
    up95=up95*100 ) %>%
  select(partit, proporcio, lb95, up95) %>%
  arrange(desc(proporcio))

Rpct$partit <- factor(Rpct$partit,                                               
                      levels = Rpct$partit[order(Rpct$proporcio, decreasing = TRUE)])


p2<-ggplot(Rpct, aes(partit, proporcio, fill=partit)) + 
  geom_col(width = 0.7, show.legend = FALSE) +  
  geom_text(    aes(x = partit, y = lb95, label = sprintf("%0.0f", round(lb95, digits = 0))),
                position = position_dodge(width = 1),
                vjust = 1.5, size = 4  ) + 
  geom_text(  aes(x = partit, y = up95, label = sprintf("%0.0f", round(up95, digits = 0))),
              position = position_dodge(width = 1),
              vjust = -0.7, size = 4  ) + 
  scale_fill_manual(values = c("#E73B39", "#FFB232", "#6E236E", "#00C3B2", "#63BE21",  "#0bb2ff", "#ffed00", "#EB6109"))+
  #  scale_x_discrete(limits = positions) +
  geom_errorbar(aes(ymin = lb95, ymax = up95), width=0.2)+
  labs(y="Percentatge de Vot vàlid ± 95%CI", x = "Partit") 
p2
png("figvotscong.png", units="in", width=8, height=8, res=300)
print(p2)
dev.off()
