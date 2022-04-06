
# *Codi per generar l'estimació de vot al Parlament de Catalunya 
# *del Baròmetre 1-2022 del Centre d'Estudis d'Opinió
# 
# 
# /*Aquest codi parteix de l'estimació de vot generada pel codi EstimacioBOP22_1_v3.do (Stata)
# 
# El codi realitza les següents operacions:
# 1. Aplica correcció vot ocult de vot
# 2. Estima distribució de vot entre províncies a partir de factor multiplicador de 2021 per cada partit
# 3. Crea una matriu d'incertesa (moe)
# 4. Simula la distribució d'escons mitjançant la regla d'Hondt a les 4 províncies, 1000 simulacions (dist. normal)
# 5. Representa gràficament els resultats

library(ceobarometre) #paquet encara no disponible. Les funcions moe i simulate a l'arxiu votes2seats.R
library(readr)
library(tidyverse)

#input data
proportionsm4 <- read_delim("proportionsm4.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

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

#Estimacions per província
factorsprov <- data.frame("partit"=c("PP", "ERC", "PSC", "Cs", "CUP", "Junts", "ECP", "Vox"),
resultats,
#"resultats"=c(0.05965434, 0.24475005, 0.27541349, 0.0314068, 0.07284891, 0.17227281, 0.07210556, 0.07154804),
"factor.bcn"=c(1.05264192, 0.95867429, 1.08730481, 1.09375847, 0.94417313, 0.89315029, 1.1301855, 1.01745065),
"factor.gi"=c(0.5183092, 1.02418894, 0.65919112, 0.58373687, 1.3518666, 1.62802819, 0.58953599, 0.80168356),
"factor.lle"=c(0.9212604, 1.24757524, 0.65210494, 0.57770628, 1.10668443, 1.39740827, 0.47032462, 0.7205036),
"factor.tgn"=c(1.11991769, 1.14938397, 0.87040195, 0.93873465, 1.01697947, 0.96596755, 0.71618978, 1.22432748))

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

#?simulate
set.seed(1234)
res <- ceobarometre::simulate(res2021, sig2021, parties, N=1000)
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

#Representació gràfica

p<-ggplot(R3, aes(party, median, fill=party)) + 
  geom_col(width = 0.7, show.legend = FALSE) +  
  geom_text(    aes(x = party, y = lo05, label = sprintf("%0.0f", round(lo05, digits = 0))),
    position = position_dodge(width = 1),
    vjust = 1.5, size = 4  ) + 
  geom_text(  aes(x = party, y = hi95, label = sprintf("%0.0f", round(hi95, digits = 0))),
    position = position_dodge(width = 1),
    vjust = -0.7, size = 4  ) + 
  scale_fill_manual(values = c("#E73B39", "#FFB232", "#00C3B2", "#63BE21", "#ffed00", "#6E236E", "#0bb2ff", "#EB6109"))+
  geom_errorbar(aes(ymin = lo05, ymax = hi95), width=0.2)+
  labs(y="Escons ± 95%CI", x = "Partit") 
p
png("figescons.png", units="in", width=8, height=8, res=300)
print(p)
dev.off()


#Gràfica pct vot
proportionsm4parl <- read_delim("proportionsm4parl.csv", 
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                trim_ws = TRUE)

proportionsm4parl <- proportionsm4parl %>%
  mutate(
    proporcio= case_when(partit == "Vox" ~ proporcio*correcciovox, 
                         TRUE ~ proporcio),
    lb95= case_when(partit == "Vox" ~ lb95*correcciovox, 
                    TRUE ~ lb95),
    up95= case_when(partit == "Vox" ~ up95*correcciovox, 
                    TRUE ~ up95))

Rpct<-proportionsm4parl %>% 
  filter(!grepl('Altres', partit)) %>%
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
  scale_fill_manual(values = c("#E73B39", "#FFB232", "#00C3B2", "#63BE21", "#ffed00", "#6E236E", "#0bb2ff", "#EB6109"))+
  geom_errorbar(aes(ymin = lb95, ymax = up95), width=0.2)+
  labs(y="Percentatge de Vot vàlid ± 95%CI", x = "Partit") 
p2
png("figvots.png", units="in", width=8, height=8, res=300)
print(p2)
dev.off()
