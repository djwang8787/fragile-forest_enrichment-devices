
pacman::p_load(dplyr, ggplot2, lubridate, reshape2)

#### BoH data ####
# Only lemurs
boh = read.csv("FF-BoH-data.csv")
boh$newDate = boh[,1]
boh = boh %>%
  dplyr::mutate(newDate = as.Date(newDate, format = "%m/%d/%Y"),
                Session = rep(1:8, each = 2),
                Avg = Duration/Counts)

boh %>%
  ggplot(., aes(x = as.factor(Session), y = Counts, fill = Device)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  theme_minimal() +
  facet_wrap(.~Time, ncol = 1) +
  theme(legend.position = "bottom") +
  xlab("Session #") +
  ylab('Interaction counts')

boh %>%
  ggplot(., aes(x = as.factor(Session), y = Duration/Counts, fill = Device)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  theme_minimal() +
  facet_wrap(.~Time, ncol = 1) +
  theme(legend.position = "bottom") +
  xlab("Session #") +
  ylab("Duration (s)")

#### Biodome data ####
biodome = read.csv("FF-biodome-data.csv")
biodome[is.na(biodome)] = 0

biodome$newDate = biodome[,1]
biodome[,1] = NULL
biodome$newDate = as.POSIXct(biodome$newDate, format = "%m/%d/%Y")


##### Coarse device-centric approach ####
c.data = biodome %>%
  group_by(newDate, Time) %>%
  mutate(Total = sum(CP, LMD, AP, WD, CHACA, NIC,MAGPIE, PS, BM, VS, Lemur))

c.data = c.data %>%
  group_by(newDate, Time, Device) %>%
  mutate(Device.total = sum(CP, LMD, AP, WD, CHACA, NIC,MAGPIE, PS, BM, VS, Lemur),
         Prop = Device.total/Total*100) %>%
  select(newDate, Time, Device, Device.total, Total, Prop)
c.data$Session = rep(1:12, each = 8)

c.data %>%
  ggplot(., aes(x = Session, y = Prop, fill = Device)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  ylab('Proportion of interations') +
  xlab('Session #') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  facet_wrap(.~Time, ncol = 1)

##### Coarse device-centric approach analysis #####
c.data.alys = biodome %>%
  select(-c(Time, Weather)) %>%
  pivot_longer(cols = CP:Lemur, names_to = "Species", values_to = "counts")

c.model = aov(counts~Device + Species,
              data = c.data.alys)
summary(c.model)
TukeyHSD(c.model)

c.model2 = lm(counts~Device,
              data = c.data.alys)
c.model0= lm(counts~1,
              data = c.data.alys)

c.model1 = lm(counts~Species,
              data = c.data.alys)
c.model3 = lm(counts~Device + Species,
              data = c.data.alys)

MuMIn::model.sel(c.model0, c.model1, c.model2, c.model3)
stargazer::stargazer(c.model0, c.model1, c.model2, c.model3,
                     type = "text",
                     title = "Regression results",
                     out= "dat1.text")

##### Coarse device-centric approach viz #####
c.data.device.viz = c.data.alys %>%
  group_by(Device) %>%
  summarise(counts = sum(counts),
            mean = mean(counts),
            sd = sd(counts)) %>%
  mutate(total = sum(counts),
         prop = (counts/total)*100)

c.data.alys %>%
  filter(Device %in% c("Ball")) %>%
  summarise(mean(counts))

ggplot(c.data.device.viz) +
  geom_bar(aes(x = Device, y = counts, fill = Device), stat = "identity") +
  theme_minimal() +
  ylab("Counts of interaction") +
  theme(legend.position = "none")

##### Device-centric ####
device.data = biodome %>%
  filter(Device %in% c("Basket", "Pipe")) %>%
  group_by(newDate, Device) %>%
  mutate(Total = sum(CP, LMD, AP, WD, CHACA, NIC,MAGPIE, PS, BM, VS, Lemur))


device.data = device.data %>%
  group_by(newDate) %>%
  mutate(Device.total = sum(CP, LMD, AP, WD, CHACA, NIC,MAGPIE, PS, BM, VS, Lemur),
         Prop = (Device.total/Total)*100) %>%
  select(newDate, Time, Device, Device.total, Total, Prop)

device.data = melt(device.data, variable.name = "Species", value.name = "Counts", id = c("newDate","Device","Total"))

device.data = device.data %>%
  group_by(newDate, Species, Device) %>%
  dplyr::summarise(Prop = Counts/Total*100) %>%
  arrange(newDate)


prop.daily.biodome$Session = rep(1:12, each = 44)


prop.daily.biodome %>% filter(Device == "Ball") %>%
ggplot(., aes(x = Session, y = Prop, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  ylab('Proportion of interactions (Ball)') +
  xlab('Session') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1))


prop.daily.biodome %>% filter(Device == "Pipe") %>%
  ggplot(., aes(x = Session, y = Prop, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  ylab('Proportion of interactions (Pipe)') +
  xlab('Session') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1))

prop.daily.biodome %>% filter(Device == "Basket") %>%
  ggplot(., aes(x = Session, y = Prop, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  ylab('Proportion of interactions (Basket)') +
  xlab('Session') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1))

prop.daily.biodome %>% filter(Device == "Box") %>%
  ggplot(., aes(x = Session, y = Prop, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = 'black')+
  ylab('Proportion of interactions (Box)') +
  xlab('Session') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1))

# glmer
library("lme4")
m0 = lmer(Counts~1 + (1|Session),
          data = boh)

m1 = lmer(Counts~Device + (1|Session),
          data = boh)

m2 = lmer(Counts~Time + (1|Session),
          data = boh)

m3 = lmer(Counts~Weather + (1|Session),
          data = boh)

m4 = lmer(Counts~Time*Device + (1|Session),
          data = boh)

m5 = lmer(Counts~Weather*Device + (1|Session),
          data = boh)

m6 = lmer(Counts~Weather*Time + (1|Session),
          data = boh)

model.sel(m0, m1, m2, m3, m4, m5, m6)
m1

m0 = lmer(Duration/Counts~1 + (1|Session),
          data = boh)

m1 = lmer(Duration/Counts~Device + (1|Session),
          data = boh)

m2 = lmer(Duration/Counts~Time + (1|Session),
          data = boh)

m3 = lmer(Duration/Counts~Weather + (1|Session),
          data = boh)

m4 = lmer(Duration/Counts~Time*Device + (1|Session),
          data = boh)

m5 = lmer(Duration/Counts~Weather*Device + (1|Session),
          data = boh)

m6 = lmer(Duration/Counts~Weather*Time + (1|Session),
          data = boh)

model.sel(m0, m1, m2, m3, m4, m5, m6)
m6

