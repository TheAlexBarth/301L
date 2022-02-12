##
# Course Info Investigation
##

raw <- readRDS('./Data/spring22_admin01_initial-class-surv.rds')
source('./R/general_tool_support-fx.R')

##
# Plot of the number of respondents
unanswr <- as.data.frame(table(raw$EQC))
ggplot(unanswr)+
  geom_bar(aes(x = Var1, y = Freq,fill = Var1),stat = "Identity")+
  scale_fill_manual(values = c("Grey","DarkGreen"))+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",y = "Num. Students")+
  theme_classic()+
  theme(legend.position = "none",axis.text = element_text(face = "bold",
                                                          color = "black"))

##
# What is the distribution of ages?
raw$Year <- factor(raw$Year, levels = c("Fresh","Soph","Junior","Senior"))
ggplot(raw[raw$EQC != "No",])+
  geom_bar(aes(x = Year,fill = Year),stat = "count")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",y = "Num. Students")+
  theme_classic()+
  theme(legend.position = "none",axis.text = element_text(face = "bold",
                                                          color = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1),
        axis.title.y = element_text(color = "black",face = "bold"))

pie(table(raw$Year)/nrow(raw),col = gg_color_hue(length(levels(as.factor(raw$Year)))))

##
# What is the distribution of majors?
raw$Major <- factor(raw$Major, levels = c("Biol","Envr","CardioTech","Psych",
                                          "Excer"))

ggplot(raw[raw$EQC != "No",])+
  geom_bar(aes(x = Major,fill = Major),stat = "count")+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("lightblue","darkgreen","red4","red2",
                               "blue2"))+
  labs(x = "",y = "Num. Students")+
  theme_classic()+
  theme(legend.position = "none",axis.text = element_text(face = "bold",
                                                          color = "black"))

pie(table(raw$Major)/nrow(raw), col =  c("lightblue","darkgreen","red4","red2",
                                         "blue2"))
table(raw$Major)/sum(raw$EQC == "Yes")                      

##
# What is the distribution of future Goals
raw$Future <- factor(raw$Future,levels = c("Dental","MD","Eco","PA","Vet","Med",
                                           "Teachers",
                                           "Opto","CadioTech","Research",
                                           "None"))
ggplot(raw[raw$EQC != "No",])+
  geom_bar(aes(x = Future,fill = Future),stat = "count")+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("lightblue","darkgreen","blue4","blue3","green4",
                               "blue2","blue1","red4","red2","yellow","grey"))+
  labs(x = "",y = "Num. Students")+
  theme_classic()+
  theme(legend.position = "none",axis.text = element_text(face = "bold",
                                                          color = "black"),
          axis.text.x = element_text(angle = 45,hjust = 1))

pie(table(raw$Future)/nrow(raw), col = c("lightblue","darkgreen","blue4","blue3","green4",
                                         "blue2","blue1","red4","red2","yellow","grey"))
table(raw$Future)/sum(raw$EQC == "Yes")

##
# Excited for
ggplot(raw[raw$EQC != "No",])+
  geom_bar(aes(x = LookingTo,fill = LookingTo),stat = "count")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",y = "Num. Students")+
  theme_classic()+
  theme(legend.position = "none",axis.text = element_text(face = "bold",
                                                          color = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1),
        axis.title.y = element_text(color = "black",face = "bold"))

pie(table(raw$LookingTo)/nrow(raw),col = gg_color_hue(length(levels(as.factor(raw$LookingTo)))))

##
# Worried about
ggplot(raw[raw$EQC != "No",])+
  geom_bar(aes(x = Concern,fill = Concern),stat = "count")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "",y= "Num. Students")+
  theme_classic()+
  theme(legend.position = "none",axis.text = element_text(face = "bold",
                                                          color = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1),
        axis.title.y = element_text(color = "black",face = "bold"),
        panel.background = element_blank(),plot.background = element_blank())
par(bg = "transparent")
pie(table(raw$Concern)/nrow(raw),col = gg_color_hue((length(levels(as.factor(raw$Concern))))))
