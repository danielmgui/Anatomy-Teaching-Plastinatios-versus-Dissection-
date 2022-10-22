library(sjPlot)

maintable = read.csv("MainTableCod.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";")
auxtable = read.csv("AuxTable2Cod.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";")

maintable$PreT.TorF.Score = (maintable$PreT.TorF.Score*100)/39 
maintable$PreT.Draw.Score = (maintable$PreT.Draw.Score*100)/14
maintable$PostT.TorF.Score = (maintable$PostT.TorF.Score*100)/39
maintable$PostT.Draw.Score = (maintable$PostT.Draw.Score*100)/14
maintable$PreT.Sum.Score = (maintable$PreT.Sum.Score*100)/53 
maintable$PostT.Sum.Score = (maintable$PostT.Sum.Score*100)/53

auxtable$PreT.TorF.Score = (auxtable$PreT.TorF.Score*100)/39 
auxtable$PreT.Draw.Score = (auxtable$PreT.Draw.Score*100)/14
auxtable$PostT.TorF.Score = (auxtable$PostT.TorF.Score*100)/39
auxtable$PostT.Draw.Score = (auxtable$PostT.Draw.Score*100)/14
auxtable$PreT.Sum.Score = (auxtable$PreT.Sum.Score*100)/53 
auxtable$PostT.Sum.Score = (auxtable$PostT.Sum.Score*100)/53

# Post-test Sum (draw and true or false combined)
summary = summarySE(maintable, measurevar = "PostT.Sum.Score", groupvars = c("Group", "Sex"), na.rm = T)

g1 = ggplot(summary, aes(x = Sex, y = PostT.Sum.Score, fill = Group)) + 
  geom_errorbar(aes(ymin = PostT.Sum.Score-sd, ymax = PostT.Sum.Score+sd), position=position_dodge(.9), width = .2) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  ylab("Post-Test Score (%)") +
  xlab("Sex") +
  theme_classic() +
  theme(text = element_text(size = 17), 
        plot.title = element_text(size = 15, face = "bold")) +
  scale_fill_grey() 

ggsave(filename = "anovaPostTestSum.tiff", plot = g1)

result.anova = aov(formula = PostT.Sum.Score ~ Group * Sex,
                   data = maintable)

summary(result.anova)
emm = emmeans(result.anova, ~ Group | Sex)
pairs(emm, adjust = "BH")

maintable [, 5:10] = scale(maintable [, 5:10])

regressor = lm(formula = PostT.Sum.Score ~ PreT.Sum.Score + Sex + Group + Class,
               data = maintable)
summary(regressor)

# Image Quiz segreggated
summary = summarySE(maintable, measurevar = "PostT.Draw.Score", groupvars = c("Group", "Sex"), na.rm = T)

g2 = ggplot(summary, aes(x = Sex, y = PostT.Draw.Score, fill = Group)) + 
  geom_errorbar(aes(ymin = PostT.Draw.Score-sd, ymax = PostT.Draw.Score+sd), position=position_dodge(.9), width = .2) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  ylab("Image Quiz Post-Test Score (%)") +
  xlab("Sex") +
  theme_classic() +
  theme(text = element_text(size = 17), 
        plot.title = element_text(size = 15, face = "bold")) +
  scale_fill_grey() 

ggsave(filename = "anovaPostTestIMAGEQUIZ.tiff", plot = g2)

result.anova = aov(formula = PostT.Draw.Score ~ Group * Sex,
                   data = maintable)

summary(result.anova)
emm = emmeans(result.anova, ~ Group | Sex)
pairs(emm, adjust = "BH")

maintable [, 5:10] = scale(maintable [, 5:10])

regressor = lm(formula = PostT.Draw.Score ~ PreT.Draw.Score + Sex + Group + Class,
               data = maintable)
summary(regressor)

# True or False
summary = summarySE(maintable, measurevar = "PostT.TorF.Score", groupvars = c("Group", "Sex"), na.rm = T)

g3 = ggplot(summary, aes(x = Sex, y = PostT.TorF.Score, fill = Group)) + 
  geom_errorbar(aes(ymin = PostT.TorF.Score-sd, ymax = PostT.TorF.Score+sd), position=position_dodge(.9), width = .2) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  ylab("True or False Post-Test Score (%)") +
  xlab("Sex") +
  theme_classic() +
  theme(text = element_text(size = 17), 
        plot.title = element_text(size = 15, face = "bold")) +
  scale_fill_grey() 

ggsave(filename = "anovaPostTestTRUEORFALSE.tiff", plot = g3)

result.anova = aov(formula = PostT.TorF.Score ~ Group * Sex,
                   data = maintable)

summary(result.anova)
emm = emmeans(result.anova, ~ Group | Sex)
pairs(emm, adjust = "BH")

maintable [, 5:10] = scale(maintable [, 5:10])

regressor = lm(formula = PostT.TorF.Score ~ PreT.TorF.Score + Sex + Group + Class,
               data = maintable)
summary(regressor)

# Subjective experience vs postest outcome
summary = summarySE(auxtable, measurevar = "PostT.Sum.Score", groupvars = c("Knowledge.Rate", "Sex"), na.rm = T)

g4 = ggplot(summary, aes(x = Sex, y = PostT.Sum.Score, fill = Knowledge.Rate)) + 
  geom_errorbar(aes(ymin = PostT.Sum.Score-sd, ymax = PostT.Sum.Score+sd), position=position_dodge(.9), width = .2) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  ylab("Post-Test Score (%)") +
  xlab("Sex") +
  theme_classic() +
  theme(text = element_text(size = 17), 
        plot.title = element_text(size = 15, face = "bold")) +
  scale_fill_grey() 

ggsave(filename = "anovaPostTestSumKnowledge.Rate.tiff", plot = g4)

result.anova = aov(formula = PostT.Sum.Score ~ Knowledge.Rate * Sex,
                   data = auxtable)

summary(result.anova)
emm = emmeans(result.anova, ~ Knowledge.Rate | Sex)
pairs(emm, adjust = "BH")

auxtable [, 5:10] = scale(auxtable [, 5:10])

regressor = lm(formula = PostT.Sum.Score ~ PreT.Sum.Score + Knowledge.Rate + Emotional.State + Sex,
               data = auxtable)
summary(regressor)
