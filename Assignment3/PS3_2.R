bone_Data <- read.delim2("bone.txt", head=TRUE)
bone_Data[1,2]
isotopic <- matrix(nrow=72, ncol=2)
isotopic[1.2]
for (i in 1:12) {
  for (j in 1:6) {
    k <- j+6*(i-1)
  isotopic[k, 1] = bone_Data[i, 1]
  isotopic[k, 2] = bone_Data[i, j+1] 
  }
}
colnames(isotopic)=c("Bone","Oxygen.isotopic.composition")
isotopic_new <- as_tibble(isotopic)
isotopic_new1 <- isotopic_new  %>%
  mutate(OIC= as.numeric(Oxygen.isotopic.composition))
isotopic_new1 %>%
  group_by(Bone) %>%
  summarise(
    count = n(),
    mean_isotopic = mean(OIC, na.rm = TRUE),
    sd_isotopic = sd(OIC, na.rm = TRUE)
  )  
ggplot(isotopic_new1, aes(x = Bone, y = OIC, fill = Bone)) +
  geom_boxplot() +
  theme_classic()
anova_one_way <- aov(OIC ~ Bone, data = isotopic_new1)
summary(anova_one_way)
TukeyHSD(anova_one_way)


new_Data <- read.delim("new.txt", head=TRUE)
new_Data %>%
  group_by(Bone) %>%
  summarise(
    count = n(),
    mean_isotopic = mean(Oxygen.isotopic.composition, na.rm = TRUE),
    sd_yield = sd(Oxygen.isotopic.composition, na.rm = TRUE)
  )
new_Data %>%
  group_by(Bone) %>%
  ggplot(aes(x = Bone, y = Oxygen.isotopic.composition, fill = Bone)) +
  geom_boxplot() +
  theme_classic()
anova_one_way1 <- aov(Oxygen.isotopic.composition ~ Bone, data = new_Data)
summary(anova_one_way1)
TukeyHSD(anova_one_way1)
