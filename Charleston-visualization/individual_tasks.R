#INDIVIDUAL BARPLOT
meanWeightEdu <- dataNewborns %>%
  group_by(edu.M) %>%
  summarise(meanWeight = round(mean(weight.C), 2))

ggplot(meanWeightEdu,
       aes(x = edu.M, y = meanWeight)) +
  geom_col(fill = "lightblue") +
  ylim(0,3500) + 
  geom_text(
    aes(label = meanWeight),
    vjust = -1) +
  labs(
    title = "Mean birth weight by mother's education level",
    x = "Mother's education level",
    y = "Mean birth weight")

#INDIVIDUAL HISTOGRAM
str(diamonds)
head(diamonds)
ggplot(diamonds,
       aes(x = price,
           fill = cut)) +
  geom_histogram(
    bins = round(1 + 3.3 * log10(length(diamonds$price)))) +
  facet_grid(cut ~ ., scales = "free_y") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribution of Diamond Prices by cut type",
    x = "Price",
    y = "Frequency",
    fill = "Cut") +
  theme_minimal()

#INDIVIDUAL SCATTERPLOT
str(women)
head(women)
ggplot(women,
       aes(x = height,
           y = weight)) +
geom_point(
  color = "darkgreen",
  size = 4,
  shape = 17) +
geom_smooth(
    method = "lm",
    level = 0.95) +
labs(
  title = "Relationship Between Height and Weight of Women",
  x = "Height",
  y = "Weight") +
theme_minimal()
