library(ggplot2)
library(dplyr)

#LOADING DATA NEWBORN.TXT
dataNewborns <- read.delim("newborns.txt")
sum(!complete.cases(dataNewborns)) #does it have NAs
dataNewborns <- tidyr::drop_na(dataNewborns)
str(dataNewborns) #checking data
#factorizing categorical values
dataNewborns$edu.M <- factor(dataNewborns$edu.M, labels = c("Grade school", "Vocational school", "High school", "University"))
dataNewborns$sex.C <- factor(dataNewborns$sex.C, labels = c("Female", "Male"))
str(dataNewborns)
##############################################################################################
#BARPLOT
##############################################################################################
#CREATING BARPLOT
ggplot(data = dataNewborns,
  aes(x = edu.M)) + 
  geom_bar()

#CHANGING COLOURS
ggplot(data = dataNewborns,
       aes(x = edu.M)) +
  geom_bar(fill = "red", #change the colour of the bar
           colour = "darkgreen") #change the outline

#ADDING LEGEND
ggplot(data = dataNewborns,
       aes(x = edu.M)) +
  geom_bar(fill = "red",
           colour = "darkgreen") +
labs(title="Number of children per each educational level of the mothers", 
     x = "Education level", y = "Number of children") #axis and graph legend

#HANGING BACKGROUND
ggplot(data = dataNewborns,
       aes(x = edu.M)) +
  geom_bar(fill = "red",
           colour = "darkgreen") +
  labs(title="Number of children per each educational level of the mothers", 
       x = "Education level", y = "Number of children") +
  theme_minimal() #change the background

#ADDING VALUE LABELS
ggplot(data = dataNewborns,
       aes(x = edu.M)) +
  geom_bar(fill = "red",
           colour = "darkgreen") +
  geom_text(stat = "count", #count observation in each category
            aes(label = after_stat(count)),
            vjust = 2) + #place the text distance 2 from the edge
  labs(title="Number of children per each educational level of the mothers", 
       x = "Education level", y = "Number of children")

#ADDING DIVISION BY CATEGORIES
ggplot(data = dataNewborns,
       aes(x = edu.M,
           fill = sex.C)) + #divide according to sex.C
  geom_bar() + 
  labs(title="Number of children per each educational level of the mothers", 
       x = "Education level",
       y = "Number of children",
       fill = "Sex") + #name what the categories are divided by
  theme_minimal()

#MOVING THE CATEGORIES NEXT TO EACH OTHER
ggplot(data = dataNewborns,
       aes(x = edu.M,
           fill = sex.C)) +
  geom_bar(colour = "darkgreen",
           position = position_dodge()) + #change the position of categories
  labs(title="Number of children per each educational level of the mothers", 
       x = "Education level", y = "Number of children", fill = "Sex") +
  theme_minimal()

#ADDING VALUES ABOVE EACH BIN WHEN DODGED
  #easier to use geom_col with multiple categories
freqEduSex <- dataNewborns %>%
  dplyr::count(edu.M, sex.C); freqEduSex

ggplot(freqEduSex,
       aes(x = edu.M, y = n, fill = sex.C)) +
  geom_col(position = position_dodge(width = 0.9)) + #set the labels next to each other by certain distance
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3) +
  labs(title="Number of children per each educational level of the mothers", 
       x = "Education level", y = "Number of children", fill = "Sex") +
  theme_minimal()

##############################################################################################
#HISTOGRAM
#############################################################################################
#CREATING HISTOGRAM
ggplot(dataNewborns)+
  aes(x = weight.C)+
  geom_histogram(bins = 11) #the bins argument is result of the Sturge's rule calculation

#ADDING A LINE SHOWING THE MEAN VALUE
ggplot(data = dataNewborns) +
  aes(x = weight.C) +
  geom_histogram(bins = 11,
                 color = "black",
                 fill = "pink") +
  geom_vline(aes(xintercept = mean(weight.C)), #establish the mean and at this position draw the line at x axis
             color = "blue", #line color
             linetype = "dashed", #line type
             linewidth = 1) #line width

#CREATING CATEGORIES AND LAYERING THEM ON TOP OF EACH OTHER
ggplot(data = dataNewborns) +
  aes(x = weight.C,
      fill = sex.C) +
  geom_histogram(bins = 11,
                 alpha = 0.4, #set opacity of each category to 40%
                 position = position_identity()) + #layer categories on each other
  geom_vline(aes(xintercept = mean(weight.C)),
             color = "blue",
             linetype = "dashed",
             linewidth = 1) +
  labs(title="Weight of children per sex", 
       x = "Weight (g)", y = "Number of children", fill = "Sex")


#ADDING A LINE SHOWING THE MEAN VALUE FOR EACH CATEGORY
meanWeightSex <- dataNewborns %>% 
  group_by(sex.C) %>% 
  summarise(mean_weight = mean(weight.C)); meanWeightSex
  #creating data that can be used for creating the mean lines

ggplot(data = dataNewborns) +
  aes(x = weight.C,
      fill = sex.C) +
  geom_histogram(bins = 11,
                 alpha = 0.4,
                 position = position_identity()) +
  geom_vline(data = meanWeightSex,
             aes(xintercept = mean_weight, #use the data saved in this column
                 colour = sex.C), #as we divide the histogrsm by category here we do the same with theline
             linetype = "dashed",
             linewidth = 1) +
  labs(title="Weight of children per sex", 
       x = "Weight (g)",
       y = "Number of children",
       fill = "Sex",
       colour = "Sex") #rewriting the legend of the lines as well)

#CHANGING COLOURS OF THE LINE AND FILL COLOUR
ggplot(data = dataNewborns) +
  aes(x = weight.C,
      fill = sex.C) +
  geom_histogram(bins = 11,
                 alpha = 0.4,
                 position = position_identity()) +
  geom_vline(data = meanWeightSex,
             aes(xintercept = mean_weight,
                 colour = sex.C),
             linetype = "dashed",
             linewidth = 1) +
  labs(title="Weight of children per sex", 
       x = "Weight (g)",
       y = "Number of children",
       fill = "Sex",
       colour = "Sex") +
  scale_fill_manual(values = c("#56B4E9", "#E69F00")) + #manualy set colours of the fill
  scale_colour_brewer(palette = "Dark2") #changing the palette from which the colours are taken

#DIVING BY CATEGORIES TO INDIVIDUAL GRAPHS
ggplot(data = dataNewborns,
       aes(x = weight.C)) +
  geom_histogram(bins = 11, fill = "pink", color = "red") +
  facet_grid(sex.C ~ .) + #create separate panels (subplots) for each value of sex.C
  labs(title="Weight of children per sex", 
       x = "Weight (g)", y = "Number of children")

#DIVING BY CATEGORIES TO INDIVIDUAL GRAPHS
#CHANGING COLOURS
#INDIVIDUAL Y AXIS
ggplot(data = dataNewborns,
       aes(x = weight.C,
           fill = sex.C)) + #if we want to operate each category independently needs to be in aes
  geom_histogram(bins = 11, color = "red") +
  facet_grid(sex.C ~ ., scales = "free_y") + #change the y axis to fit each graph
  scale_fill_manual(values = c("Male" = "pink","Female" = "lightgreen")) +
  #values directly linking "Male" to value "Male" that is divided in aes fill
  labs(title="Weight of children per sex", 
       x = "Weight (g)", y = "Number of children", fill = "Sex")

##############################################################################################
#SCATTERPLOT
#############################################################################################
#CREATING SCATTERPLOT
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()

#DIVIDE BY CATEGORY
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 shape = Species, #use different shape for each category
                 color = Species, #use different color for each category
                 alpha = 0.4)) + #lower the opacity for better visibility
  geom_point(aes(size = Petal.Width)) + #size of the point is dependent on the size of Petal.Width
  labs(title="Relationship Between Sepal Length and Sepal Width", 
       x = "Sepal lenght (cm)",
       y = "Sepal width (cm)", 
    shape = "Species",
    color = "Species",
    size = "Petal Width") +
  theme_minimal()

#ADD DENSITY 
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 shape = Species,
                 color = Species,
                 alpha = 0.4)) +
  geom_point(aes(size = Petal.Width)) + 
  geom_density_2d() + #add lines of density
  labs(title="Relationship Between Sepal Length and Sepal Width", 
       x = "Sepal lenght (cm)",
       y = "Sepal width (cm)", 
       shape = "Species",
       color = "Species",
       size = "Petal Width") +
  theme_minimal()

##############################################################################################
#BOXPLOT
#############################################################################################
#CREATING BOXPLOT
ggplot(dataNewborns) +
  aes(x = sex.C,
      y = weight.C) +
  geom_boxplot()

#DIVIDING BY CATEGORIES, ADDING NOTCH AND CHANGING OUTLIERS
ggplot(dataNewborns) +
  aes(x = sex.C,
      y = weight.C,
      fill = sex.C) + # divide by categories
  geom_boxplot(outlier.size = 2.5, #change size of outliers
               outlier.shape = 21, #change the shape of outliers
               notch = TRUE) + #add notch
  labs(title = "Birth Weight by Sex", x = "Sex", y = "Birth Weight (g)",
       fill = "Sex") +
  theme_minimal()

#MAP VALUES ON BOXPLOT
ggplot(dataNewborns) +
  aes(x = sex.C,
      y = weight.C,
      fill = sex.C) +
  geom_boxplot(outlier.size = 2.5,
               outlier.shape = 21,
               notch = TRUE) +
  geom_jitter( #add values
    position = position_jitter(0.2), #how far away from middle
    colour = "darkgreen",
    alpha = 0.2) + #how opaque
  labs(title = "Birth Weight by Sex", x = "Sex", y = "Birth Weight (g)",
       fill = "Sex") +
  theme_minimal()

##############################################################################################
#HEATMAP
#############################################################################################
#CREATING HEATMAPS WITH THE WESANDERSON PALETTES
install.packages("wesanderson")
library(wesanderson)

names(wes_palettes) #what are the names of the available palettes

wesPalette <- wes_palette(
  "GrandBudapest2",
  n = length(eurodist),
  type = "continuous") #we want more colours than offered so the palette has to generate inbetweens

heatmap(x = dataEU,
        col = wesPalette,
        main = "Distances between EU cities")