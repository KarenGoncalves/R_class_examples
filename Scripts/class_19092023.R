#### Install and load the packages ####
# start by creating a vector with all the packages you need
pkgs = c("tidyverse")

# We check which packages are NOT (!) installed
pkgs.To.Install = ! pkgs %in% installed.packages()

# any() checks if there is at least one TRUE in the vector
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])

for (curPkg in pkgs) library(curPkg, character.only = T) 
# curPkg is a variable that takes the value of each element in pkgs
# Every time the function library() is run, curPkg changes value

#### Using the pipe ####
# You could first put the path to the file in a variable, it would make things easier
myFile = "https://karengoncalves.github.io/Programming_classes/r/exampleData/Class1_exampleData.txt"

# The two commands below do the same thing, but the second is more logical
(str(read.delim(myFile)))
(read.delim(myFile) %>% str)

#### Boxplot with ggplot ####
# aes is short for aesthetics, basically what columns hold the values you want to plot
# fill is the color that will be inside the box, color is just for the border
ggplot(data = iris,
       aes(x = Species,
           y = Sepal.Length,
           fill = Species)) +
	geom_boxplot() + # the type of plot you want
	ylab("Sepal length (mm)") +
	xlab("Species epithet")

# Add the colors with scale_fill_manual and remove the legend with show.legend = F inside geom_boxplot()
# Set a theme for all future plots in this session
theme_set(theme_bw()) 
# Use different colors to fill and remove the legend
colors = c("red", "green", "blue")
ggplot(data = iris,
       aes(x = Species,
           y = Sepal.Length,
           fill = Species)) +
	geom_boxplot(show.legend = F) + # the type of plot you want
	ylab("Sepal length (mm)") +
	xlab("Species epithet") +
	scale_fill_manual(values = colors)

#### dplyr ####
# Prepare the data
myFile = "https://karengoncalves.github.io/Programming_classes/r/exampleData/Class1_exampleData.txt"
rawData = read.delim(myFile)
# The table is in format wide = columns are replicates, rows are measurements, each cell has a value
names(rawData)
# Let's change x to "Measured"
names(rawData)[1] = "Measured"

# Transform to long format: the values of all the columns will be stored in a single column
# so we need another column that tells us where the value came from
# We don't include the column Measured in the transformation because it already is a single column and contains information about the row
longData = pivot_longer(
	data = rawData,
	cols = !Measured, # gets all the columns of the table, except for the one in front of !
	names_to = "Replicates", # name of the column that will contain column names from rawData
	values_to = "Measurements"
)

str(longData)

# Let’s split the values from “Replicates” using str_split from stringr
# mutate will return the input table with the new column we create
# pattern is what separates (_)
# i is the part that we want to see: Control_1 has 2 pieces, i=1 returns "Control"

longDataTreatments = longData %>%
	mutate(Treatment = str_split_i(Replicates, pattern = "_", i = 1))

str(longDataTreatments)

# Now we can plot and filter the table easily because the values are more logically distributed
longDataTreatments %>%
	ggplot(aes(x = Treatment, y = Measurements, fill = Measured)) +
	geom_boxplot()

longDataTreatments %>%
	ggplot(aes(x = Measured, y = Measurements, fill = Measured)) +
	geom_boxplot()

longDataTreatments %>%
	ggplot(aes(x = Measured, y = Measurements, fill = Treatment)) +
	geom_boxplot()

longDataTreatments %>%
	filter(Measurements > 0.5)
