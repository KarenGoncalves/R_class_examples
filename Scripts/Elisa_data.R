library(tidyverse)
library(readxl)
library(ggpubr)
theme_set(theme_bw())

#### Set basic variables ####
# Set the name of the input file and
ElisaFile = "Inputs/ELISA_YFP_Cys & YPF clones_EF20230330.xls"
columnNames = c("chlorophyll_pos", "YFP_pos", "Mean_YFP_pos")
colors = c("black", "blue", "forestgreen",
	   "red", "magenta", "orange")
chlorophyll_threshold = 75
sheetsElisa = excel_sheets(ElisaFile)

#### Read input file and prepare it ####
Elisa_Chlorophyll_YFP = lapply(sheetsElisa, \(sheet) {
	# All the sheets will be saved into a list
	currentSheet = read_excel(ElisaFile, sheet = sheet, na = "n/a")
	if (sheet != "Metadata") { # Just execute the code inside the {} if sheet is not "Metadata"
		# Create a column to indicate the technical replicates (name of the sheet)
		currentSheet$Technical_replicate = sheet
		# And indicate that the first column has the clone names
		names(currentSheet)[1] = "Clone"
	}
	currentSheet
})

names(Elisa_Chlorophyll_YFP) = sheetsElisa

# Specify which sheet doesn't contain measurements
notMetadata = sheetsElisa != "Metadata"
# Put all the sheets containing measurements into one table
Elisa_Chlorophyll_YFP_all = 
	do.call(what = "rbind", Elisa_Chlorophyll_YFP[notMetadata])

# Create a column with the construct name
Elisa_Chlorophyll_YFP_all$Construct = 
	factor(sapply(Elisa_Chlorophyll_YFP_all$Clone, \(cloneName) {
		# Remove the number from the clone name
		pattern = paste0("^", gsub("_\\d+$", "", cloneName), "$")
		# Get the construct name from the metadata
		cloneGroup = grep(pattern, Elisa_Chlorophyll_YFP$Metadata$Name)
		construct = Elisa_Chlorophyll_YFP$Metadata$Type[cloneGroup]
		# If the construct name in the metadata has the word negative, replace the name with EV
		ifelse(grepl("Negative", construct), "EV", construct)
	}), levels = c("EV", "YFP", "PtCys_YFP"))

#### Plot ####
# Create a dot plot using the colors to indicate the technical replicates
# And the x-axis to indicate the constructs

plots = list()
# Chlorophyll plot
plots[["chlorophyll_all"]] =
	Elisa_Chlorophyll_YFP_all %>% 
	mutate(newTechRepNames = case_when(
		Technical_replicate == "TechRep_1" ~ "A",
		Technical_replicate == "TechRep_2" ~ "B",
		Technical_replicate == "TechRep_3" ~ "C",
		Technical_replicate == "TechRep_4" ~ "D",
		Technical_replicate == "TechRep_5" ~ "E",
		Technical_replicate == "TechRep_6" ~ "F",
	)) %>%
	ggplot(aes(x = Construct, 
		   y = chlorophyll_pos, 
		   color = newTechRepNames)
	) + geom_jitter(size = 0.5) +
	scale_color_manual(values = colors,
			   name = "Technical\nReplicate") +
	scale_y_continuous(limits = c(0, 100),
			   breaks = seq(0, 100, 25)) +
	labs(x = "Strain", y = "Chlorophyll-positive cells (%)")

# YFP positive plot
plots[["YFP_all"]] =
	Elisa_Chlorophyll_YFP_all %>%  
	mutate(newTechRepNames = case_when(
		Technical_replicate == "TechRep_1" ~ "A",
		Technical_replicate == "TechRep_2" ~ "B",
		Technical_replicate == "TechRep_3" ~ "C",
		Technical_replicate == "TechRep_4" ~ "D",
		Technical_replicate == "TechRep_5" ~ "E",
		Technical_replicate == "TechRep_6" ~ "F",
	)) %>%
	ggplot(aes(x = Construct, 
		   y = YFP_pos, 
		   color = newTechRepNames)
	) + geom_jitter(size = 0.5) +
	scale_color_manual(values = colors,
			   name = "Technical\nReplicate") +
	scale_y_continuous(limits = c(0, 100),
			   breaks = seq(0, 100, 25)) +
	labs(x = "Strain", y = "YFP-positive cells (%)")


#### Anova ####

# Test if there is difference between the technical replicates
(anova_techReps_chlorophyll = 
	aov(chlorophyll_pos ~ Technical_replicate, Elisa_Chlorophyll_YFP_all))
# Check with summary if the pvalue < 0.05
summary(anova_techReps_chlorophyll)

# Run the post-hoc test Tukey
(tukey_techReps_chlorophyll = 
	anova_techReps_chlorophyll %>% 
   	TukeyHSD())
# Check which comparisons have adjusted p-value < 0.05
(which(tukey_techReps_chlorophyll$Technical_replicate[, 4] < 0.05))

#### Filtering ####
# TechReps 1-3 are different from 4-6. So we remove 1-3
filteredReps_chlorophyllYFP_data = filter(Elisa_Chlorophyll_YFP_all,
				      Technical_replicate %in% paste0("TechRep_", 4:6))

# Get the mean chlorophyll_pos for each clone
# Using group_by with summarise, we get a summary of the selected columns
# but separated by the groups specified in group_by
(mean_chlorophyllYFP_data = filteredReps_chlorophyllYFP_data %>%
	group_by(Clone) %>% 
	summarise(average.Chlorophyll = mean(chlorophyll_pos, na.rm = T),
		  Construct = Construct,
		  average.YFP.pos = mean(YFP_pos, na.rm = T)) %>%
	arrange(average.Chlorophyll) %>%
	mutate(orderedClones = factor(Clone, levels = unique(Clone))) %>% 
	unique())

# Let's see which clones have mean chlorophyll positive cells < chlorophyll_threshold
(clonesToDiscard = 
	filter(mean_chlorophyllYFP_data,
	       average.Chlorophyll < chlorophyll_threshold) %>%
	arrange(Construct))

# Now we remove them from the data
filteredChloro_chlorophyllYFP_data = filteredReps_chlorophyllYFP_data %>%
	dplyr::filter(!Clone %in% clonesToDiscard$Clone)

#### New plots ####
# Chlorophyll plot
plots[["chlorophyll_filtered"]] =
 	filteredChloro_chlorophyllYFP_data %>% 
 	mutate(newTechRepNames = case_when(
 		Technical_replicate == "TechRep_4" ~ "A",
 		Technical_replicate == "TechRep_5" ~ "B",
 		Technical_replicate == "TechRep_6" ~ "C",
 	)) %>%
 	ggplot(aes(x = Construct, 
 		   y = chlorophyll_pos, 
 		   color = newTechRepNames)
 	) + geom_jitter(size = 0.5) +
 	scale_color_manual(values = colors[4:6],
 			   name = "Technical\nReplicate") +
 	scale_y_continuous(limits = c(0, 100),
 			   breaks = seq(0, 100, 25)) +
 	labs(x = "Strain", y = "Chlorophyll-positive cells (%)")

# YFP positive plot
plots[["YFP_filtered"]] =
		filteredChloro_chlorophyllYFP_data %>% 
		mutate(newTechRepNames = case_when(
			Technical_replicate == "TechRep_4" ~ "A",
			Technical_replicate == "TechRep_5" ~ "B",
			Technical_replicate == "TechRep_6" ~ "C",
		)) %>%
		ggplot(aes(x = Construct, 
			   y = YFP_pos, 
			   color = newTechRepNames)
		) + geom_jitter(size = 0.5) +
		scale_color_manual(values = colors[4:6],
				   name = "Technical\nReplicate") +
		scale_y_continuous(limits = c(0, 100),
				   breaks = seq(0, 100, 25)) +
		labs(x = "Strain", y = "YFP-positive cells (%)")

# Arrange all plots together
ggarrange(plotlist = plots, nrow = 2, ncol = 2)

# Align the plots

ggarrange(plotlist = plots, nrow = 2, ncol = 2, vjust = T, hjust = T,
	  labels = "AUTO")
