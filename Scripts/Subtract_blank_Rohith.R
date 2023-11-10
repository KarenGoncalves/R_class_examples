## This script shows how to:
# import multiple sheets from a spreadsheet and join them into one table
# remove blank values from the data
# create a boxplot

# download input file from :
# https://github.com/KarenGoncalves/R_class_examples/blob/main/Inputs/Rohith.xlsx

# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "multcomp")

# We check which packages are NOT (!) installed
pkgs.To.Install = pkgs [! pkgs %in% installed.packages()]
#if there are any packages in pkgs.To.Install, we run the function
if (length(pkgs.To.Install) > 0)  install.packages(pkgs.To.Install)

for (curPkg in pkgs) library(curPkg, character.only = T) 


#### Load input file ####
inputFile = "Inputs/Rohith.xlsx"
sheetsInput = excel_sheets(inputFile)

input = sapply(sheetsInput, simplify = F, \(sheet) {
	read_excel(inputFile, sheet) %>%
		pivot_longer(
			cols = everything(),
			names_to = "Genotypes",
			values_to = "GFP_fluorescence"
		) %>%
		mutate(Date = sheet %>% 
		       	gsub(pattern = "^(\\d+)-(\\d+)-(\\d+)", 
		       	     replacement = "\\3-\\2-\\1") %>%
		       	as_datetime(),
		       Genotype_new = 
		       	case_when(
		       		Genotypes == "control" ~ "Control",
		       		Genotypes == "col" ~ "Col-0",
		       		Genotypes == "nup136" ~ "nup136/nup1",
		       		.default = Genotypes
		       	)
		)
}) %>% list_rbind 

input$Genotype_new = factor(input$Genotype_new,
			    levels = input$Genotype_new %>% unique
)

control_fluorescence =
	input %>%
	filter(Genotype_new == "Control") %>%
	group_by(Genotype_new, Date) %>%
	summarize(mean_fluorescence = mean(GFP_fluorescence, na.rm = T),
		  sdFluorescence = sd(GFP_fluorescence, na.rm = T)
	)



normalized_fluorescence = 
	sapply(simplify = F, unique(control_fluorescence$Date), 
	       \(day) {
	       	controlDay = control_fluorescence$mean_fluorescence[
	       		control_fluorescence$Date == day]
	       	
	       	input %>%
	       		filter(Date == day) %>%
	       		mutate(Normalized_fluorescence =
	       		       	GFP_fluorescence - controlDay,
	       		       Non_negative_normFluorescence =
	       		       	ifelse(Normalized_fluorescence < 0,
	       		       	       0, Normalized_fluorescence
	       		       	)
	       		)
	       }) %>% list_rbind %>% 
	filter(Genotype_new != "Control") 



means_fluorescence =
	normalized_fluorescence %>%
	group_by(Genotype_new) %>%
	summarize(mean_fluorescence = mean(Normalized_fluorescence, na.rm = T),
		  sdFluorescence = sd(Normalized_fluorescence, na.rm = T)
	) 


#### GRAPHS!!!!!!! ####

normalized_fluorescence %>%
	ggplot(aes(x = Genotype_new, y = Normalized_fluorescence,
		   color = Genotype_new)) +
	geom_boxplot(notch = F, 
		     outlier.shape = NA,
		     show.legend = F) +
	geom_jitter(width = .2, show.legend = F) +
	labs(y = "GFP fluorescence",
	     x = "Genotype") +
	# Zoom into area of the plot with coord_cartesian
	coord_cartesian(ylim = c(0, 1.5e+05)) +
	theme_classic()