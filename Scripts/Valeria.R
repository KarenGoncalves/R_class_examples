# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "multcomp")
pkgs.To.Install = ! pkgs %in% installed.packages()
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 

theme_set(theme_bw())

#### Set basic variables ####
# Set the name of the input file and
ValeriaFile = "Inputs/Valeria_microplateReadings.xlsx"
# colors = c("black", "blue", "forestgreen",
# 	   "red", "magenta", "orange")
sheetsValeria = excel_sheets(ValeriaFile)
 
#### Read input and pivot longer ####
input = read_excel(ValeriaFile, sheet = sheetsValeria)
longFormat = input |>
	pivot_longer(cols = !Measurement,
		     # The column names will be split and stored in 4 new columns
		     names_to = c("Sample_name", 
		     	     "Elution", 
		     	     "Dilution", 
		     	     "Technical_rep"),
		     # Where to split the column names
		     names_sep =  "_", 
		     names_transform = 
		     	list("Sample_name" = as.factor, 
		     	     "Elution" = as.character, 
		     	     "Dilution" = as.factor, 
		     	     "Technical_rep" = as.character),
		     values_to = "Measure",
		     ) |>
	mutate(correctElution = 
	       	ifelse(Elution == "2E",
	       	       "Elution 2", "Elution 1") %>%
	       	as.factor) 


#### Join the data from different technical replicates ####
longFormat_meanTechReps = longFormat |>
	group_by(Sample_name, correctElution, Dilution, Technical_rep) |>
	summarise(meanTechRep = mean(Measure, na.rm = T),
		  sdTechRep = sd(Measure, na.rm = T))
# Plot
longFormat_meanTechReps |>
	ggplot(aes(x = Sample_name)) +
	geom_boxplot(aes(y = meanTechRep)) +
	# facet_wrap splits the data according to the values in the column correctElution
	facet_wrap(~correctElution, nrow = 2) +
	scale_y_continuous(limits = c(0.6, 0.8)) +
	labs(y = "[Pi] (nM)",
	     x = "") +
	theme_bw()

#### Split by elution ####
splitted.DataElution = 
	sapply(c("Elution 2", "Elution 1"), simplify = F,
	     \(elution) {
	     	longFormat_meanTechReps |>
	     		filter(correctElution == elution)
	     })

# We use the function mcp to specify the post-hoc test 
linearModel = mcp(Sample_name = "Tukey")

cldCode = sapply(names(splitted.DataElution), simplify=F, \(d) {
	result = splitted.DataElution[[d]] |>
		aov(formula = meanTechRep ~ Sample_name) |>
		# the function below runs the post-hoc analysis
		glht(linfct = linearModel) |>
		# This one gives the letters for the plot
		cld()
	
	# the letters are saved in the object mcletters$Letters of the result
	finalResult = result$mcletters$Letters
	
	data.frame(Sample_name = names(result$mcletters$Letters),
		   cldCode = unname(result$mcletters$Letters),
		   correctElution = d)
}) |> do.call(what = "rbind")


# Now we join the cldCode and the longFormat_meanTechReps tables
analysed_meanTechReps =
	left_join(longFormat_meanTechReps,
	  cldCode, by = c("Sample_name", "correctElution"))

cldCode = 
	analysed_meanTechReps[, c("Sample_name", "correctElution", 
				  "meanTechRep", "cldCode")] |> 
	group_by(Sample_name, correctElution, cldCode) |>
	summarize(y = max(meanTechRep)) 

# We specify the order of the samples for the plot
analysed_meanTechReps$Sample_name = 
	factor(analysed_meanTechReps$Sample_name,
	       levels = c("Control", "C1.1", "C1.2")
	)

# and we repeat the plot
analysed_meanTechReps |>
	ggplot(aes(x = Sample_name)) +
	geom_boxplot(aes(y = meanTechRep), 
		     outlier.colour = "red") +
	# The letters must go at the top of each box
	geom_text(data = cldCode,
		  aes(x = Sample_name,
		      y = y + .02,
		      label = cldCode),
		  size = 4
	) +
	facet_wrap(~correctElution, nrow = 2) +
	scale_y_continuous(limits = c(0.6, 0.8)) +
	labs(y = "[Pi] (nM)",
	     x = "") +
	theme_bw()
ggsave("Plots/Valeria_Treatment_by_elution.pdf",
       height = 4.6, width = 2.7)
