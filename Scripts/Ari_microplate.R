# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "ggpubr")
# We check which packages are NOT (!) installed
pkgs.To.Install = ! pkgs %in% installed.packages()
# any() checks if there is at least one TRUE in the vector
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 
# curPkg is a variable that takes the value of each element in pkgs
theme_set(theme_bw())


##### Load file #####
input = "Inputs/Ari_UVM4_Sup_OD.xlsx"
sheetsInput = excel_sheets(input)
measurementsSheets = grep("Sheet", sheetsInput, value = T)

longerTable = sapply(measurementsSheets, simplify = F, \(sheet) {
	read_excel(input, sheet = sheet) %>%
		rename("xCoord" = "...1") %>%
		pivot_longer(cols = all_of(2:13),
			     names_to = 'yCoord',
			     values_to = 'Measurements') %>%
		mutate(Day = gsub("Sheet", "", sheet))
}) %>% list_rbind

metadata = read_excel(input, sheet = "Metadata") %>%
	rename("xCoord" = "...1") %>%
	pivot_longer(cols = all_of(2:13),
		     names_to = 'yCoord',
		     values_to = 'WellName') %>%
	filter(!is.na(WellName))

completeTable = left_join(metadata, longerTable,
			  by = c("xCoord", "yCoord")) %>%
	separate(WellName, 
		 into = c("Supplementation", "Concentration", "Algae"),
		 sep = "-") 

ngvControl = completeTable %>%
	filter(Algae == "ngv") %>%
	group_by(Supplementation, Day) %>%
	summarise(Mean_OD = mean(Measurements, na.rm = T))

loopTable = ngvControl %>%
	dplyr::select(Supplementation, Day) %>%
	unique

normalisedData = sapply(1:nrow(loopTable),
			simplify = F,
			\(NROW) {
				suppl = loopTable$Supplementation[NROW]
				dayN = loopTable$Day[NROW]
				blank = ngvControl$Mean_OD[
					ngvControl$Supplementation == suppl &
						ngvControl$Day == dayN
				]
				completeTable %>%
					filter(Supplementation == suppl,
					       Day == dayN) %>%
					mutate(normalizedOD = Measurements - blank,
					       nonNeg_normalizedOD = ifelse(normalizedOD < 0,
					       			     0, normalizedOD))
			}) %>% list_rbind

mean_OD_Data = normalisedData %>%
	filter(Algae != "ngv") %>%
	group_by(Supplementation, Concentration, Day) %>%
	summarize(meanOD = mean(nonNeg_normalizedOD, na.rm = T),
		  sdOD = sd(nonNeg_normalizedOD, na.rm = T)) 

normalisedData %>%
	filter(Algae != "ngv",
	       Day %in% 0:9) %>%
	filter(Supplementation %in% c("Tyr", "none")) %>%
	mutate(Concentration_ordered = factor(
		Concentration,
		levels = normalisedData$Concentration %>% 
			as.numeric %>% 
			sort %>% 
			unique %>% as.character() 
	)) %>%
	ggplot(aes(x = Day %>% as.numeric,
		   y = log(nonNeg_normalizedOD + 1),
		   color = Concentration_ordered,
		   fill = Concentration_ordered)) +
	geom_smooth(linewidth = 1, alpha = 0.05) +
	labs(x = "Day", y = "log(OD)", 
	     color = "Concentration (\u03bcM)",
	     fill = "Concentration (\u03bcM)")



normalisedData %>%
	filter(Algae != "ngv",
	       Day %in% 0:9) %>%
	filter(Supplementation %in% c("Tys", "none")) %>%
	mutate(Concentration_ordered = factor(
		Concentration,
		levels = normalisedData$Concentration %>% 
			as.numeric %>% 
			sort %>% 
			unique %>% as.character() 
	)) %>%
	ggplot(aes(x = Day %>% as.numeric,
		   y = log(nonNeg_normalizedOD + 1),
		   color = Concentration_ordered,
		   fill = Concentration_ordered)) +
	geom_smooth(linewidth = 1, alpha = 0.1) +
	scale_colour_viridis_d() +
	scale_fill_viridis_d() +
	#coord_cartesian(ylim = c(0, 1.2)) +
	labs(x = "Day", y = "log(OD)", 
	     color = "Concentration (\u03bcM)",
	     fill = "Concentration (\u03bcM)")

#### Statistics ####
pkgs = c("multcomp", "emmeans", "lme4")
pkgs.To.Install = ! pkgs %in% installed.packages()
# any() checks if there is at least one TRUE in the vector
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 


dataForModel = with(normalisedData %>%
		    	filter(Algae != "ngv"),
		    data.frame(
		    	Day =  as.numeric(Day),
		    	Concentration = as.factor(Concentration),
		    	Supplementation = as.factor(Supplementation),
		    	normalizedOD = log(nonNeg_normalizedOD+0.01)
		    )
)


exponential.models = 
	# We will test for each supplementation if there is difference between the concentrations over time
	sapply(c("Tys", "Tyr"), simplify = F, 
	       \(metabolite) {
	       	filteredData = 
	       		filter(dataForModel,
	       		       Supplementation %in% 
	       		       	c(metabolite, "none")
	       		)
	       	
	       	model = with(filteredData, 
	       		     lmer(normalizedOD ~ 
	       		          	Day * Concentration * Supplementation + 
	       		          	(1|Day))
	       	)
	       	# Now, using the model created, we compare the concentrations
	       	emtrends(model, "Concentration", var = "Day",
	       		 at = list(Day = unique(filteredData$Day))
	       		 ) %>% pairs %>% summary
	       		 # We use pairs then summary to get the pvalue
	       })
	       	

exponential.models # To see the results

# Who is different?
exponential.models$Tys %>%
	filter(p.value < 0.05)

exponential.models$Tyr %>%
	filter(p.value < 0.05)
