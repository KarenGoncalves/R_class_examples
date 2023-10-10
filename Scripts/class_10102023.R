# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "car")
pkgs.To.Install = ! pkgs %in% installed.packages()
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 



# Set a theme for all future plots in this session
theme_set(theme_bw()) 

Snehi_rawData = read_excel("Inputs/Snehi.xlsx")

Snehi_newColumns = 
	Snehi_rawData %>% # |>
	mutate(
		SampleLine = gsub(x = Sample,
				 pattern = "(.+)(\\d)$",
				 replacement = "\\1"),
		Replicate = gsub(x = Sample,
				 pattern = "(.+)(\\d)$",
				 replacement = "\\2")
	)

Snehi_longFormat = 
	Snehi_newColumns |> # %>%
	pivot_longer(cols = any_of(paste0("Leaf", 1:4)),
		     names_to = "Leaf_number",
		     values_to = "Spore_count"
		     )
Snehi_meanPerSample =
	Snehi_longFormat |>
	group_by(SampleLine, Leaf_number) |>
	summarise(
		MeanSporeCount = mean(Spore_count, 
				      na.rm = T),
		StdSporeCount = sd(Spore_count, 
				   na.rm = T)
	)

Snehi_meanPerSample |>
	ggplot(aes(SampleLine, 
		   MeanSporeCount)) +
	geom_point() +
	geom_errorbar(aes(
		ymin = MeanSporeCount - StdSporeCount,
		ymax = MeanSporeCount + StdSporeCount
	))


Snehi_meanPerSample_Leafs =
	Snehi_longFormat |>
	group_by(SampleLine) |>
	summarise(
		MeanSporeCount = mean(Spore_count, 
				      na.rm = T),
		StdSporeCount = sd(Spore_count, 
				   na.rm = T)
	)

Snehi_meanPerSample_Leafs |>
	ggplot(aes(SampleLine, 
		   MeanSporeCount)) +
	geom_col() +
	geom_errorbar(aes(
		ymin = MeanSporeCount - StdSporeCount,
		ymax = MeanSporeCount + StdSporeCount
	), width = 0.2)


Snehi_longFormat |>
	ggplot(aes(SampleLine, 
		   Spore_count)) +
	geom_violin() #+
	#geom_boxplot(notch = T)


Snehi_longFormat |> 
	pivot_wider(id_cols = all_of(c("Sample", 
				       "SampleLine", 
				       "Replicate")
				     ), 
		    names_from = "Leaf_number", 
		    values_from = "Spore_count"
		    )

Snehi_longFormat %>%
	leveneTest(y = Spore_count ~ SampleLine)


anovaResult_1fct = 
	aov(Spore_count ~ SampleLine,
	    data = Snehi_longFormat)
summary(anovaResult_1fct)

which(TukeyHSD(anovaResult_1fct)$SampleLine[,4] < 0.05)


anovaResult_2fct = 
	aov(Spore_count ~ SampleLine * Replicate,
	    data = Snehi_longFormat)
summary(anovaResult_2fct)

which(
	TukeyHSD(anovaResult_2fct)$`SampleLine:Replicate`[,4]
	< 0.05
)
