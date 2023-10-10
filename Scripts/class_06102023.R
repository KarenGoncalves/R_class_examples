#### Install and load the packages ####
pkgs = c("tidyverse", "readxl")
pkgs.To.Install = ! pkgs %in% installed.packages()
if (any(pkgs.To.Install)) {
	install.packages(pkgs[pkgs.To.Install])
}

for (curPkg in pkgs) {
	library(curPkg, character.only = T) 
}

#####

theme_bw() %>%
	theme_set()

# Use different colors to fill and remove the legend
colors = c("red", "green", "blue")

ggplot(aes(x = Species,
	   y = Sepal.Length,
	   fill = Species),
       data = iris) +
	geom_violin(aes(alpha = Species),
		     show.legend = F#, 
		     #outlier.alpha = 0
		    ) + 
	#geom_jitter() +
	# the type of plot you want
	ylab("Sepal length (mm)") +
	xlab("Species epithet") +
	scale_fill_manual(values = colors,
			  breaks = c("virginica", "setosa", 
			  	   "versicolor")) +
	scale_alpha_manual(values = c(1, 1, 0.5)) +
	scale_y_continuous(breaks = seq(0, 10, 2),
			   limits = c(0, 8))

