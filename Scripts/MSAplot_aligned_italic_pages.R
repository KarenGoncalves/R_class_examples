# Set your variables here
fasta_path = "https://karengoncalves.github.io/Programming_classes/exampleData/Reductases.fasta"
# set parse_text to F if you do not need to italicize your seqnames
parse_text = T
alignment = list()
interval_size = 30  # change the interval (last value) depending on the size of your alignment
plot_msa_tiff = "Plots/reductases_alignment.tiff"
plot_msa_pdf = "Plots/reductases_alignment.pdf"

install.packages("devtools")
devtools::source_gist("https://gist.github.com/KarenGoncalves/0db105bceff4ff69547ee25460dda978")

install_from_dif_sources(
	cran_packages = c("tidyverse", "ggpubr"),
	bioconductor_packages = c("Biostrings", "msa",
				  "treeio", "ggtree",
				  "ape", "seqinr", "phangorn"),
	github_packages = "YuLab-SMU/ggmsa"
)

# To open a fasta file with multiple sequences, use the function readDNAStringSet() or readAAStringSet()
fasta_input = fasta_path %>%
	readAAStringSet(format = "fasta")
head(fasta_input)
fasta_for_alignment <- fasta_input[1:10]

# use ?msa to know what you need to put in the function and what the default values are
myFirstAlignment <- msa(fasta_for_alignment, 
			method = "ClustalOmega",
			verbose = T
)

print(myFirstAlignment, show = "complete", showConsensus = T)
class(myFirstAlignment) <- "AAMultipleAlignment"

# If we want to italicize part of the text in the alignment plot
# we use the variable Seqnames
# Remember that in this case the protein name comes first and is
# separated by the species name with a space
Seqnames <- 
	gsub("^([A-Za-z0-9_\\.]+) (.+)", 
	     "'\\1 '~italic('\\2')",
	     myFirstAlignment@unmasked@ranges@NAMES)
# Here I do not want to plot many pieces of the MSA
# so uncomment the line below and comment the one below it for your case
#max_size = myFirstAlignment@unmasked@ranges@width[1]; start_point = 1	
max_size = 290; start_point = 151
intervals = seq(start_point, max_size, interval_size)

for (i in 1:length(intervals)) { 
	# The lines below prevent R from continuing the loop
	# if have gone through the whole length of the alignment
	if (i == length(intervals) & 
	    intervals[i] == max_size) {
		break
	}
	start = intervals[i];
	# The ifelse checks if i is already the last value in intervals
	# if not, we set the end position of the plot as the next starting value - 1
	# If it  the last value, we set the end position as the maximum size
	end = ifelse(i < length(intervals),
		     intervals[i+1] - 1,
		     max_size)
	
	alignment[[i]] = 
		ggmsa(myFirstAlignment, 
		      start = start, end = end,
		      seq_name = T, char_width = 0.5,
		      consensus_views = T) +
		scale_x_continuous(breaks = seq(start + 9, 
						end, 10),
		# by setting the limits, we ensure that the pieces will align later
				   limits = c(start - .9, 
				   	   start + interval_size)) +
		theme(axis.text.x = element_text(size = 8, hjust = 0))
	if (parse_text) {
		# Add the new labels to the alignment
		alignment[[i]] = 
			alignment[[i]] +
			scale_y_discrete(labels = c(parse(text = Seqnames),
						    "Consensus")
			)
	}
}


# Plot in one page with ggpubr::ggarrange or cowplot::plot_grid or patchwork::wrap_plots
library(ggpubr)
plot_ = ggarrange(plotlist = alignment,
		  align = "hv",
		  hjust = 0,
		  nrow = length(alignment))

ggsave(plot = plot_,
       plot_msa_tiff, dpi = 1200,
       height = 10, width = 7, units = "in")

# or in one pdf with multiple pages 
pdf(plot_msa_pdf)
for (i in alignment) print(i)
dev.off()
