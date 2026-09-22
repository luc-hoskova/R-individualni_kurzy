# HISTOGRAM COMPARISON: OBSERVATION COUNTS

# Load and prepare the newborn data.
dataNewborns <- read.delim("newborns.txt")
dataNewborns$edu.M <- factor(dataNewborns$edu.M,
	labels = c("Grade school", "Vocational school", "High school", "University")
)
dataNewborns$sex.C <- factor(dataNewborns$sex.C,
	levels = c("f", "m"), labels = c("Female", "Male")
)
dataNewborns$prch.N <- as.numeric(as.character(dataNewborns$prch.N))
dataNewborns$weight.C <- as.numeric(as.character(dataNewborns$weight.C))
dataNewborns <- dataNewborns[
	is.finite(dataNewborns$weight.C) & is.finite(dataNewborns$prch.N),
]

histogram_breaks <- seq(
	floor(min(dataNewborns$weight.C) / 100) * 100,
	ceiling(max(dataNewborns$weight.C) / 100) * 100,
	by = 100
)
histogram_colours <- c(Female = "#D1495B", Male = "#00798C")
female_weights <- dataNewborns$weight.C[dataNewborns$sex.C == "Female"]
male_weights <- dataNewborns$weight.C[dataNewborns$sex.C == "Male"]

# Base R: counts, colours, transparency and legend are manual.
female_histogram <- hist(female_weights,
	breaks = histogram_breaks, plot = FALSE
)
male_histogram <- hist(male_weights,
	breaks = histogram_breaks, plot = FALSE
)
histogram_y_max <- max(female_histogram$counts, male_histogram$counts)

hist(female_weights,
	breaks = histogram_breaks,
	col = adjustcolor(histogram_colours["Female"], alpha.f = 0.50),
	border = "white",
	xlim = range(dataNewborns$weight.C),
	ylim = c(0, histogram_y_max * 1.10),
	xlab = "Birth weight (g)",
	ylab = "Number of observations",
	main = "Birth weight distribution by sex",
	sub = "Base R histogram: observation counts"
)
hist(male_weights,
	breaks = histogram_breaks, add = TRUE,
	col = adjustcolor(histogram_colours["Male"], alpha.f = 0.50),
	border = "white"
)
abline(v = mean(dataNewborns$weight.C), col = "grey35", lty = 3, lwd = 2)
legend("topright",
	legend = c("Female", "Male", "Overall mean"),
	fill = c(adjustcolor(histogram_colours, alpha.f = 0.50), NA),
	border = c(histogram_colours, NA),
	col = c(histogram_colours["Female"], histogram_colours["Male"], "grey35"),
	lty = c(NA, NA, 3), lwd = c(1, 1, 2),
	bty = "n", cex = 0.8
)

# ggplot2: counts are the default y-aesthetic for geom_histogram().
library(ggplot2)
ggplot(dataNewborns, aes(x = weight.C, fill = sex.C)) +
	geom_histogram(binwidth = 100, boundary = 0,
		position = "identity", alpha = 0.50,
		colour = "white") +
	geom_vline(aes(xintercept = mean(dataNewborns$weight.C),
		linetype = "Overall mean"), colour = "grey35", linewidth = 0.8) +
	scale_fill_manual(values = histogram_colours) +
	scale_linetype_manual(values = c("Overall mean" = "dashed"),
		name = NULL) +
	labs(
		title = "Birth weight distribution by sex",
		subtitle = "ggplot2 histogram: observation counts",
		x = "Birth weight (g)", y = "Number of observations", fill = "Sex"
	) +
	theme_minimal()
