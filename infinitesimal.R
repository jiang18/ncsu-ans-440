# Set random seed for reproducibility
set.seed(123)

# Function to simulate genotypes with specified frequencies and calculate genotypic values
simulate_and_calculate <- function(n_individuals, m_loci) {
  # 1) Simulate genotypes with specified frequencies
  genotypes <- matrix(sample(c(-1, 0, 1), n_individuals * m_loci, 
                             replace = TRUE, 
                             prob = c(0.25, 0.5, 0.25)),
                      nrow = n_individuals, ncol = m_loci)
  
  # 2) Fix each locus' effect as 1
  locus_effects <- rep(1, m_loci)
  
  # 3) Calculate genotypic values
  genotypic_values <- genotypes %*% locus_effects
  
  return(list(genotypic_values = genotypic_values, genotypes = genotypes))
}

# Set parameters
n_individuals <- 1000
m_loci <- 10

# Run simulation and calculation
result <- simulate_and_calculate(n_individuals, m_loci)
genotypic_values <- result$genotypic_values
genotypes <- result$genotypes

# 4) Draw histogram
hist(genotypic_values, 
     main = "Distribution of Genotypic Values",
     xlab = "Genotypic Value",
     ylab = "Frequency",
     col = "skyblue",
     border = "white",
     breaks = 30)

# Add a density curve
lines(density(genotypic_values), col = "red", lwd = 2)

# Add summary statistics to the plot
summary_stats <- summary(genotypic_values)
legend("topright", 
       legend = c(
         paste("Mean:", round(mean(genotypic_values), 2)),
         paste("SD:", round(sd(genotypic_values), 2)),
         paste("Min:", round(min(genotypic_values), 2)),
         paste("Max:", round(max(genotypic_values), 2))
       ),
       bty = "n")

# Print summary statistics
print(summary(genotypic_values))

# Check the actual frequencies of genotypes
genotype_freq <- table(genotypes) / (n_individuals * m_loci)
print("Actual genotype frequencies:")
print(genotype_freq)
