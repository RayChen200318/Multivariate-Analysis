rm(list = ls())
critical_values <- seq(9, 14)
p1c2 <- pnorm(critical_values, mean = 14, sd = 2)
p2c1 <- 1 - pnorm(critical_values, mean = 10, sd = 2)
p1a2 <- p1c2 * 0.5
p2a1 <- p2c1 * 0.5
TPM <- p1a2 + p2a1
ECM.a <- TPM * 10
ECM.b <- p1a2 * 5 + p2a1 * 15
results <- data.frame(
  "Critical_Value" = critical_values,
  "P(1|2)" = p1c2,
  "P(2|1)" = p2c1,
  "P(1,2)" = p1a2,
  "P(2,1)" = p2a1,
  "TPM" = TPM,
  "ECM(a)" = ECM.a,
  "ECM(b)" = ECM.b
)
results <- round(results, 4)
library(dplyr)
library(kableExtra)
latex_table <- kable(results, format = "latex", booktabs = TRUE, caption = "Table of Classification Analysis") %>%
  kable_styling(latex_options = c("striped", "scale_down"))
print(latex_table)
