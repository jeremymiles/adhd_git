## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Add dynamic alt text to generated plots in loops
**Learning:** When generating multiple plots (e.g., scatter plots) in a loop using `ggplot2` in R/Quarto, static alt text fails to differentiate the plots for screen reader users, reducing accessibility.
**Action:** Always add dynamic alt text to generated plots in loops using the `labs(alt = ...)` function and `paste()` to combine static text with loop variables (e.g., `labs(alt = paste("Plot for", variable_name))`) to ensure each plot has descriptive and unique alt text.
