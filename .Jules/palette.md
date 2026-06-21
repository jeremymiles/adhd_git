## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-06-21 - Dynamic alt text for looped visualizations
**Learning:** When generating multiple plots in a loop (e.g., with ggplot2 in R/Quarto), static alt text causes screen readers to announce the same description repeatedly, removing context and confusing users.
**Action:** Always dynamically generate alt text within labs(alt = ...) using functions like paste() to ensure each visualization has a unique, descriptive label based on the loop's current variable.
