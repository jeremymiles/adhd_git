## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-04-19 - Dynamic Alt Text in Generated Plots
**Learning:** Generating multiple charts inside a loop without dynamic `alt` text provides a poor experience for screen reader users as all images are either announced with the same generic alt text or read without any useful context, making it hard to distinguish charts.
**Action:** Always inject dynamically generated `alt` text into generated charts within iterative loops, for example using `labs(alt = paste("Scatter plot for", name_1))` in `ggplot2`.
