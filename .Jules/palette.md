## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-09-10 - Add Alt Text to Data Visualizations
**Learning:** Data visualizations generated with ggplot2 in R and Quarto do not inherently provide alternative text, making them completely inaccessible to screen reader users who cannot interpret the graphical output.
**Action:** Always add descriptive `alt` text to ggplot2 visualizations using `labs(alt = "...")`. For plots generated in loops or with dynamic data, ensure the alt text is dynamically generated (e.g., using `paste()`) to accurately reflect the specific content of each plot.
