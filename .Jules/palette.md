## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-06-26 - Add dynamic alt text to generated plots in loops
**Learning:** When generating multiple ggplot2 visualizations within a loop, using static descriptions or omitting alt text makes the output inaccessible to screen readers. Additionally, looping over duplicate rows wastes processing time.
**Action:** Always dynamically generate alt text within the labs() function using paste() for dynamically created plots, and use unique() in loop iterators.
