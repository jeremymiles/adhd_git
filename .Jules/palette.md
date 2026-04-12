## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in Generated Plots
**Learning:** When generating plots inside a loop, static alt text fails to accurately describe each unique image to screen readers.
**Action:** Always use dynamic text functions (e.g., `paste()`) in `labs(alt = ...)` to generate descriptive alt text that reflects the specific iteration's data or focus.
