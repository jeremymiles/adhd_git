## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-10-25 - Dynamic Alt Text for Loop Visualizations
**Learning:** When generating multiple visualizations in a loop, static alt text fails to provide screen reader users with necessary context about the specific data being presented in each iteration.
**Action:** Always use dynamic alt text (e.g., via `paste()` in `labs(alt=...)`) to include the loop variable (like the specific tool name) so each plot is uniquely identifiable and accessible.
