## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text in loops
**Learning:** Generating multiple ggplot2 visualizations inside a loop without dynamic alt text results in identical, uninformative screen reader text for all plots, severely hindering accessibility.
**Action:** Always dynamically generate the 'alt' text inside the 'labs()' function (e.g., using paste()) when generating plots in a loop to ensure each plot is uniquely described for screen readers.
