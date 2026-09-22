## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-24 - Dynamic Alt Text in ggplot2 Loops
**Learning:** When generating multiple ggplot2 visualizations within loops, static alt text descriptions become repetitive and fail to convey the specific context of each chart to screen reader users.
**Action:** Always dynamically generate screen reader alt text for loop-generated ggplot2 plots using `labs(alt = paste(...))` to ensure each chart has a unique, descriptive label.
