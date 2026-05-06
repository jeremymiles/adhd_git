## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to ggplot2 in loops
**Learning:** When generating multiple plots in a loop using ggplot2, using a static alt text description for all plots reduces accessibility for screen reader users, as they cannot distinguish between the different charts.
**Action:** Use `unique(df$column)` in the loop iterator and dynamically generate the `alt` text in `labs(alt = ...)` (e.g., using `paste()`) to reflect the specific iteration's data, ensuring each plot has a unique and descriptive alternative text.
