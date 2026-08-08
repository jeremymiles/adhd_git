## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in Plots
**Learning:** When generating multiple plots in a loop (e.g., using `ggplot2`), it's essential to dynamically generate the `alt` text to accurately describe the specific data shown in each plot. Hardcoding a static string results in repetitive and less informative alternative text, hindering accessibility for screen reader users.
**Action:** Always use dynamic string construction (like `paste()`) to incorporate iteration variables or specific data characteristics into the `alt` text when generating plots programmatically.
