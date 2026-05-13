## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in ggplot2 Loops
**Learning:** When rendering plots inside a loop, static alt text limits accessibility as it fails to describe the specific data shown. Additionally, iterating over duplicate values causes redundant rendering which degrades document performance and user experience.
**Action:** Wrap loop iterators with unique() and use paste() within labs(alt = ...) to dynamically generate descriptive alternative text for each plot.
