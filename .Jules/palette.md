## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Use dynamic alt text for looped plots in ggplot2
**Learning:** Hardcoded alt text or no alt text on images created by `ggplot` inside a loop will either be repetitively unhelpful or simply absent, significantly hindering screen reader accessibility.
**Action:** When creating plots inside loops, dynamically generate the alternative text within `labs(alt = ...)` (e.g. using `paste()`) to provide specific, contextual information for each plot variant.
