## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-18 - Dynamic Alt Text in ggplot2 loops
**Learning:** When generating multiple `ggplot2` visualizations inside a loop, static alt text fails to describe the specific data being presented in each iteration, which degrades the experience for screen reader users.
**Action:** Use dynamic alt text generation via `labs(alt = paste(...))` to ensure each plot's alternative text accurately reflects the unique context or data subset of that specific iteration.
