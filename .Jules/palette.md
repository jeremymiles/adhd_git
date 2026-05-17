## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-05-14 - Add dynamic alt text to ggplot2 charts generated in loops
**Learning:** When generating multiple data visualizations (like `ggplot2` plots) within a loop, static alt text fails to describe the specific data being presented, reducing accessibility for screen reader users.
**Action:** Always dynamically generate the `alt` text in `labs(alt = ...)` (e.g., using `paste()`) to reflect the specific iteration's data instead of using a single static description.
