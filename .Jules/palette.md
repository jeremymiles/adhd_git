## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to ggplot2 charts generated in loops
**Learning:** When generating multiple data visualizations (like `ggplot2` plots) within a loop, using a single static alt text description for all plots fails to describe the specific data shown in each iteration, reducing accessibility for screen reader users.
**Action:** Always dynamically generate the `alt` text in `labs(alt = ...)` (e.g., using `paste()` with loop variables) to ensure the alternative text accurately reflects the specific iteration's data and context.
