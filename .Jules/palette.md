## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic accessible alt text for dynamically generated plots
**Learning:** When generating multiple plots in a loop, it's critical to avoid static alt text for accessibility; dynamically constructed strings (e.g. `alt = paste(...)`) allow each visualization to be meaningful and distinct to screen readers.
**Action:** Always dynamically generate the `alt` parameter using string functions like `paste()` or `glue::glue()` within visualization commands inside loops.
