## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-11-20 - Adding Dynamic Alt Text inside ggplot Loops
**Learning:** Screen readers and accessibility tools require distinct alternative text for dynamically generated plots, especially when looping over multiple variables or categories to create similar charts. A static description is insufficient and can be confusing.
**Action:** When creating plots within loops (e.g., using `ggplot2`), dynamically generate the `alt` text in the `labs(alt = ...)` function by interpolating the loop's iteration variable (e.g., using `paste()` or `glue()`) to ensure each plot's context is uniquely described.
