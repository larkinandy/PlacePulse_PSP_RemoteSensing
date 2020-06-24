#  PlacePulse_PSP_RemoteSensing
 Comparison of remote sensing, PSPNet, and green screen scores in place pulse dataset

**Author:** [Andrew Larkin](https://www.linkedin.com/in/andrew-larkin-525ba3b5/) <br>
**Affiliation:** [Oregon State University, College of Public Health and Human Sciences](https://health.oregonstate.edu/) <br>
**Principal Investigator:** [Perry Hystad](https://health.oregonstate.edu/people/perry-hystad) <br>

**Summary** <br>
This is a comparison of different methodologies for measuring environmental composition and its relationship to participant perceptions in the MIT Place Pulse 2.0 dataset.  Measurement methodologies include:

- **Remote sensing measurements** - examples include NDVI, impervious surface area, air pollution
- **PSPNet labels** - labels include trees, roads, buildings, and paths
- **Green nature** - [percent green in an image](https://github.com/larkinandy/GSV_NDVI_Comparison) directly attributable to nature

**Repository Structure** <br>
Files are divided into three folders, with each folder corresponding to a unique stage of model development.

- **[datasets](./datasets)** - derived built environment estimates, summary statistics, and GIS shapefiles for Place Pulse image locations
- **[data preprocessing](./preprocessing)** - calculating built environment composition, downloading NDVI from the Google Earth Engine, and performing QA steps
- **[statistical analysis](./statistics)** - calculating summary statistics, creating regression models 

**External Links**
- **MIT Place Pulse (archived)** - https://www.media.mit.edu/projects/place-pulse-new/overview/
- **PSPNet with ADEK20 Weights** - https://arxiv.org/pdf/1608.05442.pdf
- **Green Screen Repository** - https://github.com/larkinandy/GSV_NDVI_Comparison
