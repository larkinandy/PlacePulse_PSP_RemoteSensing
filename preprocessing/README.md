## Data Preprocessing
 Calculating built environment composition, downloading NDVI from the Google Earth Engine, and performing QA steps


 **files** <br>
 - scripts for deriving remote sensing and GIS variables were [previously published](https://github.com/larkinandy/LUR-NO2-Model)
- **[ImgFeatures.py](./ImgFeatures.py)** - custom class which calculates joint count and green screen estimates
- **[pp_constants.py](./pp_constants.py)** - security-sensitive constants, such as filepaths to data directories
- **[PP_greenScreen.py](./PP_GreenScreen.ipynb)** - Jupyter notebook to implement ImgFeatures class and calculate joint count and green screen estimates

**External Links**
- **Join Count Statistics** - https://www.youtube.com/watch?v=BdsdYEbUkj4
- **Green Screen Github Repo** - https://github.com/larkinandy/GSV_NDVI_Comparison
