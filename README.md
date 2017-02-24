# scidbst

"scidbst" is a package for R that bundles functions to manipulate spatio-temporal arrays in the database system SciDB (by Paradigm4). This package relies on the basic scidb operation that are provided in the "scidb" package also by Paradigm4 and extends it with the ability to maintain the spatial and/or temporal references that are annotated with the arrays by the scidb plugin scidb4geo. It also enables users to use spatial or temporal constructs as parameter in scidb functions in R.

## Dependencies
The package has several prerequisites for its usage. First, there are specific system setup requirements regarding the database system SciDB that operates in the background, and second, the package depends also on some other R packages.

### System
In the background the package needs a fully operating [SciDB database](http://www.paradigm4.com/try_scidb/) that runs the [scidb4geo plugin](https://github.com/appelmar/scidb4geo) by Marius Appel, the [SHIM client](https://github.com/Paradigm4/shim) and the [r_exec plugin](https://github.com/Paradigm4/r_exec/). 

To quickly set up a working environment, check out the [docker image](https://github.com/appelmar/scidb-eo) provided by Marius Appel.

### Packages in R
The package mainly depends on the following packages:

- scidb
- raster

If you are going to use some of the coerce functions, then the following packages are also of interest:

- spacetime
- xts
- sp

Make sure to install those dependent packages before 

## Getting started

In the following we are going to give simple examples, how to install and use the package. For more details about the use and availability of functions, please check the documentation files and the vignettes.

### Install

```R
devtools::install_gitub(repo="scidbst",username="flahn")
```

### Quickstart
The first and foremost thing to do is to establish a connection to SciDB using the function 'scidbconnect' of the scidb package.

```R
scidbconnect(host=host,port=port,user=user,password=password,protocol="https",auth_type = "digest")
```

After that we can start exploring the database with either `scidbls()` or `scidbst.ls()`. The latter will show all the arrays that are in SciDB that have a spatial / temporal or spatio-temporal reference attached. If you want to add arrays to the data base, please consider using our (GDAL driver for scidb)[https://github.com/appelmar/scidb4gdal].

To load a referenced array into R we use `scidbst("some_st_array")`, where "some_st_array" is the name of a spatio-temporal array that was listed by `scidbst.ls()`.

Similarly to the raster package you can use wellknown functions like *extent* or *crs* to access information about the references. The following code shows some basic functionalities:

```R
starr = scidbst("some_st_array")
extent(starr)
crs(starr)
textent(starr)
trs(starr)
```

A typical basic task in the geoscience domain is to create subsets (a particular scene on the earth) and slices (e.g. a scene at a particular time). 

```R
subset.extent = extent(35,36.5,6,8.5) # in lon/lat using WGS84
ethiopia.subset = crop(starr,subset.extent)

ethiopia.subset = slice(ethiopia.subset,"t","2003-07-21")
```

If you need to calculate values based on attributes, you can use the `transform()` method. Here is an example how to potentially calculate a NDVI based on a Landsat7 dataset.

```R
ls7 = scidbst("some_ls7_array")
ls7_calc = transform(ls7, ndvi = "(band4_avg - band3_avg) / (band4_avg + band3_avg)", mdvi = "(band8_avg - band3_avg) / (band8_avg + band3_avg)")
```

In order to store changes on the spatial/temporal or spatio-temporal scidbst object in SciDB, we use the function `scidbsteval`, which evaluates the cascaded operations in the SciDB cluster and stores it under a given name.

```R
ls7_calc = scidbsteval(ls7_calc, "ls7_ndvi_calc")
```

The mentioned functions will give you just a glimpse of the functionality of this package. For more elaborate analysis, we recommend to have a look at the function *r.apply*, which allows to execute custom R-Scripts on data chunks. Examples for this can be found in the provided R vignettes.

### Vignettes

1. [Introduction to scidbst](./inst/doc/using_scidbst.Rmd)
2. [Introduction to r.apply](./inst/doc/interfacing_R_EXEC_on_SciDB.Rmd)
3. [Spatial r.apply Use Case](./inst/doc/slope_aspect.Rmd)
4. [Spatio-temporal r.apply Use Case](./inst/doc/r_exec_trmm_linear_model.Rmd)

or check the vignettes in R using `browseVignettes(package="scidbst")`

## Authors & Contributors
- Florian Lahn
- Marius Appel

## License
The package is released under the GNU Affero General Public License (AGPL-3), since it heavily depends on Code written by Paradigm4 in context of SciDB that is also released under the AGPL-3 license. See [LICENSE.md](./LICENSE.md) for details.
