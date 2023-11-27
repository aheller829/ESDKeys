\name{KeyPlots}
\alias{KeyPlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Key Monitoring Plots to Ecological Sites
%%  ~~function to do ... ~~
}
\description{Pull and tabulate monitoring plot data from a TerrADat database, and assign plots to ecological sites based on the proportion of ecological site criteria met. Criteria are defined from an ecological site key built by the the function ESDKeys::SiteKeyBuild(). Monitoring plots can be subset by a user-defined polygon (e.g., MLRA or administrative boundary) or by geographic state. To download a geodatabase of MLRAs to use, visit https://www.nrcs.usda.gov/resources/data-and-reports/major-land-resource-area-mlra. There is an option to weight ecological site criteria by assigning each criteria to one of three classes, which will add weight scalars to the calculation of propertion of criteria met. There is an option to compare the top five species with the highest foliar cover recorded during the line-point intercept method to the species listed in the ecological site key output. Note that not all criteria included in the ecological site key output from ESDKeys::SiteKeyBuild() are calculable from the monitoring plot data. The criteria that can be evaluated against plot data are average annual precipitation, elevation, particle size class (PSC), slope, soil depth, species, surface texture, soil surface fragments, and soil subsurface fragments.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
KeyPlots(
  source.dsn = NULL
  mlra = NULL,
  sitekey = NULL,
  keypolyset = FALSE,
  shapefile = NULL,
  keystateset = TRUE,
  keystate = NULL,
  weights = FALSE,
  species = FALSE,
  lpi.tall = NULL
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source.dsn}{Character string. The filepath and filename to a TerrADat geodatabase.
}
  \item{mlra}{Character string. Specifies the MLRA of the site key used. Use the standard MLRA symbol convention, as is used in the EDIT database.
}
  \item{sitekey}{Character string. The name of the ecological site key in the environment created by KeyBuild(). If SiteKeyBuild() has just been named, this will be SiteKey.
}
  \item{keypolyset}{True/False. If FALSE, monitoring data will be subset by state only.
}
  \item{shapefile}{Character string. The filepath and filename to a shapefile, if using.
}
  \item{keystateset}{True/False. If TRUE, monitoring data will be subset by state. If FALSE, a polygon must be provided to subset monitoring data.
}
  \item{keystate}{Character string. If keystateset = TRUE, list state of interest using conventional two-letter state abbreviation.
}
  \item{weights}{True/False. If FALSE, all criteria are weighted equally. If TRUE, weight scalars will be used when considering proportion of criteria met and criteria must be assigned to weight classes one, two, and three by defining vectors named one, two, and three, containing the ecological site criteria from the "Property" column of the site key. Weight class one should contain the "least important" criteria and weight class three should contain the "most important" criteria. Properties that are not assigned will default to weight class one. When assigning soil rock fragment properties to weight classes, use "SurfaceFrags" and "SubsurfFrags" rather than "SubsurfGravel", "SubsurfLGFrags", "SurfaceGravel", and "SurfaceLGFrags" as are used in the output of ESDKeys::SiteKeyBuild(), as monitoring plot data do not consistently have fragments broken out by size classes.
}
  \item{species}{True/False. If species = TRUE, a list of the top five dominant plant species detected during the line-point intercept (LPI) method will be compared against the list of plant species from the site key (this is pulled from the community composition table in the ecological site description, where available). The criteria will be met if there are any matches of individual species between the plot data and the site key species list. If species = TRUE, user must provide an LPI tall table stored locally as an RDS or csv file.
}
  \item{lpi.tall}{Character string. The name of the tall lpi table in the environment. An LPI tall table can be built from a TerrADat geodatabase with the R package terradactyl (terradactyl::gather_lpi()).
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Two outputs are produced: 1) a .csv of monitoring plots, the ecological site for which each plot matched the most criteria, and the proportion of criteria met; and 2) a dataframe of monitoring plots, the ecological site to which each plot matched the most criteria, and all criteria with plot values and relational statements.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# Example using an MLRA polygon to subset monitoring plots, no weights, and no species comparison
KeyPlots(
  source.dsn = "Path/To/TerrADat.gdb",
  mlra = "036X",
  sitekey = "SiteKey",
  keypolyset = TRUE,
  shapefile = "Path/To/Shapefile.shp",
  keystateset = FALSE,
  keystate = NULL,
  weights = FALSE,
  species = FALSE,
  lpi.tall = NULL

)



# Example using a state name to subset monitoring plots, defining weight classes, and comparing species
# Weight classes
one <- c("AvgPrecip", "Elevation")
two <- c("PSC", "SoilDepth", "Slope")
three <- c("SurfaceTextures", "SurfaceFragments", "SubsurfFragments")

KeyPlots(
  source.dsn = "Path/To/TerrADat.gdb",
  mlra = "036X",
  sitekey = "SiteKey",
  keypolyset = FALSE,
  shapefile = NULL,
  keystateset = TRUE,
  keystate = "NM"
  weights = TRUE,
  species = TRUE,
  lpi.tall = "lpi.tall"
)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
