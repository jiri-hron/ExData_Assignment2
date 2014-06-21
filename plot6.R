## Create a plot answering the question: 
## Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California. Which city has 
## seen greater changes over time in motor vehicle emissions?
## The function assumes that the archive is already downloaded and placed
## in the current working directory.
plot6 = function() {
    # ensure ggplot2 is present
    if(!require("ggplot2")) {
        stop("the ggplot2 package needs to be installed in order to run this");
    }
    
    # create the temporary directory used to extract the archive's content
    tmpDir = tempdir();
    on.exit(function() unlink(tmpDir, T, T));
    
    # unzip the archive to the temp dir
    zipped = file.path(getwd(), "exdata_data_NEI_data.zip")
    if(!file.exists(zipped)) {
        stop("the archive exdata_data_NEI_data.zip not present in current dir");
    }
    unzippedDir = file.path(tmpDir, "unzipped");
    dir.create(unzippedDir, showWarnings=F);
    message("unzipping file");
    unzip(zipfile=zipped,exdir=unzippedDir);
    
    # read the data
    message("extracting finished, reading the data, this may take a while ...");
    NEI <- readRDS(file.path(unzippedDir, "summarySCC_PM25.rds"));
    SCC <- readRDS(file.path(unzippedDir, "Source_Classification_Code.rds"));
    
    # subset only data covering Baltimore City
    la.and.balt.NEI = NEI[NEI$fips == "24510" | NEI$fips == "06037",];
    
    # pick all records related to vehicles; use categories containing
    # "transport" or "transportation" in names, assume most of means of
    # transport around Baltimore is based on combustion-engine vehicles
    vehicle.related = SCC[
        grepl("^(.*?\\s)?([tT]ransport(ation)?|[vV]ehicle)(\\s.*)?",
              levels(SCC$SCC.Level.Two)),];
    # get rid of the only irrelevant category to transportation
    vehicle.related = vehicle.related[vehicle.related$SCC.Level.Two 
                                      != "Transportation Equipment",];
    la.and.balt.NEI.vehicle =
        la.and.balt.NEI[la.and.balt.NEI$SCC %in% vehicle.related$SCC,];
    
    # add variable to label the towns by name
    la.and.balt.NEI.vehicle$town = "Baltimore City";
    la.and.balt.NEI.vehicle[
        la.and.balt.NEI.vehicle$fips == "06037",]$town = "Los Angeles";
    la.and.balt.NEI.vehicle$town = factor(la.and.balt.NEI.vehicle$town);
    
    # calculate the total number of tons emitted per year
    message("calculating the total volumes of emissions per year")
    emissions.by.year.town = aggregate(
        list(emissions = la.and.balt.NEI.vehicle$Emissions),
        by = list(town = la.and.balt.NEI.vehicle$town,
                  year = la.and.balt.NEI.vehicle$year),
        sum);
    
    # make a plot    
    g = ggplot(data = emissions.by.year.town,
               aes(x = year,
                   y = emissions,
                   colour = town));
    line = geom_line();
    x.lab = xlab("Year");
    y.lab = ylab(expression("Tons of"~PM[2.5]~"emitted"));
    title = ggtitle(
        expression(PM[2.5]~"caused by vehicles and transport: Baltimore vs LA"));
    g + line + x.lab + y.lab + title;
    
    ggsave("plot6.png", height = 6.4, width = 7.2, units = "in", dpi = 75);
}