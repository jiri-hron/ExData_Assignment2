## Create a plot using the ggplot2 plotting system answering the question: 
## Of the four types of sources indicated by the type (point, nonpoint, onroad,
## nonroad) variable, which of these four sources have seen decreases
## in emissions from 1999–2008 for Baltimore City? Which have seen increases
## in emissions from 1999–2008? 
## The function assumes that the archive is already downloaded and placed
## in the current working directory.
plot3 = function() {
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
    
    # subset only data covering Baltimore City
    baltimore.NEI = NEI[NEI$fips == "24510",];
    
    # calculate the total number of tons emitted per year
    message("calculating the total volumes of emissions per year")
    emissions.by.year.type = aggregate(
        list(emissions = baltimore.NEI$Emissions),
        by = list(type = baltimore.NEI$type,
                  year = baltimore.NEI$year),
        sum);
    
    # make a plot    
    g = ggplot(data = emissions.by.year.type,
           aes(x = year,
               y = emissions,
               colour = type));
    line = geom_line();
    x.lab = xlab("Year");
    y.lab = ylab(expression("Tons of"~PM[2.5]~"emitted"));
    title = ggtitle(
        expression(PM[2.5]~"concentration trend in Baltimore by source type"));
    g + line + x.lab + y.lab + title;
    
    ggsave("plot3.png", height = 6.4, width = 7.2, units = "in", dpi = 75);
}