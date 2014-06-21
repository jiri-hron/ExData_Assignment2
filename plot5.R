## Create a plot answering the question: 
## How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City? 
## The function assumes that the archive is already downloaded and placed
## in the current working directory.
plot5 = function() {
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
    baltimore.NEI = NEI[NEI$fips == "24510",];
    
    # pick all records related to vehicles; use categories containing
    # "transport" or "transportation" in names, assume most of means of
    # transport around Baltimore is based on combustion-engine vehicles
    vehicle.related = SCC[
        grepl("^(.*?\\s)?([tT]ransport(ation)?|[vV]ehicle)(\\s.*)?",
                             levels(SCC$SCC.Level.Two)),];
    # get rid of the only irrelevant category to transportation
    vehicle.related = vehicle.related[vehicle.related$SCC.Level.Two 
                                      != "Transportation Equipment",];
    baltimore.NEI.vehicle =
        baltimore.NEI[baltimore.NEI$SCC %in% vehicle.related$SCC,];
    
    # calculate the total number of tons emitted per year
    message("calculating the total volumes of emissions per year")
    emissions.by.year = aggregate(
        list(emissions = baltimore.NEI.vehicle$Emissions),
        by = list(year = baltimore.NEI.vehicle$year),
        sum);
    
    # make a plot    
    png("plot5.png");
    
    plot(emissions ~ year,
         data = emissions.by.year,
         type = "b",
         main = expression("Vehicle/transport related"~PM[2.5]~"concentration"),
         xaxt = "n",
         xlab = "Year",
         ylab=expression("Total tons of"~PM[2.5]~"emitted"),
         lwd = 3,
         col = "#1E729E");
    axis(1, at = emissions.by.year$year, labels=emissions.by.year$year);
    
    dev.off();
}