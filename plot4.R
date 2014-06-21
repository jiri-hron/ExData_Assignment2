## Create a plot answering the question: 
## Across the United States, how have emissions from coal combustion-related
## sources changed from 1999–2008?
## The function assumes that the archive is already downloaded and placed
## in the current working directory.
plot4 = function() {
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
    
    # pick all records related to coal - assuming most of the coal will be
    # combusted, pick all actions related to coal - mining, transport, ...
    coal.related = SCC[grepl("^(.*?\\s)?[cC]oal(\\s.*)?",
                             levels(SCC$SCC.Level.Three)),];
    coal.NEI = NEI[NEI$SCC %in% coal.related$SCC,];
    
    # calculate the total number of tons emitted per year
    message("calculating the total volumes of emissions per year")
    emissions.by.year = aggregate(
        list(emissions = coal.NEI$Emissions),
        by = list(year = coal.NEI$year),
        sum);
    
    # make a plot    
    png("plot4.png");
    
    plot(emissions ~ year,
         data = emissions.by.year,
         type = "b",
         main = expression("Coal related"~PM[2.5]~"concentration trend"),
         xaxt = "n",
         xlab = "Year",
         ylab=expression("Total tons of"~PM[2.5]~"emitted"),
         lwd = 3,
         col = "#72CCDB");
    axis(1, at = emissions.by.year$year, labels=emissions.by.year$year);
    
    dev.off();
}