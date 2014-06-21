## Create a plot using the base plotting system answering the question: Have 
## total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## The function assumes that the archive is already downloaded and placed
## in the current working directory.
plot1 = function() {
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
    
    # calculate the total number of tons emitted per year
    message("calculating the total volumes of emissions per year")
    emissions.by.year = aggregate(list(emissions = NEI$Emissions),
                                  by = list(year = NEI$year),
                                  sum);    
    # make a plot
    png("plot1.png");
    
    plot(emissions ~ year,
         data = emissions.by.year,
         type = "b",
         main = expression("Total"~PM[2.5]~"concentration trend during years"),
         xaxt = "n",
         xlab = "Year",
         ylab=expression("Total tons of"~PM[2.5]~"emitted"),
         lwd = 3,
         col = "#FF5575");
    axis(1, at = emissions.by.year$year, labels=emissions.by.year$year);
    
    dev.off();
}