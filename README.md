# GlobalWarming

Reads temperature data from weather services
- NASA GISS (https://data.giss.nasa.gov/gistemp/)
- Deutscher Wetterdienst (DWD, https://www.dwd.de/EN/climate_environment/cdc/cdc_node.html)
- Pages2k project (http://pastglobalchanges.org/)
and plots them as a function of time.

OpenSSL needed (on Windows copy the DLLs libeay32.dll and ssleay32.ssl into the bin folder).

Checkbox "Local files only": must be unchecked whenever files have to be read from the mentioned sites. These files are stored in a local cache (folder "data") and are accessed when "Local files only" is checked.
