# GlobalWarming

Reads temperature data from weather services
- NASA GISS
- Deutscher Wetterdienst (DWD)
and plots them as a function of time.

OpenSSL needed (on Windows copy the DLLs libeay32.dll and ssleay32.ssl into the bin folder).

Checkbox "Local files only": must be unchecked whenever files have to be read from the mentioned sites. These files are stored in a local cache (folder "data") and are access when "Local files only" is checked.
