
GetOptEX
========

GetOptEX is an auxiliary OTP application designed to help other app on parsing cmd-line arguments.

Common use of excellent Juanjo Comellas getopt involves the caller passing a suitable opts list
and then dealing with getopt leftovers. GetOptEX scans a designated application (upon apps load) and collects
opts terms and funs from any modules to do check on provided command options. The consumer do not need to
provide eny of these items as getoptsEX can scan all modules registered into the applñication manifesto and
collect these bits by itself. 

Originally getoptEx evolved as bunch of modules used in my own applications but I decided to move them into a 
separate application and also bump getopt dependece to the latest version.
