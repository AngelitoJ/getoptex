
GetOptEX
========

GetOptEX is an auxiliary OTP library application designed to help other apps on parsing cmd-line arguments.

Common parsing scebarios involve using the excellent Juanjo Comellas'' getopt by calling it directly with 
a suitable opts list, and then dealing with getopt's leftovers. 

GetOptEx uses also an extra user supplied list of funs to check on parsed command-line options and do things
like file or TCP ports checking prior to application start.

GetOptEX also adds automatic scanning and collection of opts terms lists and funs lists from any modules listed 
in the calling applicationt manifesto (app file). The consumer does not need to provide eny of these items as 
there are gatherer automagically...

Originally getoptEx evolved from a bunch of modules used in my own applications but I decided to move them into a 
separate application and also bump getopt deps to the latest version.
