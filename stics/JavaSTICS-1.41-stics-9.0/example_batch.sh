# - Syntax from javastics directory
# example for an usms list
java -jar JavaSticsCmd.exe --run example SugarCane maize
# example for successive usms
java -jar JavaSticsCmd.exe --run-successive example demo_Wheat1 demo_BareSoil2 demo_maize3
# example for the entire working directory
java -jar JavaSticsCmd.exe --run example
#
# - Syntax using absolute paths
# java -jar path/to/javastics/directory/JavaSticsCmd.exe --run path/to/workspace/directory usmname1 usmname2 
