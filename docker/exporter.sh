#!/bin/bash

############################################################
# Help                                                     #
############################################################
Help()
{
   # Display Help
   echo "Generate PlantUML and PNGs from a Structurizr DSL workspace"
   echo "It uses a custom C4 plantuml exporter that uses Properties to declare additional C4 PlantUML layout instructions"
   echo
   echo "Syntax: <workspace file location> <output path> [diagrams-prefix]"
   echo
}

if [[ $# -ne 2 ]] && [[ $# -ne 3 ]];
then

  Help
  exit 1

fi

echo "Workspace: $1";
echo "Output path: $2";
echo "Diagrams prefix: $3";

## Use structurizr to generate plantuml
java -jar /app/struct-c4-layout.jar $1 $2 $3

## Create the PNG version of each plant uml diagram
java -jar /app/plantuml.jar $2
