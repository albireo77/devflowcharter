@echo off

rem	This script is starting devFlowcharter with 2 parameters:
rem	- parameter settingsFile allows reading settings from ini file instead from Windows registry (default);
rem       if path to not existing file is provided, devF will start with default settings then create and save settings
rem       to this file upon exit.
rem 	- parameter langDefinitionsDir allows setting path to alternate directory (default is LanguageDefinitions) 
rem       to read language definition files from 

@echo on
devFlowcharter.exe -settingsFile=devFlowcharter.ini -langDefinitionsDir=LanguageDefinitions

