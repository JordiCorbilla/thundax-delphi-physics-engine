@echo off
rem ******************************************************************************************
rem Setup the msbuild /v:q variables
rem ******************************************************************************************
call rsvars


msbuild /v:q Package\TDPhysics.Engine.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Package\TDPhysics.Engine.dproj   /target:Build  /p:config=Debug

msbuild /v:q Projects\ThundaxBallsDemo\ThundaxBallsDemo.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Projects\ThundaxBallsDemo\ThundaxBallsDemo.dproj   /target:Build  /p:config=Debug

msbuild /v:q Projects\ThundaxSimulationPlant\ThundaxSimulationPlant.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Projects\ThundaxSimulationPlant\ThundaxSimulationPlant.dproj   /target:Build  /p:config=Debug

msbuild /v:q Projects\ThundaxObjectsDemo\ThundaxObjectDemo.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Projects\ThundaxObjectsDemo\ThundaxObjectDemo.dproj   /target:Build  /p:config=Debug

msbuild /v:q Projects\ThundaxTDPETests\ThundaxTDPETests.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Projects\ThundaxTDPETests\ThundaxTDPETests.dproj   /target:Clean  /p:config=Debug

