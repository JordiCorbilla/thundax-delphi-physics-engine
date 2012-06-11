@echo off
rem ******************************************************************************************
rem Setup the msbuild /v:q variables
rem ******************************************************************************************
call rsvars


msbuild /v:q bpl\TDPhysicsEngine.dproj   /target:Clean  /p:config=Debug
msbuild /v:q bpl\TDPhysicsEngine.dproj   /target:Build  /p:config=Debug

msbuild /v:q Projects\ThundaxBallsDemo\ThundaxBallsDemo.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Projects\ThundaxBallsDemo\ThundaxBallsDemo.dproj   /target:Build  /p:config=Debug

msbuild /v:q Projects\ThundaxSimulationPlant\ThundaxSimulationPlant.dproj   /target:Clean  /p:config=Debug
msbuild /v:q Projects\ThundaxSimulationPlant\ThundaxSimulationPlant.dproj   /target:Build  /p:config=Debug

