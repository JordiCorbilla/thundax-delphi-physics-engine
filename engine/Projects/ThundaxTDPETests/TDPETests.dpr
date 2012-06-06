program TDPETests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Testtdpe.lib.vector in 'lib\Array\Testtdpe.lib.vector.pas',
  Testdpe.lib.structures in 'lib\Array\Testdpe.lib.structures.pas',
  tdpe.lib.structures in '..\..\Lib\Array\tdpe.lib.structures.pas',
  tdpe.lib.vector in '..\..\Lib\Array\tdpe.lib.vector.pas',
  tdpe.lib.math.helper in '..\..\Lib\Math\tdpe.lib.math.helper.pas',
  tdpe.lib.math in '..\..\Lib\Math\tdpe.lib.math.pas',
  Testtdpe.lib.math.helper in 'lib\Math\Testtdpe.lib.math.helper.pas',
  Testtdpe.lib.math in 'lib\Math\Testtdpe.lib.math.pas';

{R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

