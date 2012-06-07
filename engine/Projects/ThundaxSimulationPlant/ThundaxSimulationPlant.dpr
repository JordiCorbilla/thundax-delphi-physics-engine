program ThundaxSimulationPlant;

uses
  Forms,
  fPlantDemo in 'fPlantDemo.pas' {FormMainPlantDemo};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Thundax Simulation Plant';
  Application.CreateForm(TFormMainPlantDemo, FormMainPlantDemo);
  Application.Run;
end.
