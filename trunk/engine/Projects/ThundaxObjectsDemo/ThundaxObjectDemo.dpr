program ThundaxObjectDemo;

uses
  Forms,
  fObjectDemo in 'fObjectDemo.pas' {FormMainCarDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainCarDemo, FormMainCarDemo);
  Application.Run;
end.
