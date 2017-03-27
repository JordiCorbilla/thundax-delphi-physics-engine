program TDPEDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufrmDemo in 'ufrmDemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
