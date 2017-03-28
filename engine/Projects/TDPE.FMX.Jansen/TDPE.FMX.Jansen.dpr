program TDPE.FMX.Jansen;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmView in 'frmView.pas' {mainView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TmainView, mainView);
  Application.Run;
end.
