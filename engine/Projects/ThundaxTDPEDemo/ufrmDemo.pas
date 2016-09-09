unit ufrmDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, tdpe.lib.engine,
  tdpe.lib.vector, tdpe.lib.render.fmx,
  tdpe.lib.jansen.scenery, tdpe.lib.engine.wrapper,
  tdpe.lib.particle.group, tdpe.lib.particle.pattern.composite,
  tdpe.lib.particle.circle, tdpe.lib.particle.spring.restriction,
  tdpe.lib.jansen.mechanism, tdpe.lib.jansen.robot, tdpe.lib.particle.box,
  FMX.Layouts, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Layout1: TLayout;
    Button1: TButton;
    Toggle: TButton;
    Direction: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas;
      const [Ref] ARect: TRectF);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure ToggleClick(Sender: TObject);
    procedure DirectionClick(Sender: TObject);
  private
    procedure DrawBackground;
  public
    Engine: TFluentEngine;
    Render: TFMXRenderer;
    FGround: TScenery;
    FRobot: TRobot;
  end;

var
  Form2: TForm2;

implementation

uses
  tdpe.lib.force;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TForm2.Button1Click(Sender: TObject);
var pt0,pt1 : TPointF;
begin
  pt0.Create(0,0);
  pt1.Create(100,50);
  Layout1.Canvas.BeginScene;
  Layout1.Canvas.DrawLine(pt0,pt1,1);
  Layout1.Canvas.EndScene;
end;

procedure TForm2.DirectionClick(Sender: TObject);
begin
  Frobot.toggleDirection();
end;

procedure TForm2.DrawBackground;
//var
//  DC: HDC;
//  Rect: TRect;
//  X, Y: integer;
//  DotColor: integer;
begin
  //Self.Canvas.Brush.Style := bsSolid;
  //Self.Canvas.Brush.color := clgray;
  //Rect := Self.Canvas.ClipRect;
//  Self.Canvas.FillRect(Rect);
//  if true then
//  begin
//    DotColor := ColorToRGB(clblack);
//    DC := Self.Canvas.Handle;
//    Y := 0;
//    while Y < Rect.Bottom do
//    begin
//      X := 0;
//      while X < Rect.Right do
//      begin
//        SetPixel(DC, X, Y, DotColor);
//        Inc(X, 15);
//      end;
//      Inc(Y, 15);
//    end;
//  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
//  Engine := TFluentEngine.New(1 / 4).AddInitialForce(TForce.Create(false, 0, 2)).AddDamping(0).AddRestrictionCollitionCycles(10);
//  Layout1.Canvas.BeginScene();
//  Render := TFMXRenderer.Create(Layout1.Canvas);
//  FGround := TScenery.Create(Render, Engine, clablue);
//  Frobot := TRobot.Create(Render, Engine, 1050, 400, 1.3, 0.02);
//  Engine.AddGroups(FGround).AddGroups(Frobot);
//  FGround.AddCollidable(Frobot);
//  Frobot.togglePower();
//  Layout1.Canvas.EndScene;

  //DoubleBuffered := true;
end;

procedure TForm2.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if KeyChar = 'p' then
    Frobot.togglePower();

  if KeyChar = 'z' then
    Frobot.toggleDirection();

  if KeyChar = 'h' then
    Frobot.toggleLegs();

  if KeyChar = 'r' then
    Frobot.Run();
end;

procedure TForm2.FormPaint(Sender: TObject; Canvas: TCanvas;
  const [Ref] ARect: TRectF);
begin
//  Engine.Paint;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
//  Engine.Run;
//  Frobot.Run();
//  Layout1.Canvas.BeginScene();
//  Layout1.Canvas.Clear(claWhite);
//  Engine.Paint;
//  Layout1.Canvas.EndScene();
end;

procedure TForm2.ToggleClick(Sender: TObject);
begin
  Frobot.togglePower();
end;

end.
