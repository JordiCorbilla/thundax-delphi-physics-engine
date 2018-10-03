(*
 * Copyright (c) 2010-2017 Thundax Delphi Physics Engine
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name of 'TDPE' nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

unit frmView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, tdpe.lib.engine,
  tdpe.lib.vector, tdpe.lib.render.fmx,
  tdpe.lib.jansen.scenery, tdpe.lib.engine.wrapper,
  tdpe.lib.particle.group, tdpe.lib.particle.pattern.composite,
  tdpe.lib.particle.circle, tdpe.lib.particle.spring.restriction,
  tdpe.lib.jansen.mechanism, tdpe.lib.jansen.robot, tdpe.lib.particle.box,
  FMX.Layouts, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, System.Threading;

type
  TDrawingThread = class(TThread)
  private
    FEngine: TFluentEngine;
    FRobot: TRobot;
    FTickEvent: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    procedure SetEngine(Engine : TFluentEngine);
    procedure SetRobot(Robot : TRobot);
    destructor Destroy; override;
    procedure FinishThreadExecution;
  end;

  TmainView = class(TForm)
    Panel1: TPanel;
    Toggle: TButton;
    Direction: TButton;
    Image1: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure DirectionClick(Sender: TObject);
    procedure ToggleClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDrawingThread: TDrawingThread;
  public
    Engine: TFluentEngine;
    Render: TFMXRenderer;
    FGround: TScenery;
    FRobot: TRobot;
    FBusy : boolean;
  end;

var
  mainView: TmainView;
  StopDrawing : boolean;
  FBitmap : TBitmap;

const
  FullHeight = 623;
  FullWidth = 1252;

implementation

uses
  tdpe.lib.force;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TmainView.DirectionClick(Sender: TObject);
begin
  Frobot.toggleDirection();
end;

procedure TmainView.FormCreate(Sender: TObject);
var
  xFactor : double;
  yFactor : double;
begin
  StopDrawing := true;
  Engine := TFluentEngine.New(1 / 4).AddInitialForce(TForce.Create(false, 0, 2)).AddDamping(0).AddRestrictionCollitionCycles(10);
  FBitmap := TBitmap.Create(Round(image1.Width), Round(image1.Height));
  Render := TFMXRenderer.Create(FBitmap);

  //Calculate x,y factor according to the new height and width
  xFactor := Image1.Width / FullWidth;
  yFactor := Image1.Height / FullHeight;

  FGround := TScenery.Create(Render, Engine, clablue, xFactor, yFactor);
  Frobot := TRobot.Create(Render, Engine, 1050, 400, 1, 0.02, xFactor, yFactor);
  Engine.AddGroups(FGround).AddGroups(Frobot);
  FGround.AddCollidable(Frobot);
  Frobot.togglePower();
end;

procedure TmainView.FormDestroy(Sender: TObject);
begin
  FDrawingThread.FinishThreadExecution;
end;

procedure TmainView.Timer1Timer(Sender: TObject);
var
  tasks: array of ITask;
  value: Integer;
  bitmap : TBitmap;
begin
  Engine.Run;
  Frobot.Run();
  bitmap := TBitmap.Create;
  try
    bitmap.SetSize(round(Image1.Width), round(Image1.Height));
    Image1.MultiResBitmap.Bitmaps[1].Assign(bitmap);
    Image1.Bitmap := Image1.MultiResBitmap.Bitmaps[1];
    Image1.Bitmap.Clear(TAlphaColorRec.White);

    Fbitmap.Canvas.BeginScene;
    Fbitmap.Clear(TAlphaColorRec.White);
    Engine.Paint;
    Fbitmap.Canvas.EndScene;
    image1.MultiResBitmap.Bitmaps[1].Assign(Fbitmap);
    image1.Bitmap := image1.MultiResBitmap.Bitmaps[1];
  finally
    bitmap.Free;
  end;
  sleep(3);
end;

procedure TmainView.ToggleClick(Sender: TObject);
begin
  if StopDrawing then
  begin
    StopDrawing := false;
    FDrawingThread := TDrawingThread.Create(true);
    FDrawingThread.SetEngine(Engine);
    FDrawingThread.SetRobot(FRobot);
    FDrawingThread.Start;
  end
  else
    StopDrawing := true;
  Frobot.togglePower();
end;

{ TDrawingThread }

constructor TDrawingThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FreeOnTerminate := True;
end;

destructor TDrawingThread.Destroy;
begin

  inherited;
end;

procedure TDrawingThread.Execute;
var
  bitmap : TBitmap;
begin
  while not Terminated do
  begin
    FEngine.Run();
    Frobot.Run();

    TThread.Synchronize(TThread.CurrentThread, procedure
      begin
        bitmap := TBitmap.Create;
        try
          bitmap.SetSize(round(mainView.Image1.Width), round(mainView.Image1.Height));
          mainView.Image1.MultiResBitmap.Bitmaps[1].Assign(bitmap);
          mainView.Image1.Bitmap := mainView.Image1.MultiResBitmap.Bitmaps[1];
          mainView.Image1.Bitmap.Clear(TAlphaColorRec.White);
          Fbitmap.Canvas.BeginScene;
          Fbitmap.Clear(TAlphaColorRec.White);
          FEngine.Paint;
          Fbitmap.Canvas.EndScene;
          mainView.image1.MultiResBitmap.Bitmaps[1].Assign(Fbitmap);
          mainView.image1.Bitmap := mainView.image1.MultiResBitmap.Bitmaps[1];
        finally
          bitmap.Free;
        end;
    end);
    Sleep(10);
    if StopDrawing then
      Exit;
  end;
end;

procedure TDrawingThread.FinishThreadExecution;
begin
  //Terminate;
end;

procedure TDrawingThread.SetEngine(Engine: TFluentEngine);
begin
  FEngine := Engine;
end;

procedure TDrawingThread.SetRobot(Robot: TRobot);
begin
  FRobot := Robot;
end;

end.
