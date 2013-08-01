(*
  * Copyright (c) 2010-2012 Thundax Delphi Physics Engine
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
unit uBallsDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ufrmlog,

  //uses TDPE
  tdpe.lib.force, tdpe.lib.render.gdi,
  tdpe.lib.structures.bridge, tdpe.lib.structures.surface.static,
  tdpe.lib.structures.dispenser.box, tdpe.lib.structures.dispenser.circle,
  tdpe.lib.richedit.writer, tdpe.lib.writer.contract,
  tdpe.lib.structures.cloth, tdpe.lib.structures.car,
  tdpe.lib.structures.box.simulation, tdpe.lib.engine.wrapper;

type
  TfBalls = class(TForm)
    tmr1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
  public
    engine: TFluentEngine;
    render: TGDIRenderer;
    bridge: TCustomBridge;
    surface: TSurfaces;
    BoxDispenser: TBoxDispenser;
    CircleDispenser: TCircleDispenser;
    richEditwriter: IWriter;
    ActiveDirect2D: Boolean;
    cloth : TCloth;
    car : TCar;
    brick : TBrick;
    LogDisplay : TFLog;
  end;

var
  fBalls: TfBalls;

implementation

uses
  D2D1, Graphics, Direct2D;

{$R *.dfm}

procedure TfBalls.FormCreate(Sender: TObject);
begin
  if not TDirect2DCanvas.Supported then
    ShowMessage('Direct2D is not supported in your system!');

  LogDisplay := TFLog.Create(Self);

  ActiveDirect2D := false;
  richEditwriter := TRichEditWriter.Create(LogDisplay.reLog);

  engine := TFluentEngine.New(1 / 4)
    .AddInitialForce(TForce.Create(false, 0, 1))
    .AddDamping(1)
    .AddRestrictionCollitionCycles(1);

  render := TGDIRenderer.Create(fBalls.Canvas, ClientRect);

  surface := TSurfaces.Create(render, engine, nil);

  BoxDispenser := TBoxDispenser.Create(render, engine, 800, 200, clred);
  BoxDispenser.setLog(richEditwriter);

  CircleDispenser := TCircleDispenser.Create(render, engine, 800, 100, clred);
  CircleDispenser.setLog(richEditwriter);

  bridge := TCustomBridge.Create(render, engine, 170, 140);
  bridge.setLog(richEditwriter);

  cloth := TCloth.Create(render, engine, 170, 300);
  car := TCar.create(render, engine);
  brick := TBrick.create(render, engine, 300, 600, clwhite);

  engine.addGroups(surface)
    .addGroups(BoxDispenser)
    .addGroups(CircleDispenser)
    .addGroups(bridge)
    .addGroups(cloth)
    .addGroups(car)
    .AddGroups(brick);

  //Define surfaces that collide
  surface.AddCollidable(brick);
  surface.AddCollidable(car);
  surface.AddCollidable(BoxDispenser);
  surface.AddCollidable(CircleDispenser);

  CircleDispenser.AddCollidable(BoxDispenser);
  CircleDispenser.AddCollidable(surface);
  CircleDispenser.AddCollidable(bridge);

  BoxDispenser.AddCollidable(CircleDispenser);
  BoxDispenser.AddCollidable(surface);
  BoxDispenser.AddCollidable(bridge);

  bridge.AddCollidable(BoxDispenser);
  bridge.AddCollidable(CircleDispenser);

  car.AddCollidable(surface);

  DoubleBuffered := True;
  LogDisplay.Show;
end;

procedure TfBalls.FormDestroy(Sender: TObject);
begin
  FreeAndNil(brick);
  FreeAndNil(bridge);
  FreeAndNil(surface);
  FreeAndNil(BoxDispenser);
  FreeAndNil(CircleDispenser);
  FreeAndNil(render);
  FreeAndNil(engine);
  FreeAndNil(cloth);
  FreeAndNil(car);
  richEditwriter := nil;
  if Assigned(LogDisplay) then
    LogDisplay.Free;
end;

procedure TfBalls.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = 'z') or (Key = 'Z') then
    bridge.DeleteObject;
  if (Key = 'd') or (Key = 'D') then
    BoxDispenser.AddBox;
  if (Key = 'f') or (Key = 'F') then
    CircleDispenser.AddCircle;
  if (Key = 'q') or (Key = 'Q') then
    ActiveDirect2D := not ActiveDirect2D;

  if (Key = 'b') or (Key = 'B') then
    car.Speed := car.Speed- 0.01;
  if (Key = 'n') or (Key = 'N') then
    car.Speed := 0;
  if (Key = 'm') or (Key = 'M') then
    car.Speed := car.Speed + 0.01;
end;

procedure TfBalls.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos: Integer;
begin
  if bridge.isInside(X, Y, pos) then
    bridge.Activate(pos);
  if BoxDispenser.isInside(X, Y, pos) then
    BoxDispenser.Activate(pos);
  if CircleDispenser.isInside(X, Y, pos) then
    CircleDispenser.Activate(pos);
  if cloth.isInside(X, Y, pos) then
    cloth.Activate(pos);
  if brick.isInside(X, Y, pos) then
    brick.Activate(pos);
end;

procedure TfBalls.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  bridge.Move(X, Y);
  BoxDispenser.Move(X, Y);
  CircleDispenser.Move(X, Y);
  cloth.Move(X,Y);
  brick.Move(X, Y);
end;

procedure TfBalls.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  bridge.DeActivate();
  BoxDispenser.DeActivate();
  CircleDispenser.DeActivate();
  cloth.Deactivate();
  brick.Deactivate();
end;

procedure TfBalls.FormPaint(Sender: TObject);
begin
  if ActiveDirect2D then
  begin
    if TDirect2DCanvas.Supported then
    begin
      render.d2dCanvas := TDirect2DCanvas.Create(Canvas, ClientRect);
      if Assigned(render.d2dCanvas) then
      begin
        render.d2dCanvas.RenderTarget.beginDraw;
        render.d2dCanvas.RenderTarget.Clear(D2D1ColorF(clBlack));
        render.d2dCanvas.RenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
        engine.Paint;
        render.d2dCanvas.RenderTarget.EndDraw;
        FreeAndNil(render.d2dCanvas);
      end;
    end
    else
    begin
      if Assigned(render.d2dCanvas) then
        FreeAndNil(render.d2dCanvas);
      engine.Paint;
    end;
  end
  else
  begin
    if Assigned(render.d2dCanvas) then
      FreeAndNil(render.d2dCanvas);
    engine.Paint;
  end;
end;

procedure TfBalls.tmr1Timer(Sender: TObject);
begin
  engine.Run;
  Repaint;
end;

end.
