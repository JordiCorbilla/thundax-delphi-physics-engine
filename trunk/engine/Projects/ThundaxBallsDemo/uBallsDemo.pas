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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls,

  //uses TDPE
  tdpe.lib.engine, tdpe.lib.force, tdpe.lib.vector, tdpe.lib.render.gdi,
  tdpe.lib.structures.bridge, tdpe.lib.particle.box, tdpe.lib.structures.surface.static,
  tdpe.lib.structures.dispenser.box, tdpe.lib.structures.dispenser.circle,
  StdCtrls, tdpe.lib.richedit.writer, Direct2D, tdpe.lib.particle.circle.solid,
  tdpe.lib.writer.contract;

type
  TfBalls = class(TForm)
    tmr1: TTimer;
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
    engine: TEngine;
    render: TGDIRenderer;
    bridge: TCustomBridge;
    surface: TSurfaces;
    rigidCircle: TSolidCircle;
    BoxDispenser: TBoxDispenser;
    CircleDispenser: TCircleDispenser;
    richEditwriter: IWriter;
    ActiveDirect2D: Boolean;
  end;

var
  fBalls: TfBalls;

implementation

uses
  ufrmlog, D2D1;

{$R *.dfm}

procedure TfBalls.FormCreate(Sender: TObject);
begin
  if not TDirect2DCanvas.Supported then
    ShowMessage('Direct2D is not supported in your system!');

  FLog := TFLog.Create(Self);

  ActiveDirect2D := false;
  richEditwriter := TRichEditWriter.Create(FLog.reLog);
  engine := TEngine.Create(1 / 4);
  render := TGDIRenderer.Create(fBalls.Canvas, ClientRect);

  engine.AddForce(TForce.Create(false, 0, 1));
  engine.damping := 1;
  engine.RestrictionCollisionCycles := 1;

  surface := TSurfaces.Create(render, engine, richEditwriter);
  engine.addGroup(surface);

  BoxDispenser := TBoxDispenser.Create(render, engine, 800, 200, clred);
  BoxDispenser.setLog(richEditwriter);
  engine.addGroup(BoxDispenser);

  CircleDispenser := TCircleDispenser.Create(render, engine, 800, 100, clred);
  CircleDispenser.setLog(richEditwriter);
  engine.addGroup(CircleDispenser);
  bridge := TCustomBridge.Create(render, engine, 170, 140);
  bridge.setLog(richEditwriter);
  engine.addGroup(bridge);

  //Define surfaces that collide
  surface.AddCollidable(BoxDispenser);
  surface.AddCollidable(CircleDispenser);
  CircleDispenser.AddCollidable(BoxDispenser);
  BoxDispenser.AddCollidable(CircleDispenser);
  bridge.AddCollidable(BoxDispenser);
  bridge.AddCollidable(CircleDispenser);
  BoxDispenser.AddCollidable(surface);
  CircleDispenser.AddCollidable(surface);
  BoxDispenser.AddCollidable(bridge);
  CircleDispenser.AddCollidable(bridge);

  DoubleBuffered := True;
  FLog.Show;
end;

procedure TfBalls.FormDestroy(Sender: TObject);
begin
  FreeAndNil(bridge);
  FreeAndNil(surface);
  FreeAndNil(BoxDispenser);
  FreeAndNil(CircleDispenser);
  FreeAndNil(render);
  FreeAndNil(engine);
  richEditwriter := nil;
  if Assigned(FLog) then
    FLog.Free;
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
end;

procedure TfBalls.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  bridge.Move(X, Y);
  BoxDispenser.Move(X, Y);
  CircleDispenser.Move(X, Y);
end;

procedure TfBalls.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  bridge.DeActivate();
  BoxDispenser.DeActivate();
  CircleDispenser.DeActivate();
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
