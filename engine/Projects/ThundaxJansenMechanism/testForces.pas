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
unit testForces;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tdpe.lib.engine, tdpe.lib.vector, tdpe.lib.render.gdi,
  tdpe.lib.jansen.scenery, ExtCtrls, tdpe.lib.engine.wrapper,
  StdCtrls, tdpe.lib.particle.group, tdpe.lib.particle.pattern.composite,
  tdpe.lib.particle.circle, tdpe.lib.particle.spring.restriction,
  tdpe.lib.jansen.mechanism, tdpe.lib.jansen.robot, tdpe.lib.particle.box;

type
  TForm1 = class(TForm)
    tmr1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
  private
    procedure DrawBackground;
  public
    Engine: TFluentEngine;
    Render: TGDIRenderer;
    FGround: TScenery;
    FRobot: TRobot;
  end;

var
  Form1: TForm1;

implementation

uses
  tdpe.lib.force;

{$R *.dfm}

procedure TForm1.DrawBackground();
var
  DC: HDC;
  Rect: TRect;
  X, Y: integer;
  DotColor: integer;
begin
  Self.Canvas.Brush.Style := bsSolid;
  Self.Canvas.Brush.color := clgray;
  Rect := Self.Canvas.ClipRect;
  Self.Canvas.FillRect(Rect);
  if true then
  begin
    DotColor := ColorToRGB(clblack);
    DC := Self.Canvas.Handle;
    Y := 0;
    while Y < Rect.Bottom do
    begin
      X := 0;
      while X < Rect.Right do
      begin
        SetPixel(DC, X, Y, DotColor);
        Inc(X, 15);
      end;
      Inc(Y, 15);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Engine := TFluentEngine.New(1 / 4)
    .AddInitialForce(TForce.Create(false, 0, 2))
    .AddDamping(0).
    AddRestrictionCollitionCycles(10);

  Render := TGDIRenderer.Create(Form1.Canvas);

  FGround := TScenery.Create(Render, Engine, clblue);

  Frobot := TRobot.Create(Render, Engine, 1050, 400, 2, 0.01);

  Engine.AddGroups(FGround)
    .AddGroups(Frobot);

  FGround.AddCollidable(Frobot);

  Frobot.togglePower();
  DoubleBuffered := true;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Engine);
  FreeAndNil(Render);
  FreeAndNil(FGround);
  FreeAndNil(Frobot);
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  Engine.Run;
  Frobot.Run();
  Repaint;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = 'p' then
    Frobot.togglePower();

  if Key = 'z' then
    Frobot.toggleDirection();

  if Key = 'h' then
    Frobot.toggleLegs();

  if Key = 'r' then
    Frobot.Run();
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  DrawBackground;
  Engine.Paint;
end;

end.
