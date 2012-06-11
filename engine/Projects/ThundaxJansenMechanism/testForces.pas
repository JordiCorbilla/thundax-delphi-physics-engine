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
  tdpe.lib.jansen.scenery, ExtCtrls,
  StdCtrls, tdpe.lib.particle.group, tdpe.lib.particle.pattern.composite,
  tdpe.lib.particle.circle, tdpe.lib.particle.spring.restriction,
  tdpe.lib.jansen.mechanism, tdpe.lib.jansen.robot, tdpe.lib.particle.box;

type
  TForm1 = class(TForm)
    tmr1: TTimer;
    Image1: TImage;
    Image2: TImage;
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
    Ape: TEngine;
    R: TGDIRenderer;
    aGround: TScenery;
    arobot: TRobot;
    s: Boolean;
    n: Boolean;

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
  s := false;
  Ape := TEngine.Create(1 / 4);
  R := TGDIRenderer.Create(Form1.Canvas);

  Ape.AddForce(TForce.Create(false, 0, 2));

  aGround := TScenery.Create(R, Ape, clblue);
  Ape.AddGroup(aGround);
  Ape.damping := 0; //0.99;
  Ape.RestrictionCollisionCycles := 10; //10;

  arobot := TRobot.Create(R, Ape, 1050, 400, 2, 0.02);
  Ape.AddGroup(arobot);
  aGround.AddCollidable(aRobot);
//  arobot.AddCollidable(aGround);

  Image1.Visible := false;
  Image2.Visible := false;

  arobot.togglePower();
  DoubleBuffered := true;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Ape);
  FreeAndNil(R);
  FreeAndNil(aGround);
  FreeAndNil(arobot);
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  Ape.Run;
  arobot.Run();
  Repaint;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = 's' then
    s := not s; // Jensen.Move(-0.2);
  if Key = 'p' then
    arobot.togglePower(); // s := not s;

  if Key = 'z' then
    arobot.toggleDirection(); // s := not s;

  if Key = 'h' then
    arobot.toggleLegs();

  if Key = 'r' then
    arobot.Run();

  if Key = 'n' then
    n := not n;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  DrawBackground;
  Ape.Paint;
end;

end.
