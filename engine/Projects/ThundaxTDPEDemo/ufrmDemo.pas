(*
 * Copyright (c) 2010-2016 Thundax Delphi Physics Engine
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
  FMX.Layouts, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Toggle: TButton;
    Direction: TButton;
    Image1: TImage;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const [Ref] ARect: TRectF);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ToggleClick(Sender: TObject);
    procedure DirectionClick(Sender: TObject);
  private
    procedure DrawBackground;
  public
    Engine: TFluentEngine;
    Render: TFMXRenderer;
    FGround: TScenery;
    FRobot: TRobot;
    FBitmap : TBitmap;
  end;

var
  Form2: TForm2;

const
  FullHeight = 623;
  FullWidth = 1252;

implementation

uses
  tdpe.lib.force;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

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
var
  xFactor : double;
  yFactor : double;
begin
  Engine := TFluentEngine.New(1 / 4).AddInitialForce(TForce.Create(false, 0, 2)).AddDamping(0).AddRestrictionCollitionCycles(10);
  FBitmap := TBitmap.Create(Round(image1.Width), Round(image1.Height));
  Render := TFMXRenderer.Create(FBitmap);

  //Calculate x,y factor according to the new height and width
  xFactor := Image1.Width / FullWidth;
  yFactor := Image1.Height / FullHeight;

  FGround := TScenery.Create(Render, Engine, clablue, xFactor, yFactor);
  Frobot := TRobot.Create(Render, Engine, 1050, 400, 1.3, 0.02, xFactor, yFactor);
  Engine.AddGroups(FGround).AddGroups(Frobot);
  FGround.AddCollidable(Frobot);
  Frobot.togglePower();
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
var
  bitmap : TBitmap;
begin
  Engine.Run;
  Frobot.Run();

  bitmap := TBitmap.Create;
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
end;

procedure TForm2.ToggleClick(Sender: TObject);
begin
  Frobot.togglePower();
end;

end.
