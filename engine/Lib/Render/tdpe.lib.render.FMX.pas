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

unit tdpe.lib.render.FMX;

interface

Uses tdpe.lib.render, FMX.Graphics, Classes, Math, tdpe.lib.vector, tdpe.lib.styles, System.UITypes, System.Types,
  tdpe.lib.styles.contract, System.UIConsts;

Type

  TFMXRenderer = Class(TAbstractRenderer)
  Private
    FCanvas: TCanvas;
  Public
    Constructor Create(const aCanvas: TCanvas); Reintroduce; overload;
    Constructor Create(const aCanvas: TCanvas; const rect: TRect); Reintroduce; overload;
    procedure DrawArrow(const Source, Target: IVector); Override;
    procedure CircleApproximation(const xcenter, ycenter, Radius, Rotate: Double; const style: IStyle); override;
    Procedure Circle(const xcenter, ycenter, Radius, Rotate: Double; const style: IStyle); Override;
    Procedure Line(const x, y, x1, y1: Double; const color: TColor; const penWidth: integer); Override;
    Procedure Text(const x, y: Double; const Text: String; const color: TColor); Override;
    Procedure FilledBox(const x, y, x1, y1, x2, y2, x3, y3: Double; const color: TColor); Override;
    procedure FilledTriangle(const x, y, x1, y1, x2, y2: Double; const color: TColor; const penWidth: integer); override;
    Procedure DrawPoint(const x, y: Double; const color: TColor); Override;
  end;

const
  points = 16;

implementation

uses
  system.Math.Vectors;

{ TFMXRenderer }

procedure TFMXRenderer.Circle(const xcenter, ycenter, Radius, Rotate: Double;  const style: IStyle);
var
  beforeBrushColor, beforePenColor: TColor;
  beforePenWidth: integer;
  rec : TRectF;
begin
FCanvas.Stroke.Thickness := 1.5;
      FCanvas.Stroke.Kind := TBrushKind.bkSolid;
      FCanvas.Fill.Color := TAlphaColorRec.Black;
      FCanvas.Fill.Kind := TBrushKind.bkSolid;
  beforeBrushColor := FCanvas.Fill.Color;
//  beforePenColor := FCanvas.Pen.color;
//  beforePenWidth := FCanvas.Pen.Width;
//
//  FCanvas.Pen.color := clWhite;
//  FCanvas.Pen.Width := style.penWidth;
//  FCanvas.Brush.color := style.BrushColor;

    FCanvas.DrawEllipse(TRectF.Create(Round(xcenter - Radius), Round(ycenter - Radius), Round(xcenter + Radius), Round(ycenter + Radius)), 1);
//
//  FCanvas.Brush.color := beforeBrushColor;
//  FCanvas.Pen.color := beforePenColor;
//  FCanvas.Pen.Width := beforePenWidth;
//
  Line(xcenter, ycenter, xcenter + Radius * cos(Rotate), ycenter + Radius * sin(Rotate), claWhite, 1);
  Line(xcenter, ycenter, xcenter + Radius * cos(Rotate + (pi / 2)), ycenter + Radius * sin(Rotate + (pi / 2)), claWhite, 1);
  Line(xcenter, ycenter, xcenter + Radius * cos(Rotate + pi), ycenter + Radius * sin(Rotate + pi), claWhite, 1);
  Line(xcenter, ycenter, xcenter + Radius * cos(Rotate - (pi / 2)), ycenter + Radius * sin(Rotate - (pi / 2)), claWhite, 1);
end;

procedure TFMXRenderer.CircleApproximation(const xcenter, ycenter, Radius,
  Rotate: Double; const style: IStyle);
begin
  inherited;

end;

constructor TFMXRenderer.Create(const aCanvas: TCanvas);
begin
  Assert(Assigned(aCanvas));
  FCanvas := aCanvas;
end;

constructor TFMXRenderer.Create(const aCanvas: TCanvas; const rect: TRect);
begin

end;

procedure TFMXRenderer.DrawArrow(const Source, Target: IVector);
begin
  inherited;

end;

procedure TFMXRenderer.DrawPoint(const x, y: Double; const color: TColor);
begin
  inherited;

end;

procedure TFMXRenderer.FilledBox(const x, y, x1, y1, x2, y2, x3, y3: Double;  const color: TColor);
var
  polygon : TPolygon;
begin
FCanvas.Stroke.Thickness := 1.5;
      FCanvas.Stroke.Kind := TBrushKind.bkSolid;
      FCanvas.Fill.Color := TAlphaColorRec.Black;
      FCanvas.Fill.Kind := TBrushKind.bkSolid;
//    FCanvas.Pen.color := clWhite;
//    FCanvas.Pen.Width := 2;
//    beforeColor := FCanvas.Brush.color;
//    FCanvas.Brush.color := color;
    setlength(polygon, 4);
    polygon[0] := Point(Round(x), Round(y));
    polygon[1] := Point(Round(x1), Round(y1));
    polygon[2] := Point(Round(x2), Round(y2));
    polygon[3] := Point(Round(x3), Round(y3));
    FCanvas.DrawPolygon(polygon, 1);
    //FCanvas.Brush.color := beforeColor;
end;

procedure TFMXRenderer.FilledTriangle(const x, y, x1, y1, x2, y2: Double;
  const color: TColor; const penWidth: integer);
var
  beforeColor: TColor;
  beforePen: integer;
  rec: array [0 .. 2] of TPoint;
  polygon : TPolygon;
begin
//  beforeColor := FCanvas.Brush.color;
//  beforePen := FCanvas.Pen.Width;
//
//  FCanvas.Brush.color := color;
//  FCanvas.Pen.Width := penWidth;
FCanvas.Stroke.Thickness := 1.5;
      FCanvas.Stroke.Kind := TBrushKind.bkSolid;
      FCanvas.Fill.Color := TAlphaColorRec.Black;
      FCanvas.Fill.Kind := TBrushKind.bkSolid;
  setlength(polygon, 3);

  polygon[0] := Point(Round(x), Round(y));
  polygon[1] := Point(Round(x1), Round(y1));
  polygon[2] := Point(Round(x2), Round(y2));
  FCanvas.DrawPolygon(polygon, 1);
//  FCanvas.Brush.color := beforeColor;
//  FCanvas.Pen.Width := beforePen;
end;

procedure TFMXRenderer.Line(const x, y, x1, y1: Double; const color: TColor; const penWidth: integer);
var
  beforeColor: TColor;
  beforePenWidth: integer;
  point1, point2 : tpointf;
begin
    //beforeColor := FCanvas.Pen.color;
    //beforePenWidth := FCanvas.Pen.Width;
    //FCanvas.Pen.color := color;
    //FCanvas.Pen.Width := penWidth;
FCanvas.Stroke.Thickness := 1.5;
      FCanvas.Stroke.Kind := TBrushKind.bkSolid;
      FCanvas.Fill.Color := TAlphaColorRec.Black;
      FCanvas.Fill.Kind := TBrushKind.bkSolid;
    point1.X :=Round(x);
    point1.Y :=Round(y);
    point2.X :=Round(x1);
    point2.Y :=Round(y1);
    FCanvas.DrawLine(point1, point2, 1);
//    FCanvas.Pen.color := beforeColor;
//    FCanvas.Pen.Width := beforePenWidth;
end;

procedure TFMXRenderer.Text(const x, y: Double; const Text: String;
  const color: TColor);
begin
  inherited;

end;

end.
