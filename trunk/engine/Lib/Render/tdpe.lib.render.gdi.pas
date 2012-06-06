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
unit tdpe.lib.render.gdi;

interface

Uses tdpe.lib.render, Graphics, Classes, Math, tdpe.lib.vector, tdpe.lib.styles, Direct2D, D2D1, Types,
  tdpe.lib.styles.contract;

Type

  TGDIRenderer = Class(TAbstractRenderer)
  Private
    FCanvas: TCanvas;
  Public
    d2dCanvas: TDirect2DCanvas;
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
  Windows, tdpe.lib.Math;

{ TGDIRenderer }

procedure TGDIRenderer.Circle(const xcenter, ycenter, Radius, Rotate: Double; const style: IStyle);
var
  beforeBrushColor, beforePenColor: TColor;
  beforePenWidth: integer;
begin
  if not Assigned(d2dCanvas) then
  begin
    beforeBrushColor := FCanvas.Brush.color;
    beforePenColor := FCanvas.Pen.color;
    beforePenWidth := FCanvas.Pen.Width;

    FCanvas.Pen.color := clWhite;
    FCanvas.Pen.Width := style.penWidth;
    FCanvas.Brush.color := style.BrushColor;
    FCanvas.Ellipse(Round(xcenter - Radius), Round(ycenter - Radius), Round(xcenter + Radius), Round(ycenter + Radius));

    FCanvas.Brush.color := beforeBrushColor;
    FCanvas.Pen.color := beforePenColor;
    FCanvas.Pen.Width := beforePenWidth;

    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate), ycenter + Radius * sin(Rotate), clWhite, 1);
    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate + (pi / 2)), ycenter + Radius * sin(Rotate + (pi / 2)), clWhite, 1);
    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate + pi), ycenter + Radius * sin(Rotate + pi), clWhite, 1);
    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate - (pi / 2)), ycenter + Radius * sin(Rotate - (pi / 2)), clWhite, 1);
  end
  else
  begin
    beforeBrushColor := d2dCanvas.Brush.color;
    beforePenColor := d2dCanvas.Pen.color;
    beforePenWidth := d2dCanvas.Pen.Width;

    d2dCanvas.Pen.color := clWhite;
    d2dCanvas.Pen.Width := style.penWidth;
    d2dCanvas.Brush.color := style.BrushColor;
    d2dCanvas.Ellipse(Round(xcenter - Radius), Round(ycenter - Radius), Round(xcenter + Radius), Round(ycenter + Radius));

    d2dCanvas.Brush.color := beforeBrushColor;
    d2dCanvas.Pen.color := beforePenColor;
    d2dCanvas.Pen.Width := beforePenWidth;

    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate), ycenter + Radius * sin(Rotate), clWhite, 1);
    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate + (pi / 2)), ycenter + Radius * sin(Rotate + (pi / 2)), clWhite, 1);
    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate + pi), ycenter + Radius * sin(Rotate + pi), clWhite, 1);
    Line(xcenter, ycenter, xcenter + Radius * cos(Rotate - (pi / 2)), ycenter + Radius * sin(Rotate - (pi / 2)), clWhite, 1);
  end;
end;

constructor TGDIRenderer.Create(const aCanvas: TCanvas);
begin
  Assert(Assigned(aCanvas));
  FCanvas := aCanvas;
end;

procedure TGDIRenderer.FilledBox(const x, y, x1, y1, x2, y2, x3, y3: Double; const color: TColor);
var
  beforeColor: TColor;
  rec: array [0 .. 3] of TPoint;
begin
  if not Assigned(d2dCanvas) then
  begin
    FCanvas.Pen.color := clWhite;
    FCanvas.Pen.Width := 2;
    beforeColor := FCanvas.Brush.color;
    FCanvas.Brush.color := color;
    rec[0] := Point(Round(x), Round(y));
    rec[1] := Point(Round(x1), Round(y1));
    rec[2] := Point(Round(x2), Round(y2));
    rec[3] := Point(Round(x3), Round(y3));
    FCanvas.Polygon(rec);
    FCanvas.Brush.color := beforeColor;
  end
  else
  begin
    d2dCanvas.Pen.color := clWhite;
    d2dCanvas.Pen.Width := 2;
    beforeColor := FCanvas.Brush.color;
    d2dCanvas.Brush.color := color;
    rec[0] := Point(Round(x), Round(y));
    rec[1] := Point(Round(x1), Round(y1));
    rec[2] := Point(Round(x2), Round(y2));
    rec[3] := Point(Round(x3), Round(y3));
    d2dCanvas.Polygon(rec);
    d2dCanvas.Brush.color := beforeColor;
  end;
end;

procedure TGDIRenderer.FilledTriangle(const x, y, x1, y1, x2, y2: Double; const color: TColor; const penWidth: integer);
var
  beforeColor: TColor;
  beforePen: integer;
  rec: array [0 .. 2] of TPoint;
begin
  beforeColor := FCanvas.Brush.color;
  beforePen := FCanvas.Pen.Width;

  FCanvas.Brush.color := color;
  FCanvas.Pen.Width := penWidth;
  rec[0] := Point(Round(x), Round(y));
  rec[1] := Point(Round(x1), Round(y1));
  rec[2] := Point(Round(x2), Round(y2));
  FCanvas.Polygon(rec);
  FCanvas.Brush.color := beforeColor;
  FCanvas.Pen.Width := beforePen;
end;

procedure TGDIRenderer.Line(const x, y, x1, y1: Double; const color: TColor; const penWidth: integer);
var
  beforeColor: TColor;
  beforePenWidth: integer;
begin
  if not Assigned(d2dCanvas) then
  begin
    beforeColor := FCanvas.Pen.color;
    beforePenWidth := FCanvas.Pen.Width;
    FCanvas.Pen.color := color;
    FCanvas.Pen.Width := penWidth;

    FCanvas.MoveTo(Round(x), Round(y));
    FCanvas.LineTo(Round(x1), Round(y1));
    FCanvas.Pen.color := beforeColor;
    FCanvas.Pen.Width := beforePenWidth;
  end
  else
  begin
    beforeColor := d2dCanvas.Pen.color;
    beforePenWidth := d2dCanvas.Pen.Width;
    d2dCanvas.Pen.color := color;
    d2dCanvas.Pen.Width := 1;
    d2dCanvas.DrawLine(D2D1PointF(x, y), D2D1PointF(x1, y1));
    d2dCanvas.Pen.color := beforeColor;
    d2dCanvas.Pen.Width := beforePenWidth;
  end;
end;

procedure TGDIRenderer.DrawPoint(const x, y: Double; const color: TColor);
var
  beforeColor: TColor;
begin
  beforeColor := FCanvas.Brush.color;
  FCanvas.Brush.color := clBlue;
  FCanvas.Brush.color := beforeColor;
end;

procedure TGDIRenderer.Text(const x, y: Double; const Text: String; const color: TColor);
var
  beforeColor: TColor;
begin
  beforeColor := FCanvas.Brush.color;
  FCanvas.Brush.color := color;
  FCanvas.TextOut(Round(x), Round(y), Text);
  FCanvas.Brush.color := beforeColor;
end;

procedure TGDIRenderer.DrawArrow(const Source, Target: IVector);
  function Distance(p1, p2: IVector): Double;
  begin
    try
      Result := sqrt(sqr(p1.x - p2.x) + sqr(p1.y - p2.y));
    except
      Result := 0;
    end;
  end;
  function CalcPoint(p: IVector; angle: Double; Distance: integer): TPoint;
  var
    x, y, M: Double;
  begin
    if TMathHelper.Compare(Abs(angle), (pi / 2), '<>') then
    begin
      if TMathHelper.Compare(Abs(angle), (pi / 2), '<') then
        Distance := -Distance;
      M := Tan(angle);
      x := p.x + Distance / sqrt(1 + sqr(M));
      y := p.y + M * (x - p.x);
      Result := Point(Round(x), Round(y));
    end
    else
    begin
      if angle > 0 then
        Distance := -Distance;
      Result := Point(Round(p.x), Round(p.y + Distance));
    end;
  end;

var
  angle: Double;
  PArrow: array [1 .. 4] of TPoint;
  restColor: TColor;
  beforeColor: TColor;
  lenArrow: integer;
  beforeStyle: TBrushStyle;
begin
  if (Distance(Source, Target) < 1.0) or (Distance(Source, Target) > 2000.0) then
    exit;
  angle := ArcTan2((Target.y - Source.y), (Target.x - Source.x));
  lenArrow := 5;
  PArrow[1] := Point(Round(Target.x), Round(Target.y));
  PArrow[2] := CalcPoint(Target, angle + pi / 9, lenArrow);
  PArrow[3] := CalcPoint(Target, angle, 2 * lenArrow div 3);
  PArrow[4] := CalcPoint(Target, angle - pi / 9, lenArrow);

  FCanvas.Pen.Width := 1;
  beforeColor := FCanvas.Pen.color;
  FCanvas.Pen.color := clBlue;
  beforeStyle := FCanvas.Brush.style;
  FCanvas.Brush.style := bsSolid;
  restColor := FCanvas.Brush.color;
  FCanvas.Brush.color := clBlue;
  FCanvas.Polygon(PArrow);
  FCanvas.Brush.color := restColor;
  FCanvas.Pen.color := beforeColor;
  FCanvas.Brush.style := beforeStyle;
end;

procedure TGDIRenderer.CircleApproximation(const xcenter, ycenter, Radius, Rotate: Double; const style: IStyle);
var
  PArrow: array [1 .. points] of TPoint;
  i: integer;
  theta: Double;
  x, y: Double;
  beforeBrushColor, beforePenColor: TColor;
  beforePenWidth: integer;
begin
  beforeBrushColor := FCanvas.Brush.color;
  beforePenColor := FCanvas.Pen.color;
  beforePenWidth := FCanvas.Pen.Width;

  FCanvas.Pen.color := style.PenColor;
  FCanvas.Pen.Width := style.penWidth;
  FCanvas.Brush.color := style.BrushColor;

  for i := 1 to points do
  begin
    theta := pi * ((i - 1) / (points / 2)) + DegToRad(Rotate);
    x := xcenter + (Radius * cos(theta));
    y := ycenter + (Radius * sin(theta));
    Line(xcenter, ycenter, x, y, clWhite, 1);
    PArrow[i] := Point(Round(x), Round(y));
  end;
  FCanvas.Polygon(PArrow);

  FCanvas.Brush.color := beforeBrushColor;
  FCanvas.Pen.color := beforePenColor;
  FCanvas.Pen.Width := beforePenWidth;
end;

constructor TGDIRenderer.Create(const aCanvas: TCanvas; const rect: TRect);
begin
  Assert(Assigned(aCanvas));
  FCanvas := aCanvas;
end;

end.
