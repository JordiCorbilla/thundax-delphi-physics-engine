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
unit tdpe.lib.particle.box.solid;

interface

uses
  tdpe.lib.particle.item.solid, tdpe.lib.particle.box, FMX.Graphics, tdpe.lib.vector, tdpe.lib.math.interval, System.UITypes, System.Types, System.UIConsts;

type
  TSolidBox = class(TSolidItem)
  Private
    Fextents: TRctTypeDouble;
    Faxes: TRctTypeVector;
    FNormals: TRctTypeVector2;
    FVertices: TRctTypeVector2;
    FMarginCenters: TRctTypeVector2;
    FStartDrag: Boolean;
    FWidth: double;
    Fheight: double;
    Fcolor: TColor;
    procedure SetStartDrag(const Value: Boolean);
    procedure Setheight(const Value: double);
    procedure SetWidth(const Value: double);
  Public
    Constructor Create(x, y, awidth, aheight, rotation: double; Fixed: Boolean; Mass: double = 1; Elasticity: double = 0.3; Friction: double = 0; angularVelocity: double = 0;
      color: TColor = claBlack); reintroduce;
    Destructor Destroy(); override;
    Procedure Init; Override;
    Procedure Paint; OVerride;
    procedure SetAxes(const Value: double); override;
    function getVertices(): TRctTypeVector2;
    Function GetProjection(AnAxis: IVector): TInterval;
    Procedure CleanUp; Override;
    Property normals: TRctTypeVector2 read FNormals;
    Property vertices: TRctTypeVector2 read getVertices;
    Property marginCenters: TRctTypeVector2 read FMarginCenters;
    Property Axes: TRctTypeVector read Faxes;
    Property Extents: TRctTypeDouble read Fextents Write Fextents;
    property StartDrag: Boolean read FStartDrag write SetStartDrag;
    property Width: double read FWidth write SetWidth;
    property height: double read Fheight write Setheight;
    function isInside(x, y: Integer): Boolean;
    procedure Move(x, y: Integer);
    function Captures(vertex: IVector): Boolean; override;
    class function New(x, y, awidth, aheight, rotation: double; Fixed: Boolean; Mass: double = 1; Elasticity: double = 0.3; Friction: double = 0; angularVelocity: double = 0; color: TColor = claBlack)
        : TSolidBox;
  end;

implementation

uses
  math, tdpe.lib.math;

{ TSolidBox }

function TSolidBox.Captures(vertex: IVector): Boolean;
var
  i: Integer;
  x: double;
  bfound: Boolean;
begin
  i := 0;
  bfound := false;
  while (i < 4) and (not bfound) do
  begin
    x := vertex.Substract(FMarginCenters[i].Add(sample)).InnerProduct(FNormals[i]);
    i := i + 1;
    bfound := TMathHelper.Compare(x, 0.01, '>');
  end;
  Result := not bfound;
end;

procedure TSolidBox.CleanUp;
begin
  inherited;

end;

constructor TSolidBox.Create(x, y, awidth, aheight, rotation: double; Fixed: Boolean; Mass, Elasticity, Friction, angularVelocity: double; color: TColor);
var
  i: Integer;
begin
  inherited Create(x, y, sqrt(awidth * (awidth / 4) + aheight * (aheight / 4)), Fixed,
    Mass, Mass * (awidth * awidth + aheight * aheight) / 12, Elasticity, Friction, rotation, angularVelocity, color);
  Fcolor := color;
  if Mass = -1 then
    Self.Mass := awidth * aheight;
  Fextents[0] := awidth / 2;
  Fextents[1] := aheight / 2;
  Fheight := aheight;
  FWidth := awidth;
  Faxes[0] := TVector.New;
  Faxes[1] := TVector.New;
  for i := 0 to 3 do
  begin
    FNormals[i] := TVector.New;
    FMarginCenters[i] := TVector.New;
    FVertices[i] := TVector.New;
  end;
end;

destructor TSolidBox.Destroy;
begin

  inherited;
end;

function TSolidBox.GetProjection(AnAxis: IVector): TInterval;
var
  radius: double;
  c: double;
begin
  radius := Extents[0] * Abs(AnAxis.InnerProduct(Axes[0])) + Extents[1] * Abs(AnAxis.InnerProduct(Axes[1]));
  c := sample.InnerProduct(AnAxis);
  interval.min := c - radius;
  interval.max := c + radius;
  Result := interval;
end;

function TSolidBox.getVertices: TRctTypeVector2;
var
  r: TRctTypeVector2;
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    r[i] := TVector.New;
    r[i].Clone(FVertices[i].Add(sample));
  end;
  Result := r;
end;

procedure TSolidBox.Init;
begin
  inherited;

end;

function TSolidBox.isInside(x, y: Integer): Boolean;
begin
  Result := (sqrt(sqr(current.x - x) + sqr(current.y - y)) <= Self.Width);
end;

procedure TSolidBox.Move(x, y: Integer);
begin
  current.x := x;
  current.y := y;
  if Fixed then
  begin
    Previous.x := x;
    Previous.y := y;
  end;
end;

class function TSolidBox.New(x, y, awidth, aheight, rotation: double; Fixed: Boolean; Mass, Elasticity, Friction, angularVelocity: double; color: TColor): TSolidBox;
begin
  Result := Create(x, y, awidth, aheight, rotation, Fixed, Mass, Elasticity, Friction, angularVelocity, color);
end;

procedure TSolidBox.Paint;
begin
  inherited;
  Renderer.box(px, py, Width, height, angle, Fcolor);
end;

procedure TSolidBox.SetAxes(const Value: double);
var
  c, s: double;
begin
  s := Sin(Value);
  c := Cos(Value);
  Axes[0].SetPoint(c, s);
  Axes[1].SetPoint(-s, c);
  FNormals[0].Clone(Axes[0]);
  FNormals[1].Clone(Axes[1]);
  FNormals[2] := Axes[0].ScalarProduct(-1);
  FNormals[3] := Axes[1].ScalarProduct(-1);
  FMarginCenters[0] := Axes[0].ScalarProduct(Extents[0]);
  FMarginCenters[1] := Axes[1].ScalarProduct(Extents[1]);
  FMarginCenters[2] := Axes[0].ScalarProduct(-Extents[0]);
  FMarginCenters[3] := Axes[1].ScalarProduct(-Extents[1]);
  FVertices[0] := FMarginCenters[0].Add(FMarginCenters[1]);
  FVertices[1] := FMarginCenters[1].Add(FMarginCenters[2]);
  FVertices[2] := FMarginCenters[2].Add(FMarginCenters[3]);
  FVertices[3] := FMarginCenters[3].Add(FMarginCenters[0]);
end;

procedure TSolidBox.Setheight(const Value: double);
begin
  Fheight := Value;
end;

procedure TSolidBox.SetStartDrag(const Value: Boolean);
begin
  FStartDrag := Value;
end;

procedure TSolidBox.SetWidth(const Value: double);
begin
  FWidth := Value;
end;

end.
