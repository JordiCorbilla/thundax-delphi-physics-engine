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
unit tdpe.lib.particle.circle.solid;

interface

uses
  tdpe.lib.particle.item.solid, FMX.Graphics, tdpe.lib.styles, tdpe.lib.structures,
  tdpe.lib.math.interval, tdpe.lib.vector, tdpe.lib.styles.contract, System.UITypes, System.Types, System.UIConsts;

type
  TSolidCircle = class(TSolidItem)
  private
    Fradius: double;
    FStyle: IStyle;
    FStartDrag: Boolean;
    procedure SetStyle(const Value: IStyle);
    procedure SetStartDrag(const Value: Boolean);
  public
    Constructor Create(x, y, radius: double; isFixed: Boolean; Mass: double = -1; Elasticity: double = 0.3; Friction: double = 0; radian: double = 0; angularVelocity: double = 0;
      color: TColor = claBlack); reintroduce;
    function radius(): double;
    Procedure Paint; override;
    Destructor Destroy(); override;
    Function GetIntervalX: TInterval;
    Function GetIntervalY: TInterval;
    procedure SetAxes(const Value: double); override;
    Function GetProjection(AnAxis: IVector): TInterval;
    function getVertices(axis: TArrayVector): TArrayVector;
    function isInside(x, y: Integer): Boolean;
    procedure Move(x, y: Integer);
    property Style: IStyle read FStyle write SetStyle;
    property StartDrag: Boolean read FStartDrag write SetStartDrag;
  end;

implementation

uses
  math, SysUtils;

{ TSolidCircle }

constructor TSolidCircle.Create(x, y, radius: double; isFixed: Boolean; Mass, Elasticity, Friction, radian, angularVelocity: double; color: TColor);
begin
  Fradius := radius;
  if Mass = -1 then
    Mass := PI * sqr(radius);

  inherited Create(x, y, radius, isFixed, Mass, Mass * (sqr(radius) / 2), Elasticity, Friction, radian, angularVelocity);
  FStyle := TStyle.Create();
  FStyle.BrushColor := color;
  FStyle.PenColor := color;
  FStyle.PenWidth := 1;
end;

destructor TSolidCircle.Destroy;
begin
  FStyle := nil;
  inherited;
end;

function TSolidCircle.GetIntervalX: TInterval;
begin
  interval.min := Sample.x - Fradius;
  interval.max := Sample.x + Fradius;
  Result := interval;
end;

function TSolidCircle.GetIntervalY: TInterval;
begin
  interval.min := Sample.y - Fradius;
  interval.max := Sample.y + Fradius;
  Result := interval;
end;

function TSolidCircle.GetProjection(AnAxis: IVector): TInterval;
var
  c: double;
begin
  c := Sample.InnerProduct(AnAxis);
  interval.min := c - Fradius;
  interval.max := c + Fradius;
  Result := interval;
end;

function TSolidCircle.getVertices(axis: TArrayVector): TArrayVector;
var
  ar1: TArrayVector;
  i: Integer;
  vec: IVector;
begin
  SetLength(ar1, 0);
  for i := 0 to Length(axis) - 1 do
  begin
    vec := TVector.New;
    vec.Clone(axis[i].ScalarProduct(radius));
    TArrayHelper.SetArrayValue(ar1, vec.Add(Sample, true));
  end;
  Result := ar1;
end;

function TSolidCircle.isInside(x, y: Integer): Boolean;
begin
  Result := (Sqrt(sqr(current.x - x) + sqr(current.y - y)) <= Self.Fradius);
end;

procedure TSolidCircle.Move(x, y: Integer);
begin
  current.x := x;
  current.y := y;
  if Fixed then
  begin
    Previous.x := x;
    Previous.y := y;
  end;
end;

procedure TSolidCircle.Paint;
begin
  inherited;
  Renderer.circle(current.x, current.y, Fradius, radian, FStyle);
end;

function TSolidCircle.radius: double;
begin
  Result := Fradius;
end;

procedure TSolidCircle.SetAxes(const Value: double);
begin
  inherited;

end;

procedure TSolidCircle.SetStartDrag(const Value: Boolean);
begin
  FStartDrag := Value;
end;

procedure TSolidCircle.SetStyle(const Value: IStyle);
begin
  FStyle := Value;
end;

end.
