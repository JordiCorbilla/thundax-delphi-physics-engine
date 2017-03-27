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
unit tdpe.lib.particle.circle;

interface

Uses
  tdpe.lib.particle.abstractparticle, tdpe.lib.vector, tdpe.lib.math.interval, FMX.Graphics, System.UITypes, System.Types, System.UIConsts;

Type
  TCircleParticle = class(TAbstractParticle)
  Private
    FRadius: Double;
    FPrintTrace: Boolean;
    FStartDrag: Boolean;
    FDensity: Double;
    procedure SetPrintTrace(const Value: Boolean);
    procedure SetStartDrag(const Value: Boolean);
    procedure SetDensity(const Value: Double);
    function CalculateMass: Double;
  Public
    Constructor Create(x, y, radius: Double; Fixed: Boolean = False; Mass: Double = 1; Elasticity: Double = 0.3; Friction: Double = 0; color: TColor = claBlue); Reintroduce;
    Procedure CleanUp; Override;
    Procedure Init; Override;
    Procedure Paint; OVerride;
    Function GetProjection(AnAxis: IVector): TInterval;
    Function GetIntervalX: TInterval;
    Function GetIntervalY: TInterval;
    Property radius: Double read FRadius Write FRadius;
    property PrintTrace: Boolean read FPrintTrace write SetPrintTrace;
    property StartDrag: Boolean read FStartDrag write SetStartDrag;
    property Density: Double read FDensity write SetDensity;
    function isInside(x, y: Integer): Boolean;
    procedure Move(x, y: Integer);
  End;

implementation

uses
  SysUtils, math;

{ TCircleParticle }

procedure TCircleParticle.CleanUp;
begin
  inherited;

end;

constructor TCircleParticle.Create(x, y, radius: Double; Fixed: Boolean; Mass, Elasticity, Friction: Double; color: TColor);
begin
  inherited Create(x, y, Mass, Elasticity, Friction, Fixed, color);
  FRadius := radius;
  FPrintTrace := False;
end;

function TCircleParticle.GetIntervalX: TInterval;
begin
  interval.min := current.x - FRadius;
  interval.max := current.x + FRadius;
  result := interval;
end;

function TCircleParticle.GetIntervalY: TInterval;
begin
  interval.min := current.y - FRadius;
  interval.max := current.y + FRadius;
  result := interval;
end;

function TCircleParticle.GetProjection(AnAxis: IVector): TInterval;
var
  c: Double;
begin
  c := Sample.InnerProduct(AnAxis);
  interval.min := c - FRadius;
  interval.max := c + FRadius;
  result := interval;
end;

procedure TCircleParticle.Init;
begin
  inherited;
end;

function TCircleParticle.isInside(x, y: Integer): Boolean;
begin
  result := (Sqrt(sqr(current.x - x) + sqr(current.y - y)) <= Self.FRadius);
end;

procedure TCircleParticle.Move(x, y: Integer);
begin
  current.x := x;
  current.y := y;
  if Fixed then
  begin
    Previous.x := x;
    Previous.y := y;
  end;
end;

procedure TCircleParticle.Paint;
var
  vec: IVector;
begin
  inherited;
  if Self.Visible then
  begin
    Renderer.CircleApproximation(current.x, current.y, FRadius, 0, Style);
//    Renderer.Line(Self.Previous.x, Previous.y, Self.current.x, Self.current.y, clared, 1);
//    vec := TVector.New.SetPoint(Self.current.x + 20 * mtdres.x, current.y + 20 * mtdres.y);
//    Renderer.Line(Self.current.x, current.y, vec.x, vec.y, claBlue, 1);
//    Renderer.drawArrow(current, vec);
//    if FPrintTrace then
//      Renderer.DrawPoint(px, py, clared);
  end;
end;

procedure TCircleParticle.SetDensity(const Value: Double);
begin
  FDensity := Value;
  Mass := CalculateMass();
end;

procedure TCircleParticle.SetPrintTrace(const Value: Boolean);
begin
  FPrintTrace := Value;
end;

function TCircleParticle.CalculateMass(): Double;
var
  area: Double;
  m: Double;
begin
  area := Pi * Power(radius, 2);
  m := FDensity * area;
  result := m;
end;

procedure TCircleParticle.SetStartDrag(const Value: Boolean);
begin
  FStartDrag := Value;
end;

end.
