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
unit tdpe.lib.particle.circle.mass;

interface

uses
  tdpe.lib.particle.circle, tdpe.lib.vector, FMX.Graphics, tdpe.lib.force.contract,
  tdpe.lib.force.list, System.UITypes, System.Types, SYstem.UICOnsts;

type
  TCircleMass = class(TCircleParticle)
  private
    FangularVelocity: Double;
    FnetTorque: Double;
    FinvertedIntertia: Double;
    FInertia: Double;
    Fradian: Double;
    Fangle: Double;
    procedure SetangularVelocity(const Value: Double);
    procedure SetInertia(const Value: Double);
    procedure SetinvertedIntertia(const Value: Double);
    procedure SetnetTorque(const Value: Double);
    procedure Setradian(const Value: Double);
    procedure Setangle(const Value: Double);
    function getangle(): Double;
  public
    property netTorque: Double read FnetTorque write SetnetTorque;
    property angularVelocity: Double read FangularVelocity write SetangularVelocity;
    property invertedIntertia: Double read FinvertedIntertia write SetinvertedIntertia;
    property Inertia: Double read FInertia write SetInertia;
    property radian: Double read Fradian write Setradian;
    property angle: Double read getangle write Setangle;
    constructor Create(x: Double; y: Double; radius: Double; fixedPosition: Boolean = false; mass: Double = 1; elasticity: Double = 0.15; friction: Double = 0.1; color: TColor = claWhite); reintroduce;
    Procedure CleanUp; Override;
    Procedure Init; Override;
    Procedure Paint; OVerride;
    Procedure UpdateGeometricState(dt2: Double; MassLEssForce: TForceList; Damping: Double); Override;
    function calculateInertia(): Double;
    procedure resolveVelocities(dv: IVector; dw: Double; normal: IVector);
  end;

implementation

uses
  Math, SysUtils, tdpe.lib.Math.helper;

{ TCircleMass }

function TCircleMass.calculateInertia: Double;
var
  iner: Double;
begin
  iner := 0.5 * mass * power(radius, 2);
  result := iner;
end;

procedure TCircleMass.CleanUp;
begin
  inherited;

end;

constructor TCircleMass.Create(x, y, radius: Double; fixedPosition: Boolean; mass, elasticity, friction: Double; color: TColor);
begin
  inherited Create(x, y, radius, fixedPosition, mass, elasticity, friction, color);
  Self.Fradian := 0;
  Self.FnetTorque := 0;
  Self.FangularVelocity := 0;
  Self.FInertia := calculateInertia();
end;

function TCircleMass.getangle: Double;
begin
  Fangle := radian * ONE_EIGHTY_OVER_PI;
  result := Fangle;
end;

procedure TCircleMass.Init;
begin
  inherited;

end;

procedure TCircleMass.Paint;
begin
  Renderer.CircleApproximation(current.x, current.y, radius, angle, Style);
end;

procedure TCircleMass.resolveVelocities(dv: IVector; dw: Double; normal: IVector);
begin
  if (not Fixed) then
  begin
    Velocity.Add(dv, true);
    FangularVelocity := FangularVelocity + dw * 0.5;
  end
  else
    FangularVelocity := FangularVelocity + dw * 0.5;
end;

procedure TCircleMass.Setangle(const Value: Double);
begin
  Fangle := Value * PI_OVER_ONE_EIGHTY; ;
end;

procedure TCircleMass.SetangularVelocity(const Value: Double);
begin
  FangularVelocity := Value;
end;

procedure TCircleMass.SetInertia(const Value: Double);
begin
  if (Value <= 0) then
    Raise Exception.Create('inertia may not be set <= 0.');
  FInertia := Value;
  FinvertedIntertia := 1 / Value;
end;

procedure TCircleMass.SetinvertedIntertia(const Value: Double);
begin
  FinvertedIntertia := Value;
end;

procedure TCircleMass.SetnetTorque(const Value: Double);
begin
  FnetTorque := Value;
end;

procedure TCircleMass.Setradian(const Value: Double);
begin
  Fradian := Value;
end;

procedure TCircleMass.UpdateGeometricState(dt2: Double; MassLEssForce: TForceList; Damping: Double);
begin
  if (not Fixed) then
  begin
    Temporal.Clone(current);
    current.Add(Velocity.ScalarProduct(Damping, true), true);
    Previous.Clone(Temporal);
    accumulateForces(MassLEssForce);
    Velocity.Add(Forces.ScalarProduct(dt2, true), true);
    Forces.SetPoint(0, 0);
  end;
  radian := radian - FangularVelocity;
  FangularVelocity := FangularVelocity - (netTorque * FinvertedIntertia * dt2);
  FnetTorque := 0;
end;

end.
