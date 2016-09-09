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
unit tdpe.lib.particle.item.solid;

interface

uses
  tdpe.lib.particle.abstractparticle, tdpe.lib.vector, FMX.Graphics, Classes,
  tdpe.lib.force.contract, Generics.Collections, tdpe.lib.force.list, System.UITypes, System.Types, System.UIConsts;

type
  TSolidItem = class(TAbstractParticle)
  private
    Fmi: Double;
    FangularVelocity: Double;
    FfrictionalCoefficient: Double;
    FprevRadian: Double;
    Fradian: Double;
    FRange: Double;
    Ftorque: Double;
    procedure SetangularVelocity(const Value: Double);
    procedure SetfrictionalCoefficient(const Value: Double);
    procedure Setmi(const Value: Double);
    procedure SetprevRadian(const Value: Double);
    procedure Setradian(const Value: Double);
    procedure SetRange(const Value: Double);
    procedure Settorque(const Value: Double);
    function GetAngle: Double;
    procedure SetAngle(const Value: Double);
  public
    property angularVelocity: Double read FangularVelocity write SetangularVelocity;
    property frictionalCoefficient: Double read FfrictionalCoefficient write SetfrictionalCoefficient;
    property radian: Double read Fradian write Setradian;
    property prevRadian: Double read FprevRadian write SetprevRadian;
    property torque: Double read Ftorque write Settorque;
    property Range: Double read FRange write SetRange;
    property mi: Double read Fmi write Setmi;
    property Angle: Double read GetAngle write SetAngle;
    constructor Create(x, y, Range: Double; isFixed: Boolean; mass: Double = 1; mi: Double = -1; elasticity: Double = 0.3; friction: Double = 0.2; radian: Double = 0; angularVelocity: Double = 0;
      color: TColor = claWhite); reintroduce;
    Procedure Init; override;
    Procedure Paint; override;
    Procedure CleanUp; override;
    procedure SetAxes(const Value: Double); virtual; abstract;
    Procedure UpdateGeometricState(deltaTime: Double; MassLEssForce: TForceList; Damping: Double); Override;
    procedure addTorque(torque: Double);
    procedure resolveRigidCollision(torque: Double; particle: TAbstractParticle);
    function Captures(vertex: IVector): Boolean; virtual;
    function getVelocityOn(vertex: IVector): IVector;
  end;

implementation

uses
  tdpe.lib.math.helper, tdpe.lib.math;

{ TSolidItem }

procedure TSolidItem.addTorque(torque: Double);
begin
  angularVelocity := angularVelocity + torque;
end;

function TSolidItem.Captures(vertex: IVector): Boolean;
var
  d: Double;
begin
  d := vertex.Distance(sample) - Range;
  result := TMathHelper.compare(d, 0, '<=');
end;

procedure TSolidItem.CleanUp;
begin
  inherited;

end;

constructor TSolidItem.Create(x, y, Range: Double; isFixed: Boolean; mass, mi, elasticity, friction, radian, angularVelocity: Double; color: TColor);
begin
  inherited Create(x, y, mass, elasticity, friction, isFixed, color);
  Self.FRange := Range;
  Self.FfrictionalCoefficient := friction;
  Self.Fradian := radian;
  Self.FangularVelocity := angularVelocity;
  Self.Ftorque := 0;
  if isFixed then
  begin
    Self.mass := High(integer);
    Self.Fmi := High(integer);
  end
  else if mi = -1 then
    Self.Fmi := mass
  else
    Self.Fmi := mi;
end;

function TSolidItem.GetAngle: Double;
begin
  result := radian * ONE_EIGHTY_OVER_PI;
end;

function TSolidItem.getVelocityOn(vertex: IVector): IVector;
var
  sampleVector: IVector;
  vector: IVector;
  w0: Double;
  VectorResultant: IVector;
begin
  sampleVector := vertex.Substract(sample);
  vector := sampleVector.Normalise;
  w0 := angularVelocity * sampleVector.Magnitude;
  VectorResultant := TVector.New.SetPoint(-vector.y, vector.x);
  VectorResultant.ScalarProduct(w0, true);
  result := VectorResultant.Add(Velocity, true);
end;

procedure TSolidItem.Init;
begin
  inherited;

end;

procedure TSolidItem.Paint;
begin
  inherited;

end;

procedure TSolidItem.resolveRigidCollision(torque: Double; particle: TAbstractParticle);
begin
  if (Fixed) or (not solid) or (not particle.solid) then
    Exit;
  addTorque(torque);
end;

procedure TSolidItem.SetAngle(const Value: Double);
begin
  radian := Value * PI_OVER_ONE_EIGHTY;
end;

procedure TSolidItem.SetangularVelocity(const Value: Double);
begin
  FangularVelocity := Value;
end;

procedure TSolidItem.SetfrictionalCoefficient(const Value: Double);
begin
  FfrictionalCoefficient := Value;
end;

procedure TSolidItem.Setmi(const Value: Double);
begin
  Fmi := Value;
end;

procedure TSolidItem.SetprevRadian(const Value: Double);
begin
  FprevRadian := Value;
end;

procedure TSolidItem.Setradian(const Value: Double);
begin
  Fradian := Value;
  SetAxes(Value);
end;

procedure TSolidItem.SetRange(const Value: Double);
begin
  FRange := Value;
end;

procedure TSolidItem.Settorque(const Value: Double);
begin
  Ftorque := Value;
end;

procedure TSolidItem.UpdateGeometricState(deltaTime: Double; MassLEssForce: TForceList; Damping: Double);
begin
  radian := radian + (angularVelocity * Damping);
  inherited UpdateGeometricState(deltaTime, MassLEssForce, Damping);
  torque := 0;
end;

end.
