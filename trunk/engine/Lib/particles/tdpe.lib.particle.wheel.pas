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
unit tdpe.lib.particle.wheel;

interface

Uses tdpe.lib.particle.wheel.rim, tdpe.lib.particle.circle, tdpe.lib.engine, tdpe.lib.vector,
  Math, tdpe.lib.Math.helper, tdpe.lib.force.list,
  tdpe.lib.particle.abstractparticle, Classes, tdpe.lib.force.contract;

Type
  TWheelParticle = class(TCircleParticle)
  Private
    FRimParticle: TRimParticle;
    Ftangent: IVector;
    FnormalSlip: IVector;
    Forientation: IVector;
    Ftraction: Double;
    function GetAngularVelocity: Double;
    function GetSpeed: Double;
    function GetTraction: Double;
    procedure SetAngularVelocity(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetTraction(const Value: Double);
    function GetRadian: Double;
    function GetAngle: Double;
  Public
    Constructor Create(aEngine: TEngine; x, y, Radius: Double; fixed: Boolean = False; Mass: Double = 1; Elasticity: Double = 0.3; Friction: Double = 0; Traction: Double = 1); reintroduce;
    Procedure UpdateGeometricState(deltaTime: Double; MassLessForce: TForceList; Damping: Double); Override;
    Procedure ResolveCollision(mtd, velocity, normal: IVector; d: Double; o: Integer; particle: TAbstractParticle); Override;
    Procedure Resolve(normal: IVector);
    Property Speed: Double read GetSpeed Write SetSpeed;
    Property AngularVelocity: Double Read GetAngularVelocity Write SetAngularVelocity;
    Property Traction: Double read GetTraction Write SetTraction;
    property Angle: Double read GetAngle;
    property Radian: Double Read GetRadian;
    function ToString(): String; override;
    Procedure Paint; OVerride;
  End;

implementation

uses
  SysUtils;

{ TWheelParticle }

constructor TWheelParticle.Create(aEngine: TEngine; x, y, Radius: Double; fixed: Boolean; Mass, Elasticity, Friction, Traction: Double);
begin
  Inherited Create(x, y, Radius, fixed, Mass, Elasticity, Friction);
  Ftangent := TVector.New;
  FnormalSlip := TVector.New;
  FRimParticle := TRimParticle.Create(aEngine, Radius, 2);
  Self.Traction := Traction;
  Forientation := TVector.New;
end;

function TWheelParticle.GetAngle: Double;
begin
  Result := Radian * ONE_EIGHTY_OVER_PI;
end;

function TWheelParticle.GetAngularVelocity: Double;
begin
  Result := FRimParticle.AngularVelocity;
end;

function TWheelParticle.GetRadian: Double;
begin
  Forientation.SetPoint(FRimParticle.current.x, FRimParticle.current.y);
  Result := ArcTan2(Forientation.y, Forientation.x) + Pi;
end;

function TWheelParticle.GetSpeed: Double;
begin
  Result := FRimParticle.Speed;
end;

function TWheelParticle.GetTraction: Double;
begin
  Result := 1 - Ftraction;
end;

procedure TWheelParticle.Paint;
begin
  inherited;
end;

procedure TWheelParticle.Resolve(normal: IVector);
var
  projectedSpeed: Double;
  wheelSurfaceVelocity, combinedVelocity: IVector;
  SlipSpeed: Double;
begin
  Ftangent.SetPoint(-FRimParticle.current.y, FRimParticle.current.x);
  Ftangent := Ftangent.Normalise;
  wheelSurfaceVelocity := Ftangent.ScalarProduct(FRimParticle.Speed);
  combinedVelocity := velocity.Add(wheelSurfaceVelocity, true);
  projectedSpeed := combinedVelocity.OuterProduct(normal);
  Ftangent.ScalarProduct(projectedSpeed, true);
  FRimParticle.previous.Clone(FRimParticle.current.Substract(Ftangent));
  SlipSpeed := (1 - Ftraction) * FRimParticle.Speed;
  FnormalSlip.SetPoint(SlipSpeed * normal.y, SlipSpeed * normal.x);
  current.Add(FnormalSlip, true);
  FRimParticle.Speed := FRimParticle.Speed * Ftraction;
end;

procedure TWheelParticle.ResolveCollision(mtd, velocity, normal: IVector; d: Double; o: Integer; particle: TAbstractParticle);
begin
  inherited ResolveCollision(mtd, velocity, normal, d, o, particle);
  Resolve(normal.ScalarProduct(Sign(d * o)));
end;

procedure TWheelParticle.SetAngularVelocity(const Value: Double);
begin
  FRimParticle.AngularVelocity := Value;
end;

procedure TWheelParticle.SetSpeed(const Value: Double);
begin
  FRimParticle.Speed := Value;
end;

procedure TWheelParticle.SetTraction(const Value: Double);
begin
  Ftraction := 1 - Value;
end;

function TWheelParticle.ToString: String;
begin
  Result := 'Angle: ' + FloatToStr(Self.AngularVelocity);
end;

procedure TWheelParticle.UpdateGeometricState(deltaTime: Double; MassLessForce: TForceList; Damping: Double);
begin
  Inherited UpdateGeometricState(deltaTime, MassLessForce, Damping);
  FRimParticle.UpdateGeometricState(deltaTime);
end;

end.
