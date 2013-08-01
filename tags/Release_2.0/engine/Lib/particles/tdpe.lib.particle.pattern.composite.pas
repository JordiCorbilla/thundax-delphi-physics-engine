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
unit tdpe.lib.particle.pattern.composite;

interface

Uses
  tdpe.lib.vector, tdpe.lib.particle.abstract.collection, Math, tdpe.lib.particle.abstractparticle,
  tdpe.lib.Math.helper, Classes, tdpe.lib.particle.spring.restriction;

Type

  TComposite = Class(TAbstractCollection)
  Private
    Fdelta: IVector;
    function GetFixed: Boolean;
    procedure SetFixed(const Value: Boolean);
  Public
    Constructor Create; Override;
    destructor Destroy(); Override;
    Procedure RotateByRadian(AngleRadian: double; Centre: IVector);
    Procedure RotateByAngle(AngleDegree: double; Centre: IVector);
    Function GetRelativeAngle(Centre, vector: IVector): double;
    Property Fixed: Boolean read GetFixed Write SetFixed;
  end;

implementation

{ TComposite }

constructor TComposite.Create;
begin
  inherited Create;
  Fdelta := TVector.New;
end;

destructor TComposite.Destroy;
begin
  Fdelta := nil;
  inherited;
end;

function TComposite.GetFixed: Boolean;
var
  particle: TAbstractParticle;
begin
  result := true;
  for particle in Particles do
  begin
    if not particle.Fixed then
    begin
      result := False;
      exit;
    end;
  end;
end;

function TComposite.GetRelativeAngle(Centre, vector: IVector): double;
begin
  Fdelta.SetPoint(vector.x - Centre.x, vector.y - Centre.y);
  result := ArcTan2(Fdelta.y, Fdelta.x);
end;

procedure TComposite.RotateByAngle(AngleDegree: double; Centre: IVector);
var
  angleRadians: double;
begin
  angleRadians := AngleDegree * PI_OVER_ONE_EIGHTY;
  RotateByRadian(angleRadians, Centre);
end;

procedure TComposite.RotateByRadian(AngleRadian: double; Centre: IVector);
var
  vector: TAbstractParticle;
  radius, angle: double;
begin
  For vector in Particles do
  begin
    radius := vector.Center.distance(Centre);
    angle := GetRelativeAngle(Centre, vector.Center) + AngleRadian;
    vector.Px := (Cos(angle) * radius) + Centre.x;
    vector.Py := (Sin(angle) * radius) + Centre.y;
  end;
end;

procedure TComposite.SetFixed(const Value: Boolean);
var
  particle : TAbstractParticle;
begin
  for particle in Particles do
    particle.Fixed := Value;
end;

end.
