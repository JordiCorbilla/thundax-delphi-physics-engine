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
unit tdpe.lib.particle.wheel.rim;

interface

Uses tdpe.lib.vector, Math, tdpe.lib.engine;

type
  TRimParticle = class(TObject)
  Private
    Fwr: Double;
    FAngularVelocity: Double;
    FSpeed: Double;
    FmaxTorque: Double;
    FTEngine: TEngine;
  Public
    current: IVector;
    previous: IVector;
    Constructor Create(anTEngine: TEngine; AngularRadius: Double; MaxTorque: Double);
    Procedure UpdateGeometricState(dt: Double);
    Property Speed: Double read FSpeed Write FSpeed;
    property AngularVelocity: Double read FAngularVelocity Write FAngularVelocity;
  End;

implementation

{ TRimParticle }

(* Origins of this code are from Raigan Burns, Metanet Software *)

constructor TRimParticle.Create(anTEngine: TEngine; AngularRadius: Double; MaxTorque: Double);
begin
  current := TVector.New.SetPoint(AngularRadius, 0);
  previous := TVector.New;
  FSpeed := 0;
  FAngularVelocity := 0;
  FTEngine := anTEngine;
  FmaxTorque := MaxTorque;
  Fwr := AngularRadius;
end;

procedure TRimParticle.UpdateGeometricState(dt: Double);
var
  Tangentx, Tangenty: Double;
  TangentLen, Previousx, Previousy: Double;
  px, py, rimPlace, diff: Double;
begin
  FSpeed := max(-FmaxTorque, min(FmaxTorque, FSpeed + FAngularVelocity));
  Tangentx := -current.y;
  Tangenty := current.x;
  TangentLen := sqrt(Tangentx * Tangentx + Tangenty * Tangenty);
  If TangentLen <> 0 then
  begin
    Tangentx := Tangentx / TangentLen;
    Tangenty := Tangenty / TangentLen;
  end;
  current.x := current.x + FSpeed * Tangentx;
  current.y := current.y + FSpeed * Tangenty;
  Previousx := previous.x;
  Previousy := previous.y;
  previous.x := current.x;
  previous.y := current.y;
  px := previous.x;
  py := previous.y;
  current.x := current.x + FTEngine.damping * (px - Previousx);
  current.y := current.y + FTEngine.damping * (py - Previousy);
  rimPlace := sqrt(current.x * current.x + current.y * current.y);
  if rimPlace <> 0 then
  Begin
    diff := (rimPlace - Fwr) / rimPlace;
    current.x := current.x - (current.x * diff);
    current.y := current.y - (current.y * diff);
  end;
end;

end.
