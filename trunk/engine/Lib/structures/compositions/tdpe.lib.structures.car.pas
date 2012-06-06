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
unit tdpe.lib.structures.car;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.wheel, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render;

type
  TCar = class(TGroup)
  private
    FwheelparticleA: TWheelParticle;
    FwheelparticleB: TWheelParticle;
    Fwheelconnector: TSpringRestriction;
    function GetSpeed: Double;
    procedure SetSpeed(const Value: Double);
  public
    Constructor Create(aRenderer: TAbstractRenderer; aEngine: TEngine); Reintroduce;
    destructor Destroy(); override;
    property Speed: Double read GetSpeed Write SetSpeed;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ Car }

constructor TCar.Create(aRenderer: TAbstractRenderer; aEngine: TEngine);
begin
  inherited Create(True);
  FwheelparticleA := TWheelParticle.Create(aEngine, 140, 500, 30, False, 2);
  FwheelparticleA.SetRenderer(aRenderer);
  FwheelparticleB := TWheelParticle.Create(aEngine, 200, 500, 14, False, 2);
  FwheelparticleB.SetRenderer(aRenderer);
  Fwheelconnector := TSpringRestriction.Create(FwheelparticleA, FwheelparticleB, 0.5, True, 8);
  Fwheelconnector.SetRenderer(aRenderer);

  AddParticle(FwheelparticleA);
  AddParticle(FwheelparticleB);
  AddRestriction(Fwheelconnector);
end;

destructor TCar.Destroy;
begin
  FreeAndNil(FwheelparticleA);
  FreeAndNil(FwheelparticleB);
  FreeAndNil(Fwheelconnector);
  inherited;
end;

function TCar.GetSpeed: Double;
begin
  result := (FwheelparticleA.AngularVelocity + FwheelparticleB.AngularVelocity) / 2;
end;

procedure TCar.SetSpeed(const Value: Double);
begin
  FwheelparticleA.AngularVelocity := Value;
  FwheelparticleB.AngularVelocity := Value;
end;

end.
