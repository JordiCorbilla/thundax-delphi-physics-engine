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
unit tdpe.lib.automation.rotator;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle,
  tdpe.lib.particle.spring.restriction, tdpe.lib.engine,
  tdpe.lib.particle.box, tdpe.lib.render,
  tdpe.lib.structures.boxcomposite, tdpe.lib.vector;

Type
  TRotator = class(TGroup)
  Private
    ctr: IVector;
    FBoxComposite: TBoxComposite;
    circA: TCircleParticle;
    rectA: TBoxParticle;
    rectB: TBoxParticle;
    ConnectorA: TSpringRestriction;
    ConnectorB: TSpringRestriction;
  Public
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine); reintroduce; Virtual;
    destructor Destroy(); override;
    function RotateByRadian(a: Double): Double;
  End;

implementation

uses
  SysUtils;

{ TRotator }

constructor TRotator.Create(render: TAbstractRenderer; aEngine: TEngine);
begin
  inherited Create;
  collideInternal := true;

  ctr := TVector.New.SetPoint(555, 175);
  FBoxComposite := TBoxComposite.Create(render, aEngine, ctr);
  addComposite(FBoxComposite);

  circA := TCircleParticle.Create(ctr.x, ctr.y, 5, false);
  circA.SetRenderer(render);
  addParticle(circA);

  rectA := TBoxParticle.Create(555, 160, 10, 10, 0, false, 3);
  rectA.SetRenderer(render);
  addParticle(rectA);

  ConnectorA := TSpringRestriction.Create(FBoxComposite.CpA, rectA, 1);
  ConnectorA.SetRenderer(render);
  addRestriction(ConnectorA);

  rectB := TBoxParticle.Create(555, 190, 10, 10, 0, false, 3);
  rectB.SetRenderer(render);
  addParticle(rectB);

  ConnectorB := TSpringRestriction.Create(FBoxComposite.cpc, rectB, 1);
  ConnectorB.SetRenderer(render);
  addRestriction(ConnectorB);
end;

destructor TRotator.Destroy;
begin
  FreeAndNil(FBoxComposite);
  FreeAndNil(circA);
  FreeAndNil(rectA);
  FreeAndNil(rectB);
  FreeAndNil(ConnectorA);
  FreeAndNil(ConnectorB);
  inherited;
end;

function TRotator.RotateByRadian(a: Double): Double;
begin
  FBoxComposite.RotateByRadian(a, ctr);
  Result := 0.0;
end;

end.
