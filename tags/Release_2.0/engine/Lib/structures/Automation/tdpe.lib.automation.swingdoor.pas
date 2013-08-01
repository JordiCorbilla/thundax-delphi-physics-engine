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
unit tdpe.lib.automation.swingdoor;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.box, tdpe.lib.particle.circle.solid,
  tdpe.lib.particle.spring.restriction, tdpe.lib.engine, tdpe.lib.render;

Type
  TSwingdoor = class(TGroup)
  Private
    swingDoorP1: TSolidCircle;
    swingDoorP2: TSolidCircle;
    swingDoorAnchor: TSolidCircle;
    swingdoor: TSpringRestriction;
    swingDoorSpring: TSpringRestriction;
    StopperA: TSolidCircle;
    StopperB: TBoxParticle;
  Public
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine); reintroduce; Virtual;
    destructor Destroy(); override;
  End;

implementation

uses
  SysUtils;

{ TSwingdoor }

constructor TSwingdoor.Create(render: TAbstractRenderer; aEngine: TEngine);
begin
  inherited Create;
  collideInternal := true;

  swingDoorP1 := TSolidCircle.Create(543, 55, 7, false);
  swingDoorP1.mass := 0.001;
  swingDoorP1.SetRenderer(render);
  addParticle(swingDoorP1);

  swingDoorP2 := TSolidCircle.Create(620, 55, 7, true);
  swingDoorP2.SetRenderer(render);
  addParticle(swingDoorP2);

  swingdoor := TSpringRestriction.Create(swingDoorP1, swingDoorP2, 1, true, 13);
  swingdoor.SetRenderer(render);
  addRestriction(swingdoor);

  swingDoorAnchor := TSolidCircle.Create(543, 5, 2, true);
  swingDoorAnchor.visible := true;
  swingDoorAnchor.collidable := false;
  swingDoorAnchor.SetRenderer(render);
  addParticle(swingDoorAnchor);

  swingDoorSpring := TSpringRestriction.Create(swingDoorP1, swingDoorAnchor, 0.02);
  swingDoorSpring.restLength := 40;
  swingDoorSpring.visible := true;
  swingDoorSpring.SetRenderer(render);
  addRestriction(swingDoorSpring);

  StopperA := TSolidCircle.Create(550, -60, 70, true);
  StopperA.visible := true;
  StopperA.SetRenderer(render);
  addParticle(StopperA);

  StopperB := TBoxParticle.Create(650, 130, 42, 70, 0, true);
  StopperB.visible := true;
  StopperB.SetRenderer(render);
  addParticle(StopperB);
end;

destructor TSwingdoor.Destroy;
begin
  FreeAndNil(swingDoorP1);
  FreeAndNil(swingDoorP2);
  FreeAndNil(swingDoorAnchor);
  FreeAndNil(swingdoor);
  FreeAndNil(swingDoorSpring);
  FreeAndNil(StopperA);
  FreeAndNil(StopperB);
  inherited;
end;

end.
