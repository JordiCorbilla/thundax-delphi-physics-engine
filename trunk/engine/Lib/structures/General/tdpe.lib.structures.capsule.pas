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
unit tdpe.lib.structures.capsule;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle.solid,
  tdpe.lib.particle.spring.restriction, tdpe.lib.engine, tdpe.lib.render;

Type
  TCapsule = class(TGroup)
  Private
    capsuleP1: TSolidCircle;
    capsuleP2: TSolidCircle;
    capsule: TSpringRestriction;
  Public
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine); reintroduce; Virtual;
    destructor Destroy(); override;
  End;

implementation

uses
  SysUtils, Graphics;

{ TCapsule }

constructor TCapsule.Create(render: TAbstractRenderer; aEngine: TEngine);
begin
  inherited Create;
  capsuleP1 := TSolidCircle.Create(300, 10, 14, false, 1.3, 0.4);
  capsuleP1.SetParticleColor(clRed);
  capsuleP1.SetRenderer(render);
  addParticle(capsuleP1);

  capsuleP2 := TSolidCircle.Create(325, 35, 14, false, 1.3, 0.4);
  capsuleP2.SetParticleColor(clRed);
  capsuleP2.SetRenderer(render);
  addParticle(capsuleP2);

  capsule := TSpringRestriction.Create(capsuleP1, capsuleP2, 1, true, 24);
  capsule.SetRenderer(render);
  addRestriction(capsule);
end;

destructor TCapsule.Destroy;
begin
  FreeAndNil(capsuleP1);
  FreeAndNil(capsuleP2);
  FreeAndNil(capsule);
  inherited;
end;

end.
