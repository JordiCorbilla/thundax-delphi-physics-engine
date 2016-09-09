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
unit tdpe.lib.structures.boxcomposite;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle,
  tdpe.lib.particle.spring.restriction, tdpe.lib.engine, tdpe.lib.render,
  tdpe.lib.particle.pattern.composite, tdpe.lib.vector;

Type
  TBoxComposite = class(TComposite)
  Private
  Public
    CpA: TCircleParticle;
    CpC: TCircleParticle;
    cpB: TCircleParticle;
    cpD: TCircleParticle;
    spra: TSpringRestriction;
    sprb: TSpringRestriction;
    sprc: TSpringRestriction;
    sprd: TSpringRestriction;
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine; Ctr: IVector); reintroduce; Virtual;
    destructor Destroy(); override;
  End;

implementation

uses
  SysUtils, FMX.Graphics, System.UITypes, System.Types, System.UIConsts;

{ TBoxComposite }

constructor TBoxComposite.Create(render: TAbstractRenderer; aEngine: TEngine; Ctr: IVector);
var
  rw, rh, rad: Double;
begin
  Inherited Create;
  rw := 75;
  rh := 18;
  rad := 4;

  CpA := TCircleParticle.Create(Ctr.x - rw / 2, Ctr.y - rh / 2, rad, true, 1, 0.3, 0, clawhite);
  cpB := TCircleParticle.Create(Ctr.x + rw / 2, Ctr.y - rh / 2, rad, true, 1, 0.3, 0, clawhite);
  CpC := TCircleParticle.Create(Ctr.x + rw / 2, Ctr.y + rh / 2, rad, true, 1, 0.3, 0, clawhite);
  cpD := TCircleParticle.Create(Ctr.x - rw / 2, Ctr.y + rh / 2, rad, true, 1, 0.3, 0, clawhite);

  CpA.SetRenderer(render);
  cpB.SetRenderer(render);
  CpC.SetRenderer(render);
  cpD.SetRenderer(render);

  spra := TSpringRestriction.Create(CpA, cpB, 0.5, true, rad * 2, 1, true, claBlue, 3);
  sprb := TSpringRestriction.Create(cpB, CpC, 0.5, true, rad * 2, 1, False, claBlue, 3);
  sprc := TSpringRestriction.Create(CpC, cpD, 0.5, true, rad * 2, 1, False, claBlue, 3);
  sprd := TSpringRestriction.Create(cpD, CpA, 0.5, true, rad * 2, 1, False, claBlue, 3);

  spra.SetRenderer(render);
  sprb.SetRenderer(render);
  sprc.SetRenderer(render);
  sprd.SetRenderer(render);

  addParticle(CpA);
  addParticle(cpB);
  addParticle(CpC);
  addParticle(cpD);

  addRestriction(spra);
  addRestriction(sprb);
  addRestriction(sprc);
  addRestriction(sprd);
end;

destructor TBoxComposite.Destroy;
begin
  FreeAndNil(CpA);
  FreeAndNil(CpC);
  FreeAndNil(cpB);
  FreeAndNil(cpD);
  FreeAndNil(spra);
  FreeAndNil(sprb);
  FreeAndNil(sprc);
  FreeAndNil(sprd);
  inherited;
end;

end.
