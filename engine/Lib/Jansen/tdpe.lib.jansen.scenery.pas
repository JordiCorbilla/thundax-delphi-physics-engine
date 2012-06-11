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
unit tdpe.lib.jansen.scenery;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.box, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, Graphics;

type
  TScenery = class(TGroup)
  private
    rec: TBoxParticle;
    limit0, limit1: TBoxParticle;
    box0, box1, box2, box3, box4, box5: TBoxParticle;
  public
    Constructor Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; color: TColor); Reintroduce;
    Destructor Destroy(); override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TScenery }

constructor TScenery.Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; color: TColor);
begin
  inherited Create(true);
  rec := TBoxParticle.Create(450, 590, 1700, 100, 0, true, 1000, 0.3, 0.3, color);
  rec.SetRenderer(aRenderer);
  addParticle(rec);

  limit0 := TBoxParticle.Create(0, 500, 10, 100, 0, true, 1000, 0.3, 0.3, color);
  limit0.SetRenderer(aRenderer);
  addParticle(limit0);

  limit1 := TBoxParticle.Create(1250, 500, 10, 100, 0, true, 1000, 0.3, 0.3, color);
  limit1.SetRenderer(aRenderer);
  addParticle(limit1);

  box0 := TBoxParticle.Create(600, 537, 600, 7, 0, true, 1000, 0.3, 0.3, color);
  box0.SetRenderer(aRenderer);
  addParticle(box0);

  box1 := TBoxParticle.Create(600, 530, 500, 7, 0, true, 1000, 0.3, 0.3, color);
  box1.SetRenderer(aRenderer);
  addParticle(box1);

  box2 := TBoxParticle.Create(600, 523, 400, 7, 0, true, 10, 0.3, 0.3, color);
  box2.SetRenderer(aRenderer);
  addParticle(box2);

  box3 := TBoxParticle.Create(600, 516, 300, 7, 0, true, 10, 0.3, 0.3, color);
  box3.SetRenderer(aRenderer);
  addParticle(box3);

  box4 := TBoxParticle.Create(600, 509, 200, 7, 0, true, 10, 0.3, 0.3, color);
  box4.SetRenderer(aRenderer);
  addParticle(box4);

  box5 := TBoxParticle.Create(600, 502, 100, 7, 0, true, 10, 0.3, 0.3, color);
  box5.SetRenderer(aRenderer);
  addParticle(box5);
end;

destructor TScenery.Destroy;
begin
  FreeAndNil(rec);
  FreeAndNil(limit0);
  FreeAndNil(limit1);
  FreeAndNil(box0);
  FreeAndNil(box1);
  FreeAndNil(box2);
  FreeAndNil(box3);
  FreeAndNil(box4);
  FreeAndNil(box5);
  inherited;
end;

end.
