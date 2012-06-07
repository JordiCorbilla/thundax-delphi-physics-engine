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
unit tdpe.lib.automation.silo;

interface

Uses tdpe.lib.particle.group, tdpe.lib.engine, tdpe.lib.render,
  tdpe.lib.particle.box.solid, Graphics, SysUtils;

type
  TSilo = class(TGroup)
  private
    Fleftwall: TSolidBox;
    Frightwall: TSolidBox;
    Fleftwall1: TSolidBox;
    Frightwall1: TSolidBox;
    Fleftwall2: TSolidBox;
    Frightwall2: TSolidBox;
    Fbox1: TSolidBox;
    Fbox2: TSolidBox;
  public
    Constructor Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: double; color: TColor); Reintroduce;
    Destructor Destroy(); Override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection;

{ TSilo }

constructor TSilo.Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: double; color: TColor);
begin
  inherited Create(True);
  Fleftwall := TSolidBox.Create(px, py, 10, 70, 0, true, 1, 0.3, 0.3, 0, color);
  Fleftwall.SetRenderer(aRenderer);
  addParticle(Fleftwall);

  Frightwall := TSolidBox.Create(px + 53, py, 10, 70, 0, true, 1, 0.3, 0.3, 0, color);
  Frightwall.SetRenderer(aRenderer);
  addParticle(Frightwall);

  Fleftwall1 := TSolidBox.Create(px + 8, py + 43, 10, 30, -0.7, true, 1, 0.3, 0.3, 0, color);
  Fleftwall1.SetRenderer(aRenderer);
  addParticle(Fleftwall1);

  Frightwall1 := TSolidBox.Create(px + 45, py + 43, 10, 30, 0.7, true, 1, 0.3, 0.3, 0, color);
  Frightwall1.SetRenderer(aRenderer);
  addParticle(Frightwall1);

  Fleftwall2 := TSolidBox.Create(px + 17, py + 55, 10, 10, 0, true, 1, 0.3, 0.3, 0, color);
  Fleftwall2.SetRenderer(aRenderer);
  addParticle(Fleftwall2);

  Frightwall2 := TSolidBox.Create(px + 36, py + 55, 10, 10, 0, true, 1, 0.3, 0.3, 0, color);
  Frightwall2.SetRenderer(aRenderer);
  addParticle(Frightwall2);

  Fbox1 := TSolidBox.Create(px + 17, py + 95, 10, 80, 0, true, 1, 0.3, 0.3, 0, color);
  Fbox1.SetRenderer(aRenderer);
  addParticle(Fbox1);

  Fbox2 := TSolidBox.Create(px + 36, py + 95, 10, 80, 0, true, 1, 0.3, 0.3, 0, color);
  Fbox2.SetRenderer(aRenderer);
  addParticle(Fbox2);
end;

destructor TSilo.Destroy;
begin
  FreeAndNil(Fleftwall);
  FreeAndNil(Frightwall);
  FreeAndNil(Fleftwall1);
  FreeAndNil(Frightwall1);
  FreeAndNil(Fleftwall2);
  FreeAndNil(Frightwall2);
  FreeAndNil(Fbox1);
  FreeAndNil(Fbox2);
  inherited;
end;

end.
