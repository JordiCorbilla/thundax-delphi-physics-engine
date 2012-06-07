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
unit tdpe.lib.automation.scale;

interface

Uses tdpe.lib.particle.group, tdpe.lib.engine, tdpe.lib.render, tdpe.lib.particle.box.solid, Graphics;

type
  TScale = class(TGroup)
  private
    door1, door2: TSolidBox;
    scale1, scale3: TSolidBox;
    limit1, limit2: Integer;
    obrir1, cerrar1, obrir2, cerrar2: Boolean;
  public
    Constructor Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; px, py: double; color: TColor); Reintroduce;
    destructor Destroy(); override;
    procedure OpenDoor();
    procedure CloseDoor();
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ Car }

procedure TScale.CloseDoor;
begin
  if door1.px >= limit1 - door1.Width then
    cerrar1 := true;
  if cerrar1 then
  begin
    if door1.px <= limit1 then
      door1.px := door1.px + 0.5;
  end;
  if door2.px <= limit2 + door1.Width then
    cerrar2 := true;
  if cerrar2 then
  begin
    if door2.px >= limit2 then
      door2.px := door2.px - 0.5;
  end;
end;

constructor TScale.Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; px, py: double; color: TColor);
begin
  inherited Create(true);
  scale1 := TSolidBox.Create(px, 240, 10, 70, -0.8, true, 1, 0.3, 0.3, 0, color);
  scale1.SetRenderer(aRenderer);
  addParticle(scale1);

  door1 := TSolidBox.Create(px + 65, 261, 90, 10, 0, true, 1, 0.3, 0.3, 0, color);
  limit1 := Round(px) + 65;
  door1.SetRenderer(aRenderer);
  addParticle(door1);

  door2 := TSolidBox.Create(px + 155, 261, 90, 10, 0, true, 1, 0.3, 0.3, 0, color);
  limit2 := Round(px) + 155;
  door2.SetRenderer(aRenderer);
  addParticle(door2);

  scale3 := TSolidBox.Create(px + 220, 240, 10, 70, 0.8, true, 1, 0.3, 0.3, 0, color);
  scale3.SetRenderer(aRenderer);
  addParticle(scale3);
end;

destructor TScale.Destroy;
begin
  FreeAndNil(door1);
  FreeAndNil(door2);
  FreeAndNil(scale1);
  FreeAndNil(scale3);
  inherited;
end;

procedure TScale.OpenDoor;
begin
  if door1.px <= limit1 then
    obrir1 := true;
  if obrir1 then
  begin
    if door1.px > limit1 - door1.Width then
      door1.px := door1.px - 0.5;
  end;
  if door2.px >= limit2 then
    obrir2 := true;
  if obrir2 then
  begin
    if door2.px < limit2 + door1.Width then
      door2.px := door2.px + 0.5;
  end;
end;

end.
