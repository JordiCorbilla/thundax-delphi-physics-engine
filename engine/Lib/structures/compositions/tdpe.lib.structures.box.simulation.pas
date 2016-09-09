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
unit tdpe.lib.structures.box.simulation;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.box.solid, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, FMX.Graphics,
  tdpe.lib.particle.circle.solid, Generics.Collections, System.UITypes, System.Types, System.UIConsts;

type
  TBrick = class(TGroup)
  private
    Circles: TObjectList<TSolidCircle>;
    c1: TSolidCircle;
    c2: TSolidCircle;
    c3: TSolidCircle;
    c4: TSolidCircle;
    s1: TSpringRestriction;
    s2: TSpringRestriction;
    s3: TSpringRestriction;
    s4: TSpringRestriction;
    s5: TSpringRestriction;
    s6: TSpringRestriction;
  public
    procedure Activate(i: integer);
    procedure Deactivate;
    function isInside(x, y: integer; var pos: integer): boolean;
    procedure Move(x, y: integer);
    Constructor Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: Double; color: TColor); Reintroduce;
    Destructor Destroy(); override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TBrick }

procedure TBrick.Activate(i: integer);
begin
  Circles[i].StartDrag := true;
end;

procedure TBrick.Deactivate();
var
  circle: TSolidCircle;
begin
  for circle in Circles do
    circle.StartDrag := false;
end;

constructor TBrick.Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: Double; color: TColor);
var
  w, h: integer;
  radius, width: integer;
begin
  inherited Create(true);
  Circles := TObjectList<TSolidCircle>.Create;
  w := 40;
  h := 25;
  radius := 5;
  width := 5;

  c1 := TSolidCircle.Create(px, py, radius, false, 0.001, 0.1, 1, claRed);
  c1.SetRenderer(aRenderer);
  addParticle(c1);
  Circles.Add(c1);

  c2 := TSolidCircle.Create(px + w, py, radius, false, 0.001, 0.1, 1, claRed);
  c2.SetRenderer(aRenderer);
  addParticle(c2);
  Circles.Add(c2);

  c3 := TSolidCircle.Create(px + w, py + h, radius, false, 0.001, 0.1, 1, claRed);
  c3.SetRenderer(aRenderer);
  addParticle(c3);
  Circles.Add(c3);

  c4 := TSolidCircle.Create(px, py + h, radius, false, 0.001, 0.1, 1, claRed);
  c4.SetRenderer(aRenderer);
  addParticle(c4);
  Circles.Add(c4);

  s1 := TSpringRestriction.Create(c1, c2, 1, true, width, 1, false, clagray);
  s1.SetRenderer(aRenderer);
  AddRestriction(s1);

  s2 := TSpringRestriction.Create(c2, c3, 1, true, width, 1, false, clagray);
  s2.SetRenderer(aRenderer);
  AddRestriction(s2);

  s3 := TSpringRestriction.Create(c3, c4, 1, true, width, 1, false, clagray);
  s3.SetRenderer(aRenderer);
  AddRestriction(s3);
  s4 := TSpringRestriction.Create(c4, c1, 1, true, width, 1, false, clagray);
  s4.SetRenderer(aRenderer);
  AddRestriction(s4);

  s5 := TSpringRestriction.Create(c1, c3, 1, true, width, 1, false, clagray);
  s5.SetRenderer(aRenderer);
  AddRestriction(s5);

  s6 := TSpringRestriction.Create(c4, c2, 1, true, width, 1, false, clagray);
  s6.SetRenderer(aRenderer);
  AddRestriction(s6);
end;

destructor TBrick.Destroy;
begin
  FreeAndNil(s1);
  FreeAndNil(s2);
  FreeAndNil(s3);
  FreeAndNil(s4);
  FreeAndNil(s5);
  FreeAndNil(s6);
  FreeAndNil(Circles);
  inherited;
end;

function TBrick.isInside(x, y: integer; var pos: integer): boolean;
var
  i: integer;
  bFound: boolean;
begin
  i := 0;
  bFound := false;
  while (i < Circles.Count) and (not bFound) do
  begin
    bFound := Circles[i].isInside(x, y);
    pos := i;
    Inc(i);
  end;
  result := bFound;
end;

procedure TBrick.Move(x, y: integer);
var
  circle: TSolidCircle;
begin
  for circle in Circles do
  begin
    if circle.StartDrag then
      circle.Move(x, y);
  end;
end;

end.
