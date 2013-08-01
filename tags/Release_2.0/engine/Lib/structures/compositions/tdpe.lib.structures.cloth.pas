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
unit tdpe.lib.structures.cloth;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle.solid, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, Graphics, Contnrs, Generics.Collections;

Type
  TCloth = class(TGroup)
  Private
    FCircles: TObjectList<TSolidCircle>;
    FSprings: TObjectList<TSpringRestriction>;
  Public
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine; x, y: Double); reintroduce; Virtual;
    function isInside(x, y: Integer; var pos: Integer): boolean;
    procedure Activate(i: Integer);
    procedure Deactivate();
    procedure Move(x, y: Integer);
    Destructor Destroy(); override;
  End;

implementation

uses
  SysUtils;

{ TCloth }

procedure TCloth.Activate(i: Integer);
begin
  FCircles[i].StartDrag := true;
end;

constructor TCloth.Create(render: TAbstractRenderer; aEngine: TEngine; x, y: Double);
var
  bx: Double;
  By: Double;
  bsize, ySize: Double;
  yslope: Double;
  ParticleSize: Double;
  nParticles: Integer;
  innerCircle: TSolidCircle;
  restriction: TSpringRestriction;
  i: Integer;
  fixed: boolean;
  clothDepth: Integer;
  h, j, k: Integer;
begin
  inherited Create(False);
  FCircles := TObjectList<TSolidCircle>.Create;
  FSprings := TObjectList<TSpringRestriction>.Create;
  bx := x;
  By := y;
  bsize := 20;
  ySize := 25;
  yslope := 0;
  ParticleSize := 1;
  nParticles := 10;
  clothDepth := 10;

  // Create the particles
  for h := 0 to clothDepth - 1 do
  begin
    for i := 0 to nParticles - 1 do
    begin
      fixed := (h = 0) and ((i = 0) or (i = (nParticles - 1)));
      if fixed then
        ParticleSize := 3;
      innerCircle := TSolidCircle.Create(bx, By, ParticleSize, fixed, 1); // 0.2
      innerCircle.Style.SetStyle(clRed, clWhite, 1);
      addParticle(innerCircle);
      innerCircle.SetRenderer(render);
      FCircles.Add(innerCircle);
      bx := bx + bsize;
      By := By + yslope;
    end;
    bx := x;
    By := By + ySize;
  end;
  j := 0;
  k := 0;
  // Create the FSprings
  for h := 0 to clothDepth - 1 do
  begin
    for i := 0 to nParticles - 2 do
    begin
      restriction := TSpringRestriction.Create(FCircles.Items[j], FCircles.Items[j + 1], 0.3, true, 2, 0.8, true, clWhite, 1);
      if (h = 0) and ((j = 0) or (j = (nParticles - 1))) then
        restriction.FixedEndLimit := 0.25;
      addRestriction(restriction);
      restriction.SetRenderer(render);
      FSprings.Add(restriction);
      inc(j);
    end;

    for i := 0 to nParticles - 1 do
    begin
      if h < (clothDepth - 1) then
      begin
        restriction := TSpringRestriction.Create(FCircles.Items[k], FCircles.Items[nParticles + k], 0.3, true, 2, 0.8, true, clWhite, 1);
        addRestriction(restriction);
        restriction.SetRenderer(render);
        FSprings.Add(restriction);
        inc(k);
      end;
    end;
    inc(j);
  end;
end;

procedure TCloth.Deactivate;
var
  circle : TSolidCircle;
begin
  for circle in FCircles do
    circle.StartDrag := False;
end;

destructor TCloth.Destroy;
begin
  FreeAndNil(FCircles);
  FreeAndNil(FSprings);
  inherited;
end;

function TCloth.isInside(x, y: Integer; var pos: Integer): boolean;
var
  i: Integer;
  bFound: boolean;
begin
  i := 0;
  bFound := False;
  while (i < FCircles.count) and (not bFound) do
  begin
    bFound := FCircles[i].isInside(x, y);
    pos := i;
    inc(i);
  end;
  result := bFound;
end;

procedure TCloth.Move(x, y: Integer);
var
  circle : TSolidCircle;
begin
  for circle in FCircles do
  begin
    if circle.StartDrag then
      circle.Move(x, y);
  end;
end;

end.
