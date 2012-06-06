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
unit tdpe.lib.particle.spring;

interface

Uses tdpe.lib.particle.box, tdpe.lib.particle.abstractparticle, tdpe.lib.particle.spring.restriction,
  tdpe.lib.vector, tdpe.lib.math.helper, tdpe.lib.particle.circle, SysUtils, Graphics;

Type
  TSpringRestrictionParticle = class(TBoxParticle)
  Private
    Fparticle1: TAbstractParticle;
    Fparticle2: TAbstractParticle;
    FAverageVelocity: IVector;
    Flambda: IVector;
    Fparent: TSpringRestriction;
    FscaleToLength: Boolean;
    Fs: Double;
    FBoxCornerA: IVector;
    FBoxCornerB: IVector;
    FrectScale: Double;
    FrectHeight: Double;
    FfixedEndLimit: Double;
  Public
    function GetMass: Double; Override;
    function GetElasticity: Double; override;
    function GetFriction: Double; Override;
    function GetVelocity: IVector; Override;
    function GetInvMass: Double; Override;
    Constructor Create(particle1, particle2: TAbstractParticle; SpringRestriction: TSpringRestriction; RectHeight,
      RectScale: Double; FscaleToLength: Boolean; color: TColor); Reintroduce;
    Procedure Init; Override;
    Procedure Paint; Override;
    Procedure UpdatePosition;
    Procedure ResolveCollision(mtd, vel, n: IVector; d: Double; o: Integer; p: TAbstractParticle); Override;
    Function ClosestParamPoint(closest: IVector): Double;
    Function GetContactParamPoint(p: TAbstractParticle): Double;
    Procedure SetCorners(boxParticle: TBoxParticle; i: Integer);
    Function ClosestPtSegmentSegment: Double;
    Property RectScale: Double read FrectScale Write FrectScale;
    Property RectHeight: Double Read FrectHeight Write FrectHeight;
    Property FixedEndLimit: Double Read FfixedEndLimit Write FfixedEndLimit;
    property Mass: Double Read GetMass;
    property InvMass: Double Read GetInvMass;
    Property Friction: Double Read GetFriction;
    property Elasticity: Double Read GetElasticity;
    Property Velocity: IVector Read GetVelocity;
  End;

implementation

uses
  tdpe.lib.math;

{ TSpringRestrictionParticle }

function TSpringRestrictionParticle.ClosestParamPoint(closest: IVector): Double;
var
  distance: IVector;
  position: Double;
begin
  distance := Fparticle2.current.Substract(Fparticle1.current);
  position := distance.InnerProduct(closest.Substract(Fparticle1.current)) / (distance.InnerProduct(distance));
  Result := TMathUtil.Clamp(position, 0, 1);
end;

function TSpringRestrictionParticle.ClosestPtSegmentSegment: Double;
var
  positionParticle1, positionParticle2, pp2, pq2: IVector;
  d1, d2, r: IVector;
  position, a, e, f: Double;
  closest, b, denom: Double;
  c1, c2, c1mc2: IVector;
begin
  positionParticle1 := Fparticle1.current;
  positionParticle2 := Fparticle2.current;
  pp2 := FBoxCornerA;
  pq2 := FBoxCornerB;

  d1 := positionParticle2.Substract(positionParticle1);
  d2 := pq2.Substract(pp2);
  r := positionParticle1.Substract(pp2);

  a := d1.InnerProduct(d1);
  e := d2.InnerProduct(d2);
  f := d2.InnerProduct(r);

  closest := d1.InnerProduct(r);
  b := d1.InnerProduct(d2);
  denom := a * e - b * b;

  if TMathHelper.Compare(denom, 0.0, '<>') then
    Fs := TMathUtil.Clamp((b * f - closest * e) / denom, 0, 1)
  else
    Fs := 0.5;
  position := (b * Fs + f) / e;
  if TMathHelper.Compare(position, 0, '<') then
  begin
    position := 0;
    Fs := TMathUtil.Clamp(-closest / a, 0, 1);
  end
  else if TMathHelper.Compare(position, 0, '>') then
  begin
    position := 1;
    Fs := TMathUtil.Clamp((b - closest) / a, 0, 1)
  end;

  c1 := positionParticle1.Add(d1.ScalarProduct(Fs));
  c2 := pp2.Add(d2.ScalarProduct(position));
  c1mc2 := c1.Substract(c2);
  Result := c1mc2.InnerProduct(c1mc2);
end;

constructor TSpringRestrictionParticle.Create(particle1, particle2: TAbstractParticle; SpringRestriction: TSpringRestriction;
  RectHeight, RectScale: Double; FscaleToLength: Boolean; color: TColor);
begin
  inherited Create(0, 0, 0, 0, 0, False, 0.1, 0, 0);
  Self.Fparticle1 := particle1;
  Self.Fparticle2 := particle2;
  Flambda := TVector.New;
  FAverageVelocity := TVector.New;
  ParticleColor := color;
  Fparent := SpringRestriction;
  Self.RectScale := RectScale;
  Self.RectHeight := RectHeight;
  Self.FscaleToLength := FscaleToLength;
  FixedEndLimit := 0;
end;

function TSpringRestrictionParticle.GetContactParamPoint(p: TAbstractParticle): Double;
var
  position, d: Double;
  ShortestIndex: Integer;
  ShortestDistance: Double;
  ParamList: array [0 .. 3] of Double;
  i: Integer;
begin
  if (p is TCircleParticle) then
    position := ClosestParamPoint(p.current)
  else if (p is TBoxParticle) then
  begin
    ShortestDistance := High(Integer);
    for i := 0 to 3 do
    begin
      ShortestIndex := 0;
      SetCorners(TBoxParticle(p), i);
      d := ClosestPtSegmentSegment;
      if TMathHelper.Compare(d, ShortestDistance, '<') then
      begin
        ShortestDistance := d;
        ShortestIndex := i;
        ParamList[i] := Fs;
      end;
    end;
    position := ParamList[ShortestIndex];
  end
  else
  begin
    raise Exception.Create('TSpringRestrictionParticle.GetContactParamPoint : Unknown Particle Type');
  end;
  Result := position;
end;

function TSpringRestrictionParticle.GetElasticity: Double;
begin
  Result := (Fparticle1.Elasticity + Fparticle2.Elasticity) / 2;
end;

function TSpringRestrictionParticle.GetFriction: Double;
begin
  Result := (Fparticle1.Friction + Fparticle2.Friction) / 2;
end;

function TSpringRestrictionParticle.GetInvMass: Double;
begin
  if (Fparticle1.Fixed And Fparticle2.Fixed) then
    Result := 0
  else
    Result := 1 / ((Fparticle1.Mass + Fparticle2.Mass) / 2)
end;

function TSpringRestrictionParticle.GetMass: Double;
begin
  Result := (Fparticle1.Mass + Fparticle2.Mass) / 2;
end;

function TSpringRestrictionParticle.GetVelocity: IVector;
var
  p1v, p2v: IVector;
begin
  p1v := Fparticle1.Velocity;
  p2v := Fparticle2.Velocity;
  FAverageVelocity.SetPoint(((p1v.x + p2v.x) / 2), ((p1v.y + p2v.y) / 2));
  Result := FAverageVelocity;
end;

procedure TSpringRestrictionParticle.Init;
begin
  inherited;

end;

procedure TSpringRestrictionParticle.Paint;
var
  width, height: Double;
  closest: IVector;
begin
  closest := Fparent.center;
  width := Fparent.CurrLength * RectScale;
  height := RectHeight;
  Fparent.Renderer.box(closest.x, closest.y, width, height, Fparent.angle, ParticleColor);
end;

procedure TSpringRestrictionParticle.ResolveCollision(mtd, vel, n: IVector; d: Double; o: Integer; p: TAbstractParticle);
var
  position, c1, c2: Double;
  denom: Double;
  corrParticle: TAbstractParticle;
begin
  inherited;
  position := GetContactParamPoint(p);
  c1 := 1 - position;
  c2 := position;
  if (Fparticle1.Fixed) Then
  begin
    if TMathHelper.Compare(c2, FixedEndLimit, '<=') then
      Exit;
    Flambda.SetPoint(mtd.x / c2, mtd.y / c2);
    Fparticle2.current.Add(Flambda, true);
    Fparticle2.Velocity := vel;
  End
  else if (Fparticle2.Fixed) then
  begin
    if TMathHelper.Compare(c1, FixedEndLimit, '<=') then
      Exit;
    Flambda.SetPoint(mtd.x / c1, mtd.y / c1);
    Fparticle1.current.Add(Flambda, true);
    Fparticle1.Velocity := vel;
  end
  else
  begin
    denom := c1 * c1 + c2 * c2;
    if (TMathHelper.Compare(denom, 0, '=')) Then
      Exit;
    Flambda.SetPoint(mtd.x / denom, mtd.y / denom);
    Fparticle1.current.Add(Flambda.ScalarProduct(c1), true);
    Fparticle2.current.Add(Flambda.ScalarProduct(c2), true);
    if TMathHelper.Compare(position, 0.5, '=') then
    begin
      Fparticle1.Velocity := vel;
      Fparticle2.Velocity := vel;
    end
    else
    begin
      If TMathHelper.Compare(position, 0.5, '<') then
        corrParticle := Fparticle1
      else
        corrParticle := Fparticle2;
      corrParticle.Velocity := vel;
    end;
  end;
end;

procedure TSpringRestrictionParticle.SetCorners(boxParticle: TBoxParticle; i: Integer);
var
  boxPosx, boxPosy : Double;
  axesExtens0x, axesExtens0y : Double;
  axesExtens1x, axesExtens1y : Double;
  extensDiff0x, extensDiff0y : Double;
  extensDiff1x, extensDiff1y: Double;
  axes: TRctTypeVector;
  Extents: TRctTypeDouble;
begin
  boxPosx := boxParticle.current.x;
  boxPosy := boxParticle.current.y;

  axes := boxParticle.axes;
  Extents := boxParticle.Extents;

  axesExtens0x := axes[0].x * Extents[0];
  axesExtens0y := axes[0].y * Extents[0];
  axesExtens1x := axes[1].x * Extents[1];
  axesExtens1y := axes[1].y * Extents[1];

  extensDiff0x := axesExtens0x - axesExtens1x;
  extensDiff0y := axesExtens0y - axesExtens1y;
  extensDiff1x := axesExtens0x + axesExtens1x;
  extensDiff1y := axesExtens0y + axesExtens1y;

  case i of
    0:
      begin
        FBoxCornerA.x := boxPosx - extensDiff1x;
        FBoxCornerA.y := boxPosy - extensDiff1y;
        FBoxCornerB.x := boxPosx + extensDiff0x;
        FBoxCornerB.y := boxPosy + extensDiff0y;
      end;
    1:
      begin
        FBoxCornerA.x := boxPosx + extensDiff0x;
        FBoxCornerA.y := boxPosy + extensDiff0y;
        FBoxCornerB.x := boxPosx + extensDiff1x;
        FBoxCornerB.y := boxPosy + extensDiff1y;
      end;
    2:
      begin
        FBoxCornerA.x := boxPosx + extensDiff1x;
        FBoxCornerA.y := boxPosy + extensDiff1y;
        FBoxCornerB.x := boxPosx - extensDiff0x;
        FBoxCornerB.y := boxPosy - extensDiff0y;
      end;
    3:
      begin
        FBoxCornerA.x := boxPosx - extensDiff0x;
        FBoxCornerA.y := boxPosy - extensDiff0y;
        FBoxCornerB.x := boxPosx - extensDiff1x;
        FBoxCornerB.y := boxPosy - extensDiff1y;
      end;
  end;
end;

procedure TSpringRestrictionParticle.UpdatePosition;
var
  closest: IVector;
begin
  closest := Fparent.center;
  current.SetPoint(closest.x, closest.y);
  if FscaleToLength then
    Width := Fparent.CurrLength * RectScale
  else
    Width := Fparent.RestLength * RectScale;
  Height := RectHeight;
  Radian := Fparent.Radian;
end;

end.
