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
unit tdpe.lib.collision.detector;

interface

Uses
  tdpe.lib.particle.abstractparticle, tdpe.lib.vector, tdpe.lib.particle.box,
  tdpe.lib.particle.circle, tdpe.lib.math.interval,
  tdpe.lib.collision.resolver, tdpe.lib.particle.box.solid,
  tdpe.lib.collision.detector.contract,
  tdpe.lib.structures, tdpe.lib.particle.circle.solid;

Type
  TCollisionDetector = Class(TInterfacedObject, ICollisionDetector)
  private
    hitpointArray: TArrayVector;
    collisionNormal: IVector;
    collisionDepth: Double;
    hitPoint: IVector;
    procedure getHitPoint(var hitPoint: IVector);
    Procedure NormalAgainstNormal(ParticleA, ParticleB: TAbstractParticle);
    Procedure SampleAgainstNormal(ParticleA, ParticleB: TAbstractParticle);
    Procedure SampleAgainstSample(ParticleA, ParticleB: TAbstractParticle);
    Function TestTypes(ParticleA, ParticleB: TAbstractParticle): Boolean;
    Function TestOBBvsOBB(boxA, boxB: TBoxParticle): Boolean;
    function TestOBBvsOBBSolid(boxA, boxB: TSolidBox): Boolean;
    Function TestCirclevsCircle(circleA, circleB: TCircleParticle): Boolean;
    Function TestCirclevsCircleSolid(circleA, circleB: TSolidCircle): Boolean;
    Function TestOBBvsCircle(boxA: TBoxParticle; circleA: TCircleParticle): Boolean;
    Function TestOBBvsCircleSolid(boxA: TSolidBox; circleA: TSolidCircle): Boolean;
    function testSolidTypes(boxA, boxB: TAbstractParticle): Boolean;
    Function TestIntervals(IntervalA, IntervalB: TInterval): Double;
    Function ClosestVertexOnOBB(vec: IVector; box: TBoxParticle): IVector;
    Function ClosestVertexOnOBB2(vec: IVector; box: TSolidBox): IVector;
  Public
    Procedure Check(ParticleA, ParticleB: TAbstractParticle);
    constructor Create();
  end;

var
  CollisionDetectorInstance: ICollisionDetector;

implementation

uses
  SysUtils, tdpe.lib.particle.item.solid, tdpe.lib.collision.resolver.solid, tdpe.lib.math,
  Graphics, tdpe.lib.logging;

{ TCollisionDetector }

constructor TCollisionDetector.Create();
begin
  hitPoint := TVector.New;
  collisionNormal := TVector.New;
end;

function TCollisionDetector.ClosestVertexOnOBB(vec: IVector; box: TBoxParticle): IVector;
var
  d, q: IVector;
  i: integer;
  dist: Double;
begin
  d := vec.Substract(box.Sample);
  q := TVector.New.SetPoint(box.Sample.x, box.Sample.y);
  for i := 0 to 1 do
  begin
    dist := d.InnerProduct(box.axes[i]);
    if TMathHelper.Compare(dist, 0, '>=') then
      dist := box.extents[i]
    else
      dist := -box.extents[i];

    q.Add(box.axes[i].ScalarProduct(dist), true);
  end;
  result := q;
end;

procedure TCollisionDetector.NormalAgainstNormal(ParticleA, ParticleB: TAbstractParticle);
begin
  ParticleA.Sample.Clone(ParticleA.Current);
  ParticleB.Sample.Clone(ParticleB.Current);
  TestTypes(ParticleA, ParticleB);
end;

procedure TCollisionDetector.SampleAgainstNormal(ParticleA, ParticleB: TAbstractParticle);
var
  s, t: Double;
  i: integer;
begin
  s := 1 / (ParticleA.MultiSample + 1);
  t := s;
  ParticleB.Sample.Clone(ParticleB.Current);

  for i := 0 to ParticleA.MultiSample - 1 do
  begin
    ParticleA.Sample.SetPoint(ParticleA.Previous.x + t * (ParticleA.Current.x - ParticleA.Previous.x),
      ParticleA.Previous.y + t * (ParticleA.Current.y - ParticleA.Previous.y));
    if TestTypes(ParticleA, ParticleB) then
      Exit;
    t := t + s;
  end;
end;

procedure TCollisionDetector.SampleAgainstSample(ParticleA, ParticleB: TAbstractParticle);
var
  s, t: Double;
  i: integer;
begin
  s := 1 / (ParticleA.MultiSample + 1);
  t := s;
  ParticleB.Sample.Clone(ParticleB.Current);

  for i := 0 to ParticleA.MultiSample - 1 do
  begin
    ParticleA.Sample.SetPoint(ParticleA.Previous.x + t * (ParticleA.Current.x - ParticleA.Previous.x),
      ParticleA.Previous.y + t * (ParticleA.Current.y - ParticleA.Previous.y));
    ParticleB.Sample.SetPoint(ParticleB.Previous.x + t * (ParticleB.Current.x - ParticleB.Previous.x),
      ParticleB.Previous.y + t * (ParticleB.Current.y - ParticleB.Previous.y));
    if TestTypes(ParticleA, ParticleB) then
      Exit;
    t := t + s;
  end;
end;

procedure TCollisionDetector.Check(ParticleA, ParticleB: TAbstractParticle);
begin
  if ParticleA.Fixed And ParticleB.Fixed then
    Exit;
  if (ParticleA.MultiSample = 0) And (ParticleB.MultiSample = 0) then
    NormalAgainstNormal(ParticleA, ParticleB)
  else if (ParticleA.MultiSample > 0) And (ParticleB.MultiSample = 0) then
    SampleAgainstNormal(ParticleA, ParticleB)
  else if (ParticleB.MultiSample > 0) And (ParticleA.MultiSample = 0) then
    SampleAgainstNormal(ParticleB, ParticleA)
  else if ParticleA.MultiSample = ParticleB.MultiSample then
    SampleAgainstSample(ParticleA, ParticleB)
  else
    NormalAgainstNormal(ParticleA, ParticleB);
end;

function TCollisionDetector.TestCirclevsCircle(circleA, circleB: TCircleParticle): Boolean;
var
  DepthX, DepthY: Double;
  CollisionNormal: IVector;
  CollisionDepth: Double;
  Mag: Double;
begin
  result := False;
  DepthX := TestIntervals(circleA.GetIntervalX, circleB.GetIntervalX);
  if DepthX = 0 then
    Exit;

  DepthY := TestIntervals(circleA.GetIntervalY, circleB.GetIntervalY);
  if DepthY = 0 then
    Exit;

  CollisionNormal := circleA.Sample.Substract(circleB.Sample);
  Mag := CollisionNormal.Magnitude;
  CollisionDepth := (circleA.Radius + circleB.Radius) - Mag;

  if TMathHelper.Compare(CollisionDepth, 0, '>') then
  begin
    CollisionNormal.Divide(Mag);
    CollisionResolverInstance.Resolve(circleA, circleB, nil, CollisionNormal, CollisionDepth);
    result := true;
  end;
end;

function TCollisionDetector.TestCirclevsCircleSolid(circleA, circleB: TSolidCircle): Boolean;
var
  DepthX, DepthY: Double;
  Mag: Double;
begin
  result := False;

  DepthX := TestIntervals(circleA.GetIntervalX, circleB.GetIntervalX);
  if TMathHelper.Compare(DepthX, 0, '=') then
    Exit;

  DepthY := TestIntervals(circleA.GetIntervalY, circleB.GetIntervalY);
  if TMathHelper.Compare(DepthY, 0, '=') then
    Exit;

  collisionNormal := circleA.Sample.Substract(circleB.Sample);
  Mag := collisionNormal.Magnitude;
  collisionDepth := (circleA.Radius + circleB.Radius) - Mag;

  if TMathHelper.Compare(collisionDepth, 0, '>') then
  begin
    collisionNormal.Divide(Mag);
    result := true;
  end;
end;

function TCollisionDetector.TestIntervals(IntervalA, IntervalB: TInterval): Double;
var
  lena, lenB: Double;
begin
  if TMathHelper.Compare(IntervalA.Max, IntervalB.Min, '<') then
    result := 0
  else if TMathHelper.Compare(IntervalB.Max, IntervalA.Min, '<') then
    result := 0
  else
  begin
    lena := IntervalB.Max - IntervalA.Min;
    lenB := IntervalB.Min - IntervalA.Max;
    result := lenB;
    if abs(lena) < abs(lenB) then
      result := lena;
  end;
end;

function TCollisionDetector.TestOBBvsCircle(boxA: TBoxParticle; circleA: TCircleParticle): Boolean;
var
  CollisionNormal: IVector;
  CollisionDepth: Double;
  Depths: Array [0 .. 1] of Double;
  i: integer;
  BoxAxis: IVector;
  Depth: Double;
  box: Double;
  Vertex: IVector;
  Mag: Double;
begin
  CollisionDepth := High(integer);
  CollisionNormal := boxA.axes[0];
  for i := 0 to 1 do
  begin
    BoxAxis := boxA.axes[i];
    Depth := TestIntervals(boxA.GetProjection(BoxAxis), circleA.GetProjection(BoxAxis));
    If Depth = 0 then
    begin
      result := False;
      Exit;
    end;
    if abs(Depth) < abs(CollisionDepth) then
    begin
      CollisionNormal := BoxAxis;
      CollisionDepth := Depth;
    end;
    Depths[i] := Depth;
  end;
  box := circleA.Radius;
  if (abs(Depths[0]) < box) And (abs(Depths[1]) < box) Then
  begin
    Vertex := ClosestVertexOnOBB(circleA.Sample, boxA);
    CollisionNormal := Vertex.Substract(circleA.Sample);
    Mag := CollisionNormal.Magnitude;
    CollisionDepth := box - Mag;
    If CollisionDepth > 0 then
    begin
      CollisionNormal.Divide(Mag);
    end
    else
    begin
      result := False;
      Exit;
    end;
  end;

  CollisionResolverInstance.Resolve(boxA, circleA, nil, CollisionNormal, CollisionDepth);
  result := true;
end;

function TCollisionDetector.TestOBBvsCircleSolid(boxA: TSolidBox; circleA: TSolidCircle): Boolean;
var
  Depths: Array [0 .. 1] of Double;
  i: integer;
  BoxAxis: IVector;
  Depth: Double;
  box: Double;
  Vertex: IVector;
  Mag: Double;
begin
  collisionDepth := High(integer);
  collisionNormal := boxA.axes[0];
  for i := 0 to 1 do
  begin
    BoxAxis := boxA.axes[i];
    Depth := TestIntervals(boxA.GetProjection(BoxAxis), circleA.GetProjection(BoxAxis));
    If TMathHelper.Compare(Depth, 0, '=') then
    begin
      result := False;
      Exit;
    end;
    if abs(Depth) < abs(collisionDepth) then
    begin
      collisionNormal := BoxAxis;
      collisionDepth := Depth;
    end;
    Depths[i] := Depth;
  end;
  box := circleA.Radius;
  if (abs(Depths[0]) < box) And (abs(Depths[1]) < box) Then
  begin
    Vertex := ClosestVertexOnOBB2(circleA.Sample, boxA);
    collisionNormal := Vertex.Substract(circleA.Sample);
    Mag := collisionNormal.Magnitude;
    collisionDepth := box - Mag;
    If TMathHelper.Compare(collisionDepth, 0, '>') then
      collisionNormal.Divide(Mag)
    else
    begin
      result := False;
      Exit;
    end;
  end;
  result := true;
end;

function TCollisionDetector.TestOBBvsOBB(boxA, boxB: TBoxParticle): Boolean;
var
  CollisionNormal: IVector;
  CollisionDepth: Double;
  i: integer;
  axisA, Axisb: IVector;
  absA, AbsB: Double;
  DepthA, DepthB: Double;
  altb: Boolean;
begin
  CollisionDepth := High(integer);
  CollisionNormal := boxA.axes[0];
  For i := 0 to 1 do
  begin
    axisA := boxA.axes[i];
    DepthA := TestIntervals(boxA.GetProjection(axisA), boxB.GetProjection(axisA));
    if DepthA = 0 then
    begin
      result := False;
      Exit;
    end;
    Axisb := boxB.axes[i];
    DepthB := TestIntervals(boxA.GetProjection(Axisb), boxB.GetProjection(Axisb));
    if TMathHelper.Compare(DepthB, 0, '=') then
    begin
      result := False;
      Exit;
    end;
    absA := abs(DepthA);
    AbsB := abs(DepthB);
    if (absA < abs(CollisionDepth)) or (AbsB < abs(CollisionDepth)) then
    begin
      altb := absA < AbsB;
      CollisionNormal := Axisb;
      CollisionDepth := DepthB;
      if altb then
      begin
        CollisionNormal := axisA;
        CollisionDepth := DepthA;
      end;
    end;
  end;
  CollisionResolverInstance.Resolve(boxA, boxB, nil, CollisionNormal, CollisionDepth);
  result := true;
end;

function TCollisionDetector.TestOBBvsOBBSolid(boxA, boxB: TSolidBox): Boolean;
var
  i: integer;
  axisA, Axisb: IVector;
  absA, AbsB: Double;
  DepthA, DepthB: Double;
  altb: Boolean;
begin
  collisionDepth := High(integer);
  collisionNormal := boxA.axes[0];
  For i := 0 to 1 do
  begin
    axisA := boxA.axes[i];
    DepthA := TestIntervals(boxA.GetProjection(axisA), boxB.GetProjection(axisA));
    if TMathHelper.Compare(DepthA, 0, '=') then
    begin
      result := False;
      Exit;
    end;
    Axisb := boxB.axes[i];
    DepthB := TestIntervals(boxA.GetProjection(Axisb), boxB.GetProjection(Axisb));
    if TMathHelper.Compare(DepthB, 0, '=') then
    begin
      result := False;
      Exit;
    end;
    absA := abs(DepthA);
    AbsB := abs(DepthB);
    if (absA < abs(collisionDepth)) or (AbsB < abs(collisionDepth)) then
    begin
      altb := TMathHelper.Compare(absA, AbsB, '<');
      collisionNormal := Axisb;
      collisionDepth := DepthB;
      if altb then
      begin
        collisionNormal := axisA;
        collisionDepth := DepthA;
      end;
    end;
  end;
  result := true;
end;

function TCollisionDetector.TestTypes(ParticleA, ParticleB: TAbstractParticle): Boolean;
begin
  result := False;
  if (ParticleA is TSolidItem) and (ParticleB is TSolidItem) then
  begin
    testSolidTypes(ParticleA, ParticleB);
    result := False;
  end
  else
  begin
    if (ParticleA is TBoxParticle) And (ParticleB is TBoxParticle) then
      result := TestOBBvsOBB(TBoxParticle(ParticleA), TBoxParticle(ParticleB))
    else if (ParticleA is TCircleParticle) And (ParticleB is TCircleParticle) then
      result := TestCirclevsCircle(TCircleParticle(ParticleA), TCircleParticle(ParticleB))
    else if (ParticleA is TBoxParticle) And (ParticleB is TCircleParticle) then
      result := TestOBBvsCircle(TBoxParticle(ParticleA), TCircleParticle(ParticleB))
    else if (ParticleA is TCircleParticle) And (ParticleB is TBoxParticle) then
      result := TestOBBvsCircle(TBoxParticle(ParticleB), TCircleParticle(ParticleA));
  end;
end;

function TCollisionDetector.ClosestVertexOnOBB2(vec: IVector; box: TSolidBox): IVector;
var
  d, q: IVector;
  i: integer;
  dist: Double;
begin
  d := vec.Substract(box.Sample);
  q := TVector.New.SetPoint(box.Sample.x, box.Sample.y);
  for i := 0 to 1 do
  begin
    dist := d.InnerProduct(box.axes[i]);
    if TMathHelper.Compare(dist, 0, '>=') then
      dist := box.extents[i]
    else
      dist := -box.extents[i];
    q.Add(box.axes[i].ScalarProduct(dist), true);
  end;
  result := q;
end;

procedure TCollisionDetector.getHitPoint(var hitPoint: IVector);
var
  i: integer;
begin
  hitPoint.SetPoint(0, 0);
  for i := 0 to Length(hitpointArray) - 1 do
    hitPoint.Add(hitpointArray[i], true);
  if (Length(hitpointArray) > 1) then
    hitPoint.ScalarProduct(1 / Length(hitpointArray), true);
end;

function TCollisionDetector.testSolidTypes(boxA, boxB: TAbstractParticle): Boolean;
  function captures(box: TSolidItem; vertices: TRctTypeVector2): Boolean; overload
  var
    re: Boolean;
    i : integer;
  begin
    re := False;
    for i := 0 to Length(vertices) - 1 do
    begin
      if (box.captures(vertices[i])) then
      begin
        TArrayHelper.SetArrayValue(hitpointArray, vertices[i]);
        re := true;
      end;
    end;
    result := re;
  end;
  function captures(box: TSolidItem; vertices: TArrayVector): Boolean; overload
  var
    re: Boolean;
    i : integer;
  begin
    re := False;
    for i := 0 to Length(vertices) - 1 do
    begin
      if (box.captures(vertices[i])) then
      begin
        TArrayHelper.SetArrayValue(hitpointArray, vertices[i]);
        re := true;
      end;
    end;
    result := re;
  end;
  function findHitPointRR(a: TSolidBox; b: TSolidBox): Boolean;
  var
    box: Boolean;
  begin
    box := False;
    if (captures(a, b.getVertices())) then
      box := true;
    if (captures(b, a.getVertices())) then
      box := true;
    result := box;
  end;
  function ConvertTRCTypeToArrayVector(a: TRctTypeVector2): TArrayVector;
  var
    s: TArrayVector;
    i: integer;
  begin
    for i := 0 to Length(a) - 1 do
      TArrayHelper.SetArrayValue(s, a[i]);
    result := s;
  end;
  function findHitPointRC(a: TSolidBox; b: TSolidCircle): Boolean;
  var
    box: Boolean;
  begin
    box := False;
    if (captures(b, a.getVertices())) then
      box := true;
    if (captures(a, b.getVertices(ConvertTRCTypeToArrayVector(a.normals)))) then
      box := true;
    result := box;
  end;
  function findHitPointCC(a: TSolidCircle; b: TSolidCircle): Boolean;
  var
    box: Boolean;
    d: IVector;
    f: IVector;
  begin
    box := False;
    d := TVector.New.SetPoint(0, 0);
    f := TVector.New.SetPoint(0, 0);
    d.Clone(b.Sample.Substract(a.Sample));
    if TMathHelper.Compare(d.Magnitude, (a.Range + b.Range), '<=') then
    begin
      f.Clone(d.Normalise.ScalarProduct(a.Range, true));
      TArrayHelper.SetArrayValue(hitpointArray, f.Add(a.Sample, true));
      box := true;
    end;
    result := box;
  end;
var
  res, res2: Boolean;
begin
  res := False;
  res2 := False;
  SetLength(hitpointArray, 0);
  if (boxA is TSolidBox) and (boxB is TSolidBox) then
  begin
    res := TestOBBvsOBBSolid(boxA as TSolidBox, boxB as TSolidBox);
    if (res) then
      res2 := findHitPointRR(boxA as TSolidBox, boxB as TSolidBox);
  end
  else if (boxA is TSolidCircle) and (boxB is TSolidCircle) then
  begin
    res := TestCirclevsCircleSolid(boxA as TSolidCircle, boxB as TSolidCircle);
    if (res) then
      res2 := findHitPointCC(boxA as TSolidCircle, boxB as TSolidCircle);
  end
  else if (boxA is TSolidBox) and (boxB is TSolidCircle) then
  begin
    res := TestOBBvsCircleSolid(boxA as TSolidBox, boxB as TSolidCircle);
    if (res) then
      res2 := findHitPointRC(boxA as TSolidBox, boxB as TSolidCircle);
  end
  else if (boxA is TSolidCircle) and (boxB is TSolidBox) then
  begin
    res := TestOBBvsCircleSolid(boxB as TSolidBox, boxA as TSolidCircle);
    if (res) then
      res2 := findHitPointRC(boxB as TSolidBox, boxA as TSolidCircle);
    if (res2) then
    begin
      getHitPoint(hitPoint);
      SolidCollisionResolverInstance.Resolve(boxB as TSolidItem, boxA as TSolidItem, hitPoint, collisionNormal, collisionDepth);
    end;
  end;
  if (res2) then
  begin
    getHitPoint(hitPoint);
    SolidCollisionResolverInstance.Resolve(boxA as TSolidItem, boxB as TSolidItem, hitPoint, collisionNormal, collisionDepth);
    result := False;
  end
  else
    result := res;
end;

Initialization

CollisionDetectorInstance := TCollisionDetector.Create;

Finalization

CollisionDetectorInstance := nil;

end.
