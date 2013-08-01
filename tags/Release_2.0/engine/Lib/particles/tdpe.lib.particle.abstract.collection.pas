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
unit tdpe.lib.particle.abstract.collection;

interface

Uses
  tdpe.lib.vector, tdpe.lib.particle.abstractparticle, tdpe.lib.particle.abstract.restriction,
  tdpe.lib.particle.spring.restriction,
  tdpe.lib.particle.spring, tdpe.lib.collision.detector, Classes,
  tdpe.lib.force.contract, generics.Collections, tdpe.lib.force.list;

Type

  TAbstractCollection = Class
  Private
    FIsPArented: Boolean;
  Public
    Particles: TList<TAbstractParticle>;
    Restrictions: TList<TAbstractRestriction>;
    constructor Create; Virtual;
    Destructor Destroy; Override;
    Function GetIsParented: Boolean; Overload;
    Procedure SetIsParented(value: Boolean); Overload;
    Procedure Init; Virtual;
    Procedure AddParticle(particle: TAbstractParticle);
    Procedure AddRestriction(restriction: TAbstractRestriction);
    Procedure RemoveParticle(particle: TAbstractParticle);
    Procedure RemoveRestriction(restriction: TAbstractRestriction);
    Procedure Paint; Virtual;
    Procedure Integrate(deltaTime: Double; MassLessForce: TForceList; Damping: Double); Virtual;
    Procedure SatisfyRestrictions; Virtual;
    Procedure CheckCollisionAgainstCollection(AbstractCollection: TAbstractCollection);
    Procedure CheckInternalCollision;
    Property IsPArented: Boolean read GetIsParented Write SetIsParented;
  end;

implementation

uses
  Sysutils;

{ TAbstractCollection }

procedure TAbstractCollection.AddRestriction(restriction: TAbstractRestriction);
begin
  Restrictions.Add(restriction);
  if IsPArented then
    restriction.Init;
end;

procedure TAbstractCollection.AddParticle(particle: TAbstractParticle);
begin
  Particles.Add(particle);
  if IsPArented then
    particle.Init;
end;

procedure TAbstractCollection.CheckCollisionAgainstCollection(AbstractCollection: TAbstractCollection);
var
  particleA, particleB: TAbstractParticle;
  restriction: TAbstractRestriction;
  restrictionA, restrictionB: TSpringRestriction;
begin
  for particleA in Particles do
  begin
    if not(particleA.Collidable) then
      continue;

    for particleB in AbstractCollection.Particles do
    begin
      if particleB.Collidable then
        CollisionDetectorInstance.Check(particleA, particleB);
    end;

    for restriction in AbstractCollection.Restrictions do
    begin
      restrictionB := TSpringRestriction(restriction);
      if ((restrictionB.Collidable) and not(restrictionB.isConnectedTo(particleA))) then
      begin
        TSpringRestrictionParticle(restrictionB.scp).UpdatePosition;
        CollisionDetectorInstance.Check(particleA, TSpringRestrictionParticle(restrictionB.scp));
      end;
    end;
  end;

  for restriction in Restrictions do
  begin
    restrictionA := TSpringRestriction(restriction);
    if not(restrictionA.Collidable) then
      continue;

    for particleB in AbstractCollection.Particles do
    begin
      if ((particleB.Collidable) And Not(restrictionA.isConnectedTo(particleB))) Then
      begin
        TSpringRestrictionParticle(restrictionA.scp).UpdatePosition;
        CollisionDetectorInstance.Check(particleB, TSpringRestrictionParticle(restrictionA.scp));
      end;
    end;
  end;
end;

procedure TAbstractCollection.CheckInternalCollision;
var
  i, j: Integer;
  particleA, particleB: TAbstractParticle;
  SpringRestriction: TSpringRestriction;
  restriction: TAbstractRestriction;
begin
  for j := 0 to Particles.Count - 1 do
  begin
    particleA := Particles[j];
    if not(particleA.Collidable) then
      continue;

    for i := j + 1 to Particles.Count - 1 do
    begin
      particleB := Particles[i];
      if particleB.Collidable then
        CollisionDetectorInstance.Check(particleA, particleB);
    end;

    for restriction in Restrictions do
    begin
      SpringRestriction := TSpringRestriction(restriction);
      if (SpringRestriction.Collidable and not(SpringRestriction.isConnectedTo(particleA))) then
      begin
        TSpringRestrictionParticle(SpringRestriction.scp).UpdatePosition;
        CollisionDetectorInstance.Check(particleA, TSpringRestrictionParticle(SpringRestriction.scp));
      end;
    end;
  end;
end;

constructor TAbstractCollection.Create;
begin
  FIsPArented := False;
  Particles := TList<TAbstractParticle>.Create;
  Restrictions := TList<TAbstractRestriction>.Create;
end;

destructor TAbstractCollection.Destroy;
begin
  FreeAndNil(Particles);
  FreeAndNil(Restrictions);
  inherited;
end;

procedure TAbstractCollection.Init;
var
  particle: TAbstractParticle;
  restriction: TAbstractRestriction;
begin
  for particle in Particles do
    particle.Init;

  for restriction in Restrictions do
    restriction.Init;
end;

procedure TAbstractCollection.Integrate(deltaTime: Double; MassLessForce: TForceList; Damping: Double);
var
  particle: TAbstractParticle;
begin
  for particle in Particles do
    particle.UpdateGeometricState(deltaTime, MassLessForce, Damping);
end;

procedure TAbstractCollection.SetIsParented(value: Boolean);
begin
  FIsPArented := value;
end;

function TAbstractCollection.GetIsParented: Boolean;
begin
  Result := FIsPArented;
end;

procedure TAbstractCollection.RemoveParticle(particle: TAbstractParticle);
begin
  Particles.Remove(particle);
end;

procedure TAbstractCollection.RemoveRestriction(restriction: TAbstractRestriction);
begin
  Restrictions.Remove(restriction);
end;

procedure TAbstractCollection.SatisfyRestrictions;
var
  restriction: TAbstractRestriction;
begin
  for restriction in Restrictions do
    restriction.Resolve;
end;

procedure TAbstractCollection.Paint;
var
  particle: TAbstractParticle;
  SpringRestriction: TSpringRestriction;
  restriction: TAbstractRestriction;
begin
  for particle in Particles do
  begin
    if not(particle.Fixed) then
      particle.Paint;
  end;

  for restriction in Restrictions do
  begin
    SpringRestriction := TSpringRestriction(restriction);
    if Not(SpringRestriction.Fixed) then
      SpringRestriction.Paint;
  end;
end;

end.
