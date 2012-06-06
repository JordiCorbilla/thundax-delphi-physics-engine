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
unit tdpe.lib.collision.resolver;

Interface

Uses
  tdpe.lib.particle.abstractparticle, tdpe.lib.vector, tdpe.lib.collision, tdpe.lib.math.helper,
  tdpe.lib.collision.resolver.contract;

Type
  CollisionResolver = Class(TInterfacedObject, ICollisionResolver<TAbstractParticle>)
  Public
    procedure resolve(particleA, particleB: TAbstractParticle; hitpoint, normal: ICustomVector<Double>; depth: Double);
  end;

var
  CollisionResolverInstance: ICollisionResolver<TAbstractParticle>;

implementation

uses
  SysUtils, tdpe.lib.collision.contract;

{ CollisionResolver }

procedure CollisionResolver.resolve(particleA, particleB: TAbstractParticle; hitpoint, normal: ICustomVector<Double>; depth: Double);
var
  mtd, vectorNormalA, vectorNormalB, mtda, mtdb, CoefficientA, CoefficientB: IVector;
  CoefficientNormalA, CoefficientNormalB, RestitutionA, RestitutionB: IVector;
  totalElasticity, TotalInvMass, totalFriction: Double;
  collisionComponentsA, collisionComponentsB: ICollision;
begin
  particleA.Current.Clone(particleA.Sample);
  particleB.Current.Clone(particleB.Sample);

  mtd := normal.ScalarProduct(depth);
  totalElasticity := particleA.Elasticity + particleB.Elasticity;
  TotalInvMass := particleA.InvMass + particleB.InvMass;

  totalFriction := TMathUtil.Clamp(1 - (particleA.Friction + particleB.Friction), 0, 1);

  collisionComponentsA := particleA.GetComponents(normal);
  collisionComponentsB := particleB.GetComponents(normal);

  CoefficientA := collisionComponentsB.normal.ScalarProduct((totalElasticity + 1) * particleA.InvMass);
  CoefficientB := collisionComponentsA.normal.ScalarProduct((totalElasticity + 1) * particleB.InvMass);
  CoefficientNormalA := collisionComponentsA.normal.ScalarProduct(particleB.InvMass - totalElasticity * particleA.InvMass);
  CoefficientNormalB := collisionComponentsB.normal.ScalarProduct(particleA.InvMass - totalElasticity * particleB.InvMass);
  RestitutionA := CoefficientA.Add(CoefficientNormalA);
  RestitutionB := CoefficientB.Add(CoefficientNormalB);
  vectorNormalA := RestitutionA.Divide(TotalInvMass);
  vectorNormalB := RestitutionB.Divide(TotalInvMass);

  collisionComponentsA.velocity.ScalarProduct(totalFriction, True);
  collisionComponentsB.velocity.ScalarProduct(totalFriction, True);

  mtda := mtd.ScalarProduct(particleA.InvMass / TotalInvMass);
  mtdb := mtd.ScalarProduct(-particleB.InvMass / TotalInvMass);

  vectorNormalA.Add(collisionComponentsA.velocity, True);
  vectorNormalB.Add(collisionComponentsB.velocity, True);

  if not particleA.Fixed then
    particleA.ResolveCollision(mtda, vectorNormalA, normal, depth, -1, particleB);

  if not particleB.Fixed then
    particleB.ResolveCollision(mtdb, vectorNormalB, normal, depth, 1, particleA);
end;

Initialization

CollisionResolverInstance := CollisionResolver.Create;

Finalization

CollisionResolverInstance := nil;

end.
