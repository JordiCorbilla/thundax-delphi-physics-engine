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
unit tdpe.lib.collision.resolver.solid;

interface

uses
  tdpe.lib.vector, tdpe.lib.particle.box.solid, tdpe.lib.math, tdpe.lib.particle.item.solid, tdpe.lib.collision.resolver.contract;

type
  TSolidCollisionResolver = class(TInterfacedObject, ICollisionResolver<TSolidItem>)
  public
    procedure resolve(particleA, particleB: TSolidItem; hitpoint, normal: ICustomVector<Double>; depth: Double);
  end;

var
  SolidCollisionResolverInstance: ICollisionResolver<TSolidItem>;

implementation

{ TRigidCollisionResolver }

procedure TSolidCollisionResolver.resolve(particleA, particleB: TSolidItem; hitpoint, normal: ICustomVector<Double>; depth: Double);
var
  mtd: IVector;
  te, raxn, rbxn: Double;
  sumInvMass, j, aaa, aab, j1, j2, j3, j4: Double;
  vap, vbp, vabp, vn, l, n, ra, rb, vna, vnb, mtdA, mtdB: IVector;
begin
  mtd := normal.ScalarProduct(depth);
  te := particleA.elasticity + particleB.elasticity;
  sumInvMass := particleA.invMass + particleB.invMass;
  vap := particleA.getVelocityOn(hitpoint);
  vbp := particleB.getVelocityOn(hitpoint);
  vabp := vap.Substract(vbp);
  vn := normal.ScalarProduct(vabp.InnerProduct(normal));
  l := vabp.Substract(vn).Normalise();
  n := normal.Add(l.ScalarProduct(-0.1)).Normalise();
  ra := hitpoint.Substract(particleA.sample);
  rb := hitpoint.Substract(particleB.sample);
  raxn := ra.OuterProduct(n);
  rbxn := rb.OuterProduct(n);

  j2 := 1 + (te / 2);

  j1 := -vabp.InnerProduct(n) * j2;
  j3 := (raxn * (raxn / particleA.mi));

  j4 := (rbxn * (rbxn / particleB.mi));
  j := j1 / (sumInvMass + j3 + j4);
  vna := particleA.velocity.Add(n.ScalarProduct(j * particleA.invMass));
  vnb := particleB.velocity.Add(n.ScalarProduct(-j * particleB.invMass));
  aaa := j * (raxn / particleA.mi);

  aab := -j * (rbxn / particleB.mi);
  particleA.resolveRigidCollision(aaa, particleB);

  particleB.resolveRigidCollision(aab, particleA);
  mtdA := mtd.ScalarProduct(particleA.invMass / sumInvMass);
  mtdB := mtd.ScalarProduct(-particleB.invMass / sumInvMass);
  particleA.resolveCollision(mtdA, vna, normal, depth, -1, particleB);
  particleB.resolveCollision(mtdB, vnb, normal, depth, 1, particleA);
end;

Initialization

SolidCollisionResolverInstance := TSolidCollisionResolver.Create;

Finalization

SolidCollisionResolverInstance := nil;

end.
