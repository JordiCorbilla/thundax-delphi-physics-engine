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
unit tdpe.lib.particle.group;

interface

uses tdpe.lib.particle.abstract.collection, tdpe.lib.vector, Classes, tdpe.lib.particle.pattern.composite,
  tdpe.lib.particle.abstractparticle, tdpe.lib.particle.abstract.restriction, generics.Collections,
  tdpe.lib.force.contract, tdpe.lib.force.list;

Type

  TGroup = Class(TAbstractCollection)
  Private
    FCollideInternal: Boolean;
    FComposites: TList<TComposite>;
    FCollisionList: TList<TGroup>;
    Procedure CheckCollisionGroupInternal;
    Procedure CheckCollisionAgainstGroup(group: TGroup);
  public
    constructor Create(CollideInternal: Boolean = False); Reintroduce; Virtual;
    Destructor Destroy(); Override;
    Procedure Init; Override;
    Procedure AddComposite(composite: TComposite);
    Procedure RemoveComposite(composite: TComposite);
    Procedure Paint; Override;
    Procedure AddCollidable(group: TGroup);
    Procedure RemoveCollidable(group: TGroup);
    Procedure AddCollidableList(list: TList<TGroup>);
    Function GetAll: TList;
    Procedure Integrate(deltaTime: Double; MassLessForce: TForceList; Damping: Double); Override;
    Procedure SatisfyRestrictions; Override;
    Procedure CheckCollision;
    Property CollideInternal: Boolean read FCollideInternal Write FCollideInternal;
    property Composites: TList<TComposite>read FComposites write FComposites;
    property CollisionList: TList<TGroup>read FCollisionList write FCollisionList;
  end;

implementation

uses MaskUtils, SysUtils;

{ TGroup }

procedure TGroup.AddCollidable(group: TGroup);
begin
  FCollisionList.Add(group);
end;

procedure TGroup.AddCollidableList(list: TList<TGroup>);
var
  group: TGroup;
begin
  for group in list do
    FCollisionList.Add(group);
end;

procedure TGroup.AddComposite(composite: TComposite);
begin
  FComposites.Add(composite);
  composite.IsParented := true;
  if IsParented then
    composite.Init;
end;

procedure TGroup.CheckCollision;
var
  group: TGroup;
begin
  if CollideInternal then
    CheckCollisionGroupInternal;

  for group in FCollisionList do
    CheckCollisionAgainstGroup(group);
end;

procedure TGroup.CheckCollisionGroupInternal;
var
  i, j: Integer;
  composite: TComposite;
begin
  CheckInternalCollision;
  for j := 0 to FComposites.Count - 1 do
  begin
    composite := Composites[j];
    composite.CheckCollisionAgainstCollection(Self);
    for i := j + 1 to FComposites.Count - 1 do
      composite.CheckCollisionAgainstCollection(Composites[i]);
  end;
end;

procedure TGroup.CheckCollisionAgainstGroup(group: TGroup);
var
  composite: TComposite;
  gComposite: TComposite;
begin
  CheckCollisionAgainstCollection(group);
  for composite in FComposites do
  begin
    composite.CheckCollisionAgainstCollection(group);
    for gComposite in group.Composites do
      composite.CheckCollisionAgainstCollection(gComposite);
  end;

  for gComposite in group.Composites do
    CheckCollisionAgainstCollection(gComposite);
end;

constructor TGroup.Create(CollideInternal: Boolean);
begin
  inherited Create;
  FCollideInternal := CollideInternal;
  FComposites := TList<TComposite>.Create;
  FCollisionList := TList<TGroup>.Create;
end;

destructor TGroup.Destroy;
begin
  FreeAndNil(FComposites);
  FreeAndNil(FCollisionList);
  inherited;
end;

function TGroup.GetAll: TList;
var
  abstractParticle : TAbstractParticle;
  abstractRestriction : TAbstractRestriction;
  composite : TComposite;
begin
  Result := TList.Create;
  for abstractParticle in Particles do
    Result.Add(abstractParticle);
  for abstractRestriction in Restrictions do
    Result.Add(abstractRestriction);
  for composite in Composites do
    Result.Add(composite);
end;

procedure TGroup.Init;
var
  composite : TComposite;
begin
  For composite in FComposites do
    composite.Init;
end;

procedure TGroup.Integrate(deltaTime: Double; MassLessForce: TForceList; Damping: Double);
var
  composite : TComposite;
begin
  inherited Integrate(deltaTime, MassLessForce, Damping);
  For composite in FComposites do
    composite.Integrate(deltaTime, MassLessForce, Damping);
end;

procedure TGroup.Paint;
var
  abstractParticle : TAbstractParticle;
  abstractRestriction : TAbstractRestriction;
  composite : TComposite;
begin
  for composite in FComposites do
    composite.Paint;
  for abstractParticle in Particles do
    abstractParticle.Paint;
  for abstractRestriction in Restrictions do
    abstractRestriction.Paint;
end;

procedure TGroup.RemoveCollidable(group: TGroup);
begin
  FCollisionList.Remove(group);
end;

procedure TGroup.RemoveComposite(composite: TComposite);
begin
  FComposites.Remove(composite);
  composite.IsParented := False;
  composite.Free;
end;

procedure TGroup.SatisfyRestrictions;
var
  composite : TComposite;
begin
  inherited SatisfyRestrictions;
  for composite in FComposites do
    composite.SatisfyRestrictions;
end;

end.
