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
unit tdpe.lib.engine;

interface

Uses
  tdpe.lib.vector, tdpe.lib.particle.group, Classes, tdpe.lib.force.contract,
  tdpe.lib.force, generics.Collections, tdpe.lib.engine.contract, tdpe.lib.render.contract,
  tdpe.lib.particle.group.list, tdpe.lib.force.list;

Type

  TEngine = Class(TInterfacedObject, IRunnable, IDrawable)
  private
    FTimeSteps: Double;
    FDamping: Double;
    FRestrictionCycles: Integer;
    FRestrictionCollisionCycle: Integer;
    FForces: TForceList;
    FGroups: TGroupList;
    procedure SetRestrictionCollisionCycle(const Value: Integer);
    procedure SetRestrictionCycle(const Value: Integer);
    procedure SetDamping(const Value: Double);
    procedure Integrate;
    procedure SatisfyRestrictions;
    procedure CheckCollisions;
    procedure SetForces(const Value: TForceList);
    procedure SetGroups(const Value: TGroupList);
  public
    procedure AddForce(const vector: IForce);
    procedure AddGroup(group: TGroup);
    procedure RemoveGroup(group: TGroup);
    procedure RemoveForce(vector: IForce);
    procedure ClearForces();
    procedure Run;
    procedure Paint;
    constructor Create(const deltaTime: Double = 0.25);
    destructor Destroy(); override;
    property Groups: TGroupList read FGroups write SetGroups;
    property Forces: TForceList read FForces write SetForces;
    Property RestrictionCycles: Integer read FRestrictionCycles Write SetRestrictionCycle;
    Property RestrictionCollisionCycles: Integer read FRestrictionCollisionCycle Write SetRestrictionCollisionCycle;
    Property Damping: Double read FDamping Write SetDamping;
  end;

implementation

uses
  SysUtils;

{ TEngine }

procedure TEngine.AddForce(const vector: IForce);
begin
  FForces.Add(vector);
end;

procedure TEngine.AddGroup(group: TGroup);
begin
  FGroups.Add(group);
  group.IsPArented := True;
  group.Init;
end;

procedure TEngine.CheckCollisions;
begin
  FGroups.CheckCollisions;
end;

destructor TEngine.Destroy;
begin
  if Assigned(FGroups) then
    FreeAndNil(FGroups);
  if Assigned(FForces) then
    FreeAndNil(FForces);
  inherited;
end;

procedure TEngine.Integrate;
begin
  FGroups.Integrate(FTimeSteps, FForces, Damping);
end;

procedure TEngine.Paint;
begin
  FGroups.Paint;
end;

procedure TEngine.ClearForces;
begin
  if Assigned(FForces) then
    FreeAndNil(FForces);
  FForces := TForceList.Create;
end;

constructor TEngine.Create(const deltaTime: Double);
begin
  FTimeSteps := Sqr(deltaTime);
  FGroups := TGroupList.Create;
  FForces := TForceList.Create;

  FDamping := 1;
  FRestrictionCycles := 0;
  FRestrictionCollisionCycle := 1;
end;

procedure TEngine.RemoveForce(vector: IForce);
begin
  FForces.Remove(vector);
  FreeAndNil(vector);
end;

procedure TEngine.RemoveGroup(group: TGroup);
begin
  FGroups.Remove(group);
  group.IsPArented := false;
  group.free;
end;

procedure TEngine.SatisfyRestrictions;
begin
  FGroups.SatisfyRestrictions;
end;

procedure TEngine.SetRestrictionCollisionCycle(const Value: Integer);
begin
  FRestrictionCollisionCycle := Value;
end;

procedure TEngine.SetRestrictionCycle(const Value: Integer);
begin
  FRestrictionCycles := Value;
end;

procedure TEngine.SetDamping(const Value: Double);
begin
  FDamping := Value;
end;

procedure TEngine.SetForces(const Value: TForceList);
begin
  FForces := Value;
end;

procedure TEngine.SetGroups(const Value: TGroupList);
begin
  FGroups := Value;
end;

procedure TEngine.Run;
var
  i: Integer;
begin
  Integrate;
  For i := 0 to FRestrictionCycles - 1 do
  begin
    SatisfyRestrictions;
  end;

  For i := 0 to FRestrictionCollisionCycle - 1 do
  begin
    SatisfyRestrictions;
    CheckCollisions;
  end;
end;

end.
