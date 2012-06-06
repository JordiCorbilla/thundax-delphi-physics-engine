(*
  *  This file is part of TDPE (Thundax Delphi Physics Engine) an open source
  *  2D physics Engine. Copyright 2011, Jordi Coll Corbilla
  *
  *  TDPE (Thundax Delphi Physics Engine) is free development platform software:
  *  you can redistribute it and/or modify
  *  it under the terms of the GNU Lesser General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  TDPE (Thundax Delphi Physics Engine) is distributed in the hope that it will
  *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU Lesser General Public License for more details.
  *
  *  You should have received a copy of the GNU Lesser General Public License
  *  along with TDPE (Thundax Delphi Physics Engine).  If not,
  *  see <http://www.gnu.org/licenses/>.
  *
  *  Copyright     2010,2011     Jordi Coll Corbilla
  *)
unit tdpe.lib.structures.bridge;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle.solid, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, Graphics, Contnrs, tdpe.lib.writer.contract, Generics.Collections;

Type
  TCustomBridge = class(TGroup)
  Private
    FCircles: TObjectList<TSolidCircle>;
    FSprings: TObjectList<TSpringRestriction>;
    FLog: IWriter;
  Public
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine; x, y: Double); reintroduce; Virtual;
    procedure setLog(const value: IWriter);
    function isInside(x, y: Integer; var pos: Integer): boolean;
    procedure Activate(i: Integer);
    procedure Deactivate();
    procedure Move(x, y: Integer);
    Destructor Destroy(); override;
    procedure DeleteObject();
  End;

implementation

uses
  SysUtils;

{ Bridge }

procedure TCustomBridge.Activate(i: Integer);
begin
  FCircles[i].StartDrag := true;
end;

constructor TCustomBridge.Create(render: TAbstractRenderer; aEngine: TEngine; x, y: Double);
var
  bx: Double;
  By: Double;
  bsize: Double;
  yslope: Double;
  ParticleSize: Double;
  nParticles: Integer;
  innerCircle: TSolidCircle;
  restriction: TSpringRestriction;
  i: Integer;
  fixed: boolean;
begin
  inherited Create(False);
  FCircles := TObjectList<TSolidCircle>.Create();
  FSprings := TObjectList<TSpringRestriction>.Create();
  bx := x;
  By := y;
  bsize := 10;
  yslope := 0;
  ParticleSize := 4;
  nParticles := 30;

  // Create the particles
  for i := 0 to nParticles - 1 do
  begin
    fixed := (i = 0) or (i = (nParticles - 1));
    innerCircle := TSolidCircle.Create(bx, By, ParticleSize, fixed);
    innerCircle.setLog(FLog);
    addParticle(innerCircle);
    innerCircle.SetRenderer(render);
    FCircles.Add(innerCircle);
    bx := bx + bsize;
    By := By + yslope;
  end;

  // Create the Springs
  for i := 0 to nParticles - 2 do
  begin
    restriction := TSpringRestriction.Create(FCircles.Items[i], FCircles.Items[i + 1], 0.9, true, 2, 0.8, true, clWhite, 1);
    if (i = 0) or (i = (nParticles - 1)) then
      restriction.FixedEndLimit := 0.25;
    addRestriction(restriction);
    restriction.setLog(FLog);
    restriction.SetRenderer(render);
    FSprings.Add(restriction);
  end;
end;

procedure TCustomBridge.Deactivate;
var
  circle: TSolidCircle;
begin
  for circle in FCircles do
    circle.StartDrag := False;
end;

procedure TCustomBridge.DeleteObject;
begin
  // Testing purposes. This will break the bridge in two halves
  if Assigned(FSprings[10]) then
  begin
    RemoveRestriction(FSprings[10]);
    FSprings[10].Free;
  end;
end;

destructor TCustomBridge.Destroy;
begin
  FreeAndNil(FCircles);
  FreeAndNil(FSprings);
  inherited;
end;

function TCustomBridge.isInside(x, y: Integer; var pos: Integer): boolean;
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
    Inc(i);
  end;
  result := bFound;
end;

procedure TCustomBridge.Move(x, y: Integer);
var
  circle: TSolidCircle;
begin
  for circle in FCircles do
  begin
    if circle.StartDrag then
      circle.Move(x, y);
  end;
end;

procedure TCustomBridge.setLog(const value: IWriter);
begin
  FLog := value;
end;

end.
